(in-package :s79-console)

(scheme-79:scheme-79-version-reporter "Scheme Machine DSO" 0 3 1
                                      "Time-stamp: <2022-01-13 14:24:22 gorbag>"
                                      "internal-freeze -> run-nano")

;; 0.3.1   1/13/22 change references from internal-freeze to run-nano
;;                    for consistancy with AIM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.5   8-16-21  reorder bus lines in graph

;; 0.0.4   8-12-21  split out dso-update-starting-tick

;; 0.0.3   7-15-21  reset-dso should only force redraw when dso is running... :-)

;; 0.0.2   7-10-21  reset-dso should force a redraw (to clear)
;;                     fix record-value bug (should call pad-value)

;; 0.0.1   6-25-21  reset-dso

;; 0.0.0   6-21-21  New (tested)

(require "graphic-tools")

;; extend our console support with a DSO (digital storage
;; oscilloscope) like feature.  the idea is to create a tool similar
;; to a DSO (also provided typically for debugging simulated digital
;; electronic circuits), where we can look at the change in the values
;; of particular pads and internal digital busses over time.

;; because our clock is simulated, we capture the state of the pads,
;; busses or both at each change of the clock phase - i.e. at the end
;; of the phase (or before we do anything else at the start of the
;; next one), we record (the storage part of the DSO) the state for
;; later display.

;; because we don't want the overhead of recording things when we
;; aren't going to look, we only start recording after the screen has
;; been activated.

(defparameter *dso-screen-initial-width* 1600)

(defparameter *dso-screen-initial-height* 1000)

(defvar *dso* nil)

(defvar *dso-pad-alist* '())

(defvar *dso-bus-alist* '())

(defparameter *ticks-to-display* 6
  "Number of ticks to display at a time, defines the total width of the graph")

(defparameter *initial-graph-width* (* *ticks-to-display* *total-clock-phases*)
  "number of transitions to draw")

(defparameter *graph-margin-left* 120)

(defparameter *graph-margin-right* 30)

(defparameter *graph-margin-bottom* 30)

(defparameter *graph-top-ratio* 0.94)

(defparameter *graph-right-ratio* 0.95) 

(defparameter *total-ticks* 100 ;; also temporary, should come from current *tick* of underlying simulation
  "Final clock tick shown in the graph, should be updated every clock cycle being recorded")

(defun reset-dso ()
  "Typically called when we reset *tick* to 0 to clear out anything we've recorded"
  (setq *dso-pad-alist* nil)
  (setq *dso-bus-alist* nil)
  (when (dso-running-p)
    (dso-redraw *dso*)))

(defun dso-running-p ()
  (or (record-pads-p)
      (record-busses-p)))

(defun record-pads-p ()
  "returns non-nil if we should record pads"
  ;; for now, just activate if we've ever called start-dso
  (not (null *dso*))
  )

(defun record-busses-p ()
  "returns non-nil if we should record busses"
  ;; for now, just activate if we've ever called start-dso
  (not (null *dso*))
  )

(defun pad-value (pad-name)
  (bit (symbol-value pad-name) 0))

(defun record-pad (pad-name state)
  "add a record for the pad state at the current clock. State is typically the micro-tick"

  (when (record-pads-p)
    (let ((entry (assoc *tick* *dso-pad-alist*)))
      (cond
        (entry
         (update-alist-alist state pad-name (pad-value pad-name) (cdr entry)))
        (t
         (setq *dso-pad-alist*
               (acons *tick*
                      (acons state
                             (acons pad-name
                                    (pad-value pad-name)
                                    nil)
                             nil)
                      *dso-pad-alist*)))))))

(defun historic-pad-value (pad-name major-tick minor-tick)
  (or (let*-non-null ((tick-entry (assoc major-tick *dso-pad-alist*))
                      (minor-tick-entry (assoc (nth minor-tick *phase-names*) (cdr tick-entry)))
                      (raw-pad-value (cdr (assoc pad-name (cdr minor-tick-entry)))))
        (if (bit-vector-p raw-pad-value)
          (bit-vector->integer raw-pad-value)
          raw-pad-value))
      0)) ; so we always return a number

(defun record-bus (bus-name state)
  "add a record for the bus state at the current clock. State is
typically a bit vector. Note that some \"busses\" are represented as
registers."
  (when (record-busses-p)
    (let ((entry (assoc *tick* *dso-bus-alist*)))
      (cond
        (entry
         (update-alist-alist state bus-name (bit-vector->integer (symbol-value bus-name)) (cdr entry)))
        (t
         (setq *dso-bus-alist*
               (acons *tick*
                      (acons state
                             (acons bus-name
                                    (bit-vector->integer (symbol-value bus-name))
                                    nil)
                             nil)
                      *dso-bus-alist*)))))))

(defun historic-bus-value (bus-name major-tick minor-tick)
  (or (let*-non-null ((tick-entry (assoc major-tick *dso-bus-alist*))
                      (minor-tick-entry (assoc (nth minor-tick *phase-names*) (cdr tick-entry)))
                      (raw-bus-value (cdr (assoc bus-name (cdr minor-tick-entry)))))
        (if (bit-vector-p raw-bus-value)
          (bit-vector->integer raw-bus-value)
          raw-bus-value))
      0))

(defun historic-bus-changed-p (bus-name major-tick minor-tick)
  "return true if the bus just changed values in the last minor tick"
  (let ((current-bus-value (historic-bus-value bus-name major-tick minor-tick))
        (previous-minor (if (zerop minor-tick) 
                          7
                          (1- minor-tick)))
        (previous-major (if (zerop minor-tick)
                          (1- major-tick) 
                          major-tick)))
    (if (minusp previous-major) ; first clock tick
      t
      (not (eql current-bus-value (historic-bus-value bus-name previous-major previous-minor))))))

(defun do-record-pads ()
  (mapc #'(lambda (pad-name)
            (record-pad pad-name *symbolic-clock-phase*))
        *all-pad-names*)

  (setq *total-ticks* (max *total-ticks* *tick*)) ; update if needed

  ;; if the last tick was displayed, but the next tick would be offscreen, update the tick start
  ;; (this keeps the scroll if something more interesting is focused on)
  (when (dso-running-p)
    (let ((current-start (dso-starting-tick *dso*))
          (display-width-in-ticks *ticks-to-display*))
      (when (= (- *tick* current-start display-width-in-ticks) 1)
        (dso-update-starting-tick (slot-value *dso* 'tick-option-pane) (+ current-start 2) *dso*)))))

(add-initialization "DSO record pads"
                    '(do-record-pads)
                    nil
                    '*all-ph-pre-update-list*)

(defun do-record-busses ()
  (mapc #'(lambda (bus-name)
            (record-bus bus-name *symbolic-clock-phase*))
        *all-bus-names*))

(add-initialization "DSO record busses"
                    '(do-record-busses)
                    nil
                    '*all-ph-pre-update-list*)

(add-initialization "DSO update tick"
                    '(when *dso* (update-tick-pane (slot-value *dso* 'tick-pane) *dso*))
                    nil
                    '*ph1-rising-list*)

;; in order to present the pads in a more useful order, we change the order of *all-pad-names* and use the new order.
(defparameter *all-pad-names-presentation-order*
  (union ; union with *all-pad-names* for any we left out. They will appear on the bottom (head of this list)
   ;; this is in the order of presentation - bottom to top
   '(*reset* *ph2* *ph1* *run-nano* *cdr* *write* *read* *ale* 
     *freeze* *read-interrupt* *interrupt-request* *gc-needed*)
   *all-pad-names*))

;; turn the pad values into a graph
(defun make-pad-graph-spec (pad color)
  (lw-gt:make-basic-graph-spec 'pad-value-at-time 0 1 *initial-graph-width*
                               :var1 pad
                               :var2 (position pad *all-pad-names-presentation-order*)
                               :var3 0 ;tick offset
                               :color color
                               :thickness 3 ; want it to be larger than the major axis
                               :name (string pad)))

(defun make-bus-graph-spec (bus color)
  (lw-gt:make-basic-graph-spec 'bus-changed-at-time-p 0 1 *initial-graph-width*
                               :var1 bus
                               :var2 (position bus *all-bus-names*)
                               :var3 0 ;tick offset
                               :color color
                               :thickness 3
                               :name (string bus)))

(defparameter *pad-color-list*
  '(:red :blue :green :purple :orange
    :red :blue :green :purple :orange
    :red :blue :green :purple :orange
    :red :blue :green :purple :orange
    :khaki :yellow :turquoise :thistle :slateblue 
    :royalblue :orchid :maroon :plum :mistyrose 
    :medium-green :medium-brown :mediumaquamarine :pink)
  "List of colors to use for each pad")
  
(defparameter *pad-graph-specs*
  (mapcar #'make-pad-graph-spec *all-pad-names-presentation-order* *pad-color-list*))

(defparameter *bus-graph-specs*
  (mapcar #'make-bus-graph-spec *all-bus-names* *pad-color-list*))

(defparameter *signal-active-offset* 0.5)

;; compute the y value for a given x (minor tick offset from the major start-tick)

(defun pad-value-at-time (graph-spec minor-tick)
  (let* ((pad-symbol (lw-gt:basic-graph-spec-var1 graph-spec))
         (pad-position (lw-gt:basic-graph-spec-var2 graph-spec))
         (start-major-tick (lw-gt:basic-graph-spec-var3 graph-spec))
         ;(start-major-tick (floor (/ (lw-gt:basic-graph-spec-start-x graph-spec) *total-clock-phases*)))
         (current-major-tick ;;(floor (/ minor-tick *total-clock-phases*)))
                             (+ start-major-tick (floor (/ minor-tick *total-clock-phases*))))
         (current-minor-tick (mod minor-tick *total-clock-phases*)))

    ;; a 0 value should return the pad-position in the graph, while a 1 value should return the pad-position + .5

    (+ pad-position
       (* (historic-pad-value pad-symbol current-major-tick current-minor-tick) *signal-active-offset*))))

(defun bus-changed-at-time-p (graph-spec minor-tick)
  (let* ((bus-symbol (lw-gt:basic-graph-spec-var1 graph-spec))
         (bus-position (lw-gt:basic-graph-spec-var2 graph-spec))
         (start-major-tick (lw-gt:basic-graph-spec-var3 graph-spec))
         ;(start-major-tick (floor (/ (lw-gt:basic-graph-spec-start-x graph-spec) *total-clock-phases*)))
         (current-major-tick ;;(floor (/ minor-tick *total-clock-phases*)))
                             (+ start-major-tick (floor (/ minor-tick *total-clock-phases*))))
         (current-minor-tick (mod minor-tick *total-clock-phases*)))

    (+ bus-position
       (if (historic-bus-changed-p bus-symbol current-major-tick current-minor-tick) 
         *signal-active-offset*
         0))))

(defun start-dso (&key (major-color :darkgreen) (minor-color :lightblue))
  "start up the screen for the optional DSO and record the pad
  values (and optionally bus values) going forward"
  (mlet (pad-graph pad-graph-objects pad-horizontal-labels bus-graph bus-graph-objects bus-horizontal-labels)
            (dso-generate-all *pad-graph-specs* *bus-graph-specs* major-color minor-color)
    (let ((interface (make-instance 'dso-interface
                                    :pad-graph pad-graph
                                    :pad-graph-objects pad-graph-objects
                                    :pad-horizontal-labels pad-horizontal-labels
                                    :bus-graph bus-graph
                                    :bus-graph-objects bus-graph-objects
                                    :bus-horizontal-labels bus-horizontal-labels
                                    :best-width *dso-screen-initial-width*
                                    :best-height *dso-screen-initial-height*)))
      (setq *dso* interface)
      (capi:display interface))))
                                    
(capi:define-interface dso-interface ()
  ((pad-graph-objects :reader dso-pad-graph-objects :initarg :pad-graph-objects)
   (bus-graph-objects :reader dso-bus-graph-objects :initarg :bus-graph-objects)
   (pad-graph :reader dso-pad-graph :initarg :pad-graph)
   (bus-graph :reader dso-bus-graph :initarg :bus-graph)
   (pad-horizontal-labels :reader dso-pad-horizontal-labels :initarg :pad-horizontal-labels)
   (bus-horizontal-labels :reader dso-bus-horizontal-labels :initarg :bus-horizontal-labels)
   (starting-tick :accessor dso-starting-tick :initarg :starting-tick :initform *tick*)
   (current-zoom :accessor dso-current-zoom :initarg :current-zoom :initform 100)
   (current-minor-ticks :accessor dso-current-minor-ticks :initarg :current-minor-ticks :initform *initial-graph-width*))

  (:panes
   (pad-pane
    lw-gt:objects-displayer
    :reader pad-pane-displayer
    :drawing-object pad-graph
    )
   (bus-pane                                                            
    lw-gt:objects-displayer
    :reader bus-pane-displayer                                          
    :drawing-object bus-graph
    )

   (tick-option-pane
    capi:slider
    :start-point :left
    :start 0
    :end *total-ticks* ; for now
    :slug-start starting-tick
    :callback #'(lambda (pane data action)
                  (declare (ignorable pane action))
                  (setf starting-tick data)
                  ;; update the specs
                  (dso-update-starting-tick pane data capi:interface)

                  (dso-update-and-redraw capi:interface))) ; want to update the graph and labels

   (tick-pane
    capi:display-pane
    :text (format nil "Tick: ~D Current-Tick: ~D" starting-tick *tick*))

   #||
   (zoom-option-pane
    capi:slider
    :start-point :right
    :start 1
    :end 100
    :slug-start current-zoom
    :callback #'(lambda (pane data action)
                  (declare (ignorable pane action))
                  (setf current-zoom data)
                  (apply-in-pane-process zoom-pane #'(setf capi:display-pane-text)
                                         (list (format nil "Zoom ~D%" current-zoom))
                                         zoom-pane)
                  ;; for now just set the global, need to move this to the interface
                  (setq current-minor-ticks (floor (* (/ current-zoom 100) *initial-graph-width*)))
                  ;; update the specs
                  (let ((scale (/ current-zoom 100)))
                    (mapc #'(lambda (pgo)
                              (setf (lw-gt:basic-graph-spec-x-scale (lw-gt:compound-drawing-object-data pgo)) scale)
                              (setf (lw-gt:basic-graph-spec-range (lw-gt:compound-drawing-object-data pgo)) *initial-graph-width*))
                          pad-graph-objects)
                    (mapc #'(lambda (bgo)
                              (setf (lw-gt:basic-graph-spec-x-scale (lw-gt:compound-drawing-object-data bgo)) scale)
                              (setf (lw-gt:basic-graph-spec-range (lw-gt:compound-drawing-object-data bgo)) *initial-graph-width*))
                          bus-graph-objects))
                  
                  (dso-update-and-redraw capi:interface)))

   (zoom-pane
    capi:display-pane
    :text (format nil "Zoom: ~D%" current-zoom))
  ||#
   )
  (:layouts
   (default
    capi:column-layout
    '(dso-tab-layout info-row1-layout #|| info-row2-layout ||#))

   (info-row1-layout
    capi:row-layout
    '(tick-option-pane tick-pane))

   #||
   (info-row2-layout
    capi:row-layout
    '(zoom-option-pane zoom-pane))
   ||#

   (dso-tab-layout
    capi:tab-layout
    ()
    :items'(("Pads" pad-pane-layout) 
            ("Busses" bus-pane-layout))
    :print-function 'car
    :visible-child-function 'cadr)
   
   (pad-pane-layout
    capi:column-layout
    '(pad-pane))

   (bus-pane-layout
    capi:column-layout
    '(bus-pane)))

  (:default-initargs
   :layout 'default
   :title "Digital Storage Oscilloscope"))

(defun dso-update-starting-tick (pane new-starting-tick interface)
  (setf (dso-starting-tick interface) new-starting-tick)
  
  (apply-in-pane-process pane
                         #'(setf capi:range-end)
                         *total-ticks*
                         pane)
  (apply-in-pane-process pane
                         #'(setf capi:range-slug-start)
                         new-starting-tick
                         pane)
  
  (mapc #'(lambda (pgo)
            (setf (lw-gt:basic-graph-spec-var3 (lw-gt:compound-drawing-object-data pgo)) new-starting-tick))
        (dso-pad-graph-objects interface))
  (mapc #'(lambda (bgo)
            (setf (lw-gt:basic-graph-spec-var3 (lw-gt:compound-drawing-object-data bgo)) new-starting-tick))
        (dso-bus-graph-objects interface))

  ;; UPDATE graph objects
  (update-tick-pane (slot-value interface 'tick-pane) interface))

(defun update-tick-pane (pane interface)
  (apply-in-pane-process pane
                         #'(setf capi:display-pane-text)
                         (format nil "Tick: ~D Current-Tick: ~D" (dso-starting-tick interface) *tick*)
                         pane)
  (dso-update-and-redraw interface))

;; generate the graph drawings

;; fixed drawings are the grid and labels - modified from graph-example.lisp

(defun dso-generate-fixed-drawings-pads (major-color minor-color)
  (let* ((graph-height (length *all-pad-names*))
         (grid-lines (lw-gt:generate-grid-lines
                      :horizontal-count *initial-graph-width*
                      :vertical-count graph-height
                      :right-thickness 3
                      :major-x-step *total-clock-phases*
                      :major-y-step 1
                      :thickness 1
                      :major-thickness 2
                      :major-color major-color
                      :color minor-color))
         (vertical-labels (lw-gt:generate-labels
                           nil 0 1 graph-height
                           :color :blue
                           :print-function #'(lambda (n)
                                               (format nil "~A"
                                                       (or (nth n
                                                                *all-pad-names-presentation-order*)
                                                           ""))))))
    (list grid-lines vertical-labels)))

(defun dso-generate-fixed-drawings-busses (major-color minor-color)
  (let* ((graph-height (length *all-bus-names*))
         (grid-lines (lw-gt:generate-grid-lines
                      :horizontal-count *initial-graph-width*
                      :vertical-count graph-height
                      :right-thickness 3
                      :major-x-step *total-clock-phases*
                      :major-y-step 16 ; temp
                      :thickness 1
                      :major-thickness 2
                      :major-color major-color
                      :color minor-color))
         (vertical-labels (lw-gt:generate-labels
                           nil 0 1 graph-height
                           :color :blue
                           :print-function #'(lambda (n)
                                               (format nil "~A"
                                                       (or (nth n
                                                                *all-bus-names*)
                                                           ""))))))
    (list grid-lines vertical-labels)))

(defun dso-generate-pad-labels (starting-tick)
  (lw-gt:generate-labels t starting-tick *total-clock-phases* *initial-graph-width*
   :color :red
   :print-function #'(lambda (n)
                       (format nil "~D" (floor n *total-clock-phases*)))))

(defun dso-generate-bus-labels (starting-tick)
  (dso-generate-pad-labels starting-tick)) ; in case they are different in the future

(defun dso-generate-all (pad-specs bus-specs major-color minor-color)
  (let* ((fixed-pads (dso-generate-fixed-drawings-pads major-color minor-color))
         (pad-horizontal-labels (dso-generate-pad-labels 0))
         (fixed-busses (dso-generate-fixed-drawings-busses major-color minor-color))
         ;; in case we make them different in the future
         (bus-horizontal-labels (dso-generate-bus-labels 0))

         (pad-graph-objects (mapcar #'(lambda (x)
                                        (lw-gt:position-object nil :data (copy-structure x)
                                                                   :function 'lw-gt:generate-graph-from-graph-spec))
                                    pad-specs))
         (bus-graph-objects (mapcar #'(lambda (x)
                                        (lw-gt:position-object nil :data (copy-structure x)
                                                                   :function 'lw-gt:generate-graph-from-graph-spec))
                                    bus-specs))
         (pad-graph (lw-gt:position-and-fit-object
                     (list fixed-pads pad-horizontal-labels pad-graph-objects)
                     *initial-graph-width*
                     (length *all-pad-names*)
                     :left-margin *graph-margin-left*
                     :right-margin *graph-margin-right*
                     :bottom-margin *graph-margin-bottom*
                     :top-ratio *graph-top-ratio*
                     :right-ratio *graph-right-ratio*))
         (bus-graph (lw-gt:position-and-fit-object
                     (list fixed-busses bus-horizontal-labels bus-graph-objects)
                     *initial-graph-width*
                     (length *all-bus-names*)
                     :left-margin *graph-margin-left*
                     :right-margin *graph-margin-right*
                     :bottom-margin *graph-margin-bottom*
                     :top-ratio *graph-top-ratio*
                     :right-ratio *graph-right-ratio*)))
    (values pad-graph pad-graph-objects pad-horizontal-labels bus-graph bus-graph-objects bus-horizontal-labels)))

(defun dso-update (interface) 
  (dso-update-pad-graph interface)
  (dso-update-bus-graph interface))

(defun dso-update-pad-graph (interface)
  (let ((new-pad-labels (dso-generate-pad-labels (* (dso-starting-tick interface) *total-clock-phases*))))
    (mapc #'(lambda (phl nl)
              (setf (lw-gt:compound-drawing-object-sub-object phl)
                    (lw-gt:compound-drawing-object-sub-object nl)))
          (dso-pad-horizontal-labels interface)
          new-pad-labels))
  (mapc #'(lambda (pgo)
            (setf (lw-gt:compound-drawing-object-sub-object pgo)
                  (lw-gt:generate-graph-from-graph-spec (lw-gt:compound-drawing-object-data pgo))))
        (dso-pad-graph-objects interface)))

(defun dso-update-bus-graph (interface)
  (let ((new-bus-labels (dso-generate-bus-labels (* (dso-starting-tick interface) *total-clock-phases*))))
    (mapc #'(lambda (bhl nl)
              (setf (lw-gt:compound-drawing-object-sub-object bhl)
                    (lw-gt:compound-drawing-object-sub-object nl)))
          (dso-bus-horizontal-labels interface)
          new-bus-labels))
  (mapc #'(lambda (pgo)
            (setf (lw-gt:compound-drawing-object-sub-object pgo)
                  (lw-gt:generate-graph-from-graph-spec (lw-gt:compound-drawing-object-data pgo)))) 
        (dso-bus-graph-objects interface)))

(defun dso-redraw (interface)
  (lw-gt:force-objects-redraw (pad-pane-displayer interface))
  (lw-gt:force-objects-redraw (bus-pane-displayer interface)))

(defun dso-update-and-redraw (&optional (interface *dso*))
  (dso-update interface)
  (dso-redraw interface))

(defmethod capi:interface-display :after ((interface dso-interface))
  (dso-update interface))

#||
;; setup quick test (run after (start-dso))

(defun test-dso (&optional (ticks 20))
  (let ((minor-tick-count 0))
    
    ;; temporary
    (setq *total-ticks* (* ticks *total-clock-phases*)) ; so 160 if default

    ;; initialize all pads to 0
    (mapc #'(lambda (pad-name)
              (setf (bit (symbol-value pad-name) 0) 0))
          *all-pad-names*)
    
    (dotimes (*tick* ticks)
      (dotimes (phase *total-clock-phases*)
        (let ((pad-number 0))
          (mapc #'(lambda (pad-name)                                    
                    ;; hack in a value for the pad
                    (if (or (zerop pad-number)
                            (zerop (mod minor-tick-count pad-number)))
                        (setf (bit (symbol-value pad-name) 0)
                              (mod (1+ (bit (symbol-value pad-name) 0)) 2)))
                    (record-pad pad-name (nth phase *phase-names*))
                    (incf pad-number))
                *all-pad-names*))
        (incf minor-tick-count))))
  (dso-update-and-redraw *dso*))
||#
