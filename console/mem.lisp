(in-package :s79-console)

(scheme-79:scheme-79-version-reporter "Scheme Machine Memory Win" 0 4 0
                                      "Time-stamp: <2022-03-18 15:16:17 gorbag>"
                                      "add options checkboxes for dump-memory contents")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.4.0   3/18/22 snapping a line: 0.4 release of scheme-79 supports test-0 thru test-3. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.3.5   3/15/22 add options for the dump memory, preselecting show-registers.
;;                     the hope is to make it easier to follow the display stack, 
;;                     the control stack, etc.

;; 0.3.4   3/ 4/22 make editor-pane temporary (will be GC'd when interface destroyed)

;; 0.3.3   3/ 2/22 rename module to fit version-reporter limit

;; 0.3.2   2/23/22 convert main panel to capi:editor-panel. 
;;                 add new status panel to right of update button to show last update tick
;;                     (not auto-updating at this point so we can capture history; if we 
;;                     click "Memory" button on the console we get a new window so we can 
;;                     do a comparison to the prior one!)

;; 0.3.1   2/22/22 use capi:redisplay-element instead of gp:invalidate-rectangle (new in LW 8.0)
;;                 Add update button

;; 0.3.0   2/18/22 New window for showing the contents of memory 
;;                    (essentially a floating, updating, dump-memory)

(defparameter *memory-window-initial-width* 1200)

(defparameter *memory-window-initial-height* 1400)

(defvar *mem* nil)

(defun reset-mem ()
  (when (mem-running-p)
    (redraw-mem *mem*)))

(defun mem-running-p ()
  *mem*) ; probably should check that it's exposed...

(defun start-mem ()
  (let ((interface (make-instance 'mem-interface
                                  :best-width *memory-window-initial-width*
                                  :best-height *memory-window-initial-height*)))
    (setq *mem* interface)
    (capi:display interface)))

(defun redraw-mem (interface)
  (capi:redisplay-element (slot-value interface 'content-viewer))
  ;(capi:redisplay-element (slot-value interface 'status))
  )

(defun update-contents (interface new-text)
  (let ((status-pane (slot-value interface 'status)))
    (setf (memory-contents-text interface) new-text)
    (setf (last-update-tick interface) *tick*)
    (capi:apply-in-pane-process status-pane
                                #'(setf capi:display-pane-text)
                                (format nil "Last Updated: Tick ~d" *tick*)
                                status-pane)
    (capi:modify-editor-pane-buffer
     (slot-value interface 'content-viewer) :contents new-text)
    (redraw-mem interface)
  ))

(defparameter *mem-dump-options*
  '(:decode-p :show-registers :show-stack-membership :show-display-membership))

(defun mem-update (interface)
  ;; get a memory dump. We should be able to select parameters (TBD),
  ;; but for now just do a generic dump.  note our intent is to
  ;; annotate the memory dump, e.g., with where the registers are
  ;; pointing. We can either do this through a separte column, or
  ;; integrating something into the memory-dump generator
  ;; (memory-dump-internal). (TBD)

  ;; for now, just do a straight dump so we have something we can look
  ;; at.
  (let ((*error-output* (make-string-output-stream)) ; memory dumps are to *error-output*
        (options (capi:choice-selected-items (slot-value interface 'options))))
    (let ((option-args (mapcan #'(lambda (option)
                                   (if (member option options)
                                     (list option t)
                                     (list option nil)))
                               *mem-dump-options*)))
      (apply #'dump-memory 0 option-args))
    (update-contents interface (get-output-stream-string *error-output*))))

(capi:define-interface mem-interface ()
  ((memory-contents-text :initform ""
                         :accessor memory-contents-text)
   (last-update-tick :initform 0 
                     :accessor last-update-tick))
  (:panes
   (content-viewer
    capi:editor-pane ; use editor-pane instead of display pane to preserve spacing (may be a font issue?)
    :buffer :temp ; gc when we destroy the interface
    :enabled nil
    :vertical-scroll t
    :text memory-contents-text
    :font (gp:make-font-description
           :familly "Menlo"
           :size 13
           :weight :normal
           :slant :roman
           :pitch :fixed)
    )
   (push-buttons
    capi:push-button-panel
    :items '(:update)
    :print-function 'capitalize-if-symbol
    :callback-type :data-interface
    :selection-callback 'mem-button-selection-callback
    :layout-args '(:x-uniform-size-p t))
   (options
    capi:check-button-panel
    :title "Options"
    :items *mem-dump-options*
    :selected-items '(:decode-p :show-registers)
    :layout-class 'capi:row-layout)
   (status
    capi:display-pane
    :enabled nil
    :font (gp:make-font-description
           :familly "Menlo"
           :size 13
           :weight :normal
           :slant :roman
           :pitch :fixed)
    :text (format nil "Last Updated: Tick ~d" last-update-tick))
   )

  (:layouts
   (status-panel 
    capi:row-layout
    '(push-buttons options status))
   (default
    capi:column-layout
    '(status-panel content-viewer)))

  (:default-initargs 
   :layout 'default
   :title "Memory Viewer")
  )

(defmethod capi:interface-display :after ((interface mem-interface))
  (mem-update interface))

(defun mem-button-selection-callback (data interface)
  (ecase data
    (:update
     (mem-update interface))))
     
