(in-package :s79-console)

(scheme-79:scheme-79-version-reporter "Scheme Machine Memory Win" 0 4 1
                                      "Time-stamp: <2022-04-07 12:46:16 gorbag>"
                                      "repatriating")

;; 0.4.1   4/ 7/22 moving dump-memory support to console/mem.lisp

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

(defparameter *format-car-for-dm-with-types* "M=~o P=~o T=~A ~48tD~4,'0o F~4,'0o A~8,'0o~58t . ")

(defparameter *format-cdr-for-dm-with-types* "M=~o P=~o T=~A ~110tD~4,'0o F~4,'0o A~8,'0o")

(defparameter *format-car-for-dm-decode-p* "~A~A ~40t~8,'0o~49t . ")

(defparameter *format-cdr-for-dm-decode-p* "~A~A ~80t~8,'0o ~A")


(defun clear-memory ()
  "used for debugging"
  (dotimes (i scheme-mach:*maximum-memory-size*)
    (setf (elt *memory-vector* i) 0)))

(defun dump-memory-internal (start end print-fn row-leader-print-fn)
  (let ((row-count 0))
    (do ((i (* 2 start) (1+ i))) ; array offset
        ((> i (1+ (* 2 (min scheme-mach:*maximum-memory-size* end)))))
      (when (= row-count 0)
        (funcall row-leader-print-fn i))
      (funcall print-fn i)
      (incf row-count)
      (when (>= row-count *dump-values-per-row*)
        (terpri *error-output*)
        (setq row-count 0)))))

(let ((all-regs nil))
  (defun dump-memory (&optional (start 0) &key (end (bit-vector->integer microlisp-shared:*memtop*)) 
                                (decode-p t) (show-registers nil)
                                show-stack-membership
                                show-display-membership); not implemented yet
    "used for debugging. decode-p prints the mark bit, and type for
each memory address in range; field specific data should use
\(dump-memory-with-types)"
    (let ((reg-address-alist nil)) ; clear each time so we get LAG

      (unless all-regs
        ;; we defer this until **machine-registers** is available
        (setq all-regs (mapcan #'(lambda (x) (if (endp (cdr x)) (list (car x)))) **machine-registers**)))

      (when show-registers
        (mapc #'(lambda (reg-name)
                  (let* ((reg-value (bit-vector->integer (get-address-bits (symbol-value reg-name))))
                         (existing-entry (cdr (assoc reg-value reg-address-alist))))
                    (update-alist reg-value (list* reg-name existing-entry) reg-address-alist)))
              all-regs))

      (when show-stack-membership
        ;; use the reg-address-alist to add indicators that the current address is on the stack
        (let* ((stack-ptr (logand *address-field-mask* (bit-vector->integer *stack*)))
               (existing-entry (cdr (assoc stack-ptr reg-address-alist))))
          (while (plusp stack-ptr)
            (update-alist stack-ptr (list* :s existing-entry) reg-address-alist)
            (setq stack-ptr (logand *address-field-mask* (read-address stack-ptr t t)))
            (setq existing-entry (cdr (assoc stack-ptr reg-address-alist))))))

      (when show-display-membership
        nil) ;; (TBD) .. tricky because it's saved on the stack so we have to know which elements are the display

      (dump-memory-internal
       start end
       (cl:if decode-p
         #'(lambda (i) (mlet (mark ptr type disp frame addr)
                           (scheme-mach:break-out-bits-as-integers (elt *memory-vector* i))
                         (declare (ignore ptr disp frame)) ; run dump-memory-with-types if you want 
                                                           ; these broken out 
                                                           ; (included in fields printed!)
                         (format *error-output* 
                                 (cl:if (evenp i)
                                   *format-car-for-dm-decode-p* ; left value (CAR)
                                   *format-cdr-for-dm-decode-p*) ; right value (CDR)
                                 (cl:if (plusp mark)
                                   "*"
                                   " ")
                                 (int->type-name type)
                                 addr
                                 (if (and (or show-registers show-stack-membership)
                                          (cdr (assoc (floor (/ i 2)) reg-address-alist)))
                                   (format nil "(~{~A ~})" (cdr (assoc (floor (/ i 2)) reg-address-alist)))
                                   ""))))
         #'(lambda (i) (format *error-output* "~11,'0o " (elt *memory-vector* i))))
       #'(lambda (i) (format *error-output* "~8,'0o: " (/ i 2)))))))

(defun dump-memory-with-types (&optional (start 0) &key (end (bit-vector->integer microlisp-shared:*memtop*)))
  "similar to (dump-memory) but prints out each field separately"
  (let ((*dump-values-per-row* 2)) ; override if needed
    (dump-memory-internal
     start end
     #'(lambda (i) (mlet (mark ptr type disp frame addr)
                       (scheme-mach:break-out-bits-as-integers (elt *memory-vector* i))
                     (format *error-output* 
                             (cl:if (evenp i)
                               *format-car-for-dm-with-types* ; left value (CAR)
                               *format-cdr-for-dm-with-types*) ; right value (CDR)
                             mark ptr 
                             (int->type-name type)
                             disp frame addr)))
     #'(lambda (i) (format *error-output* "~8,'0o: " (/ i 2))))))

;; Memory comparison function for testing

(defun compare-memory (goal-array start-address len &optional failure-handling)
  "Given a goal-array of memory (a list of numbers similaar to what the test-n sets up as *cold-boot-memory-array*) a
starting offset into the simulated memory and a length, we compare the two arrays, returning three values, the first
indicating success or failure, the second indicating the address and the third indicating if the CAR or the CDR was
the first part of that address to fail the comparison (we check the CAR before the CDR). Failure-handling does nothing
if NIL (the default). If :warn a warning is issued, and otherwise an error is signalled."
  (let ((remaining-words len)
        (current-address start-address)
        (cdr-p nil)
        (goal (copy-list goal-array)))
    (while (plusp remaining-words)
       (let ((mem (read-address current-address cdr-p t))
             (goal-mem (if (bit-vector-p (car goal))
                         (bit-vector->integer (car goal))
                         (car goal)))) ; comparison as integers is simpler
         (cond
           ((eql goal-mem mem)
            (setq cdr-p (not cdr-p))
            (pop goal)
            (when (not cdr-p)
              (incf current-address)
              (decf remaining-words)))
           ((eql failure-handling :warn)
            (warn "Compare-memory failure: expected ~o at ~a of address ~o but saw ~o"
                  goal-mem
                  (cl:if cdr-p "CDR" "CAR")
                  current-address
                  mem)
            (return-from compare-memory (values nil current-address (cl:if cdr-p :cdr :car))))
           (failure-handling
            (error "Compare-memory failure: expected ~o at ~a of address ~o but saw ~o"
                   goal-mem
                   (cl:if cdr-p "CDR" "CAR")
                   current-address
                   mem)
            (return-from compare-memory (values nil current-address (cl:if cdr-p :cdr :car))))
           (t
            (return-from compare-memory (values nil current-address (cl:if cdr-p :cdr :car)))))))
    (values t nil nil)))

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
     
