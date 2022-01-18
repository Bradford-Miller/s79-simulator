(in-package :s79-console)

(scheme-79:scheme-79-version-reporter "Scheme Machine Console" 0 3 3
                                      "Time-stamp: <2022-01-13 14:26:31 gorbag>"
                                      "handle display of non-standard registers")

;; 0.3.2   1/17/22 handle display of non-standard registers

;; 0.3.1   1/13/22 change references from internal-freeze to run-nano
;;                    for consistancy with AIM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.22  1/ 6/22 use property accessor fns

;; 0.1.21 12/15/21 use microlisp-shared pkg

;; 0.1.20 12/10/21 pads now in microlisp pkg

;; 0.1.19 12/ 1/21 add temporary code for debugging the background stream

;; 0.1.18 11/12/21 move break-out-bits-as-integers to machine-defs

;; 0.1.17 11/ 2/21 clear-register-description-pane

;; 0.1.16 10/26/21 use *control-lines* and *sense-lines* instead of
;;                     individual declarations to help automate
;;                     changing them.

;; 0.1.15 10/21/21 support micro-pc and nano-pc as bit-vectors

;; 0.1.14 10/20/21 Promote redraw-console to regular function (was a
;;                    flet) so we can call it when debugging

;; 0.1.13 10/12/21 Add support for a 4th diagnostics panel to display
;;                    results of evaluating predicates (so we can
;;                    make sure we got success and failure cases
;;                    covered!)

;; 0.1.12 10/11/21 Always show the value of NIL's address in memory
;;                    (car is used for temp address, but otherwise
;;                    NIL's own pointer, i.e. 0, and CDR is a pointer
;;                    to NIL's property list, which should be the
;;                    OBARRAY per the AIM 559

;; 0.1.11 10/ 7/21 Extend "Run-Until" to accept a u-address breakpoint
;;                    as well as a tick 
;;                 Also have the focused-register recorded in the
;;                    interface and refreshed whenever we refresh the
;;                    register grid

;; 0.1.10  9/30/21 Fix presentation of Mark sense (got confused with
;;                    the Mark control)

;; 0.1.9   9/28/21 Add alternate-callback for Run button to allow
;;                    specifying a tick to Run until. (since this
;;                    isn't supported for button-panels I added a
;;                    button instead and commented out the alternate
;;                    callback)

;; 0.1.8   9/21/21 Add control columns for mark! unmark! type! and
;;                    pointer!

;; 0.1.7   9/17/21 Add column for decoded type name of register content
;;                    if register supports type field.

;; 0.1.6   9/13/21 Split some definitional and macro stuff into
;;                    s79-console-defs to resolve compilation ordering
;;                    issue

;; 0.1.5   9/10/21 simplify specifying control/sense columns slightly
;;                    (moving toward ability to auto-generate from
;;                    machine declarations)

;; 0.1.4   9/ 9/21 use type=type and type=pointer instead of TNP sense
;;                    line (reality is we'd use an inverter in
;;                    hardware, but we can't specify that yet

;; 0.1.3   9/ 7/21 only use address portion of stack to print top N entries

;; 0.1.2   8/25/21 aesthetics on halt

;; 0.1.1   8/20/21 remove check-box function, add image for status lights

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.39   8/17/21     enhance the source printout to include address tag
;;                          info (e.g. $cons-done2285) so we can
;;                          better understand the code snippet

;; 0.0.38   8/16/21     add info to the nano detail pane about what will
;;                          happen in the next phase (which internal
;;                          blocks listed in clock-triggers.lisp will
;;                          fire)

;; 0.0.37   8/13/21     add *halt-address*, halt-processing

;; 0.0.36   8/12/21 cosmetics (esp increase pane min-height showing
;;                    stack addresses)

;; 0.0.35   8/11/21     show stack addresses, show 5 words of memory (was 3);
;;                          may want to generify and allow presentation of any
;;                          N words, not just those at *address* (TBD)

;; 0.0.34   8/10/21     add redraw for diagnostics when running

;; 0.0.33   8/ 9/21     display pseudo-registers (electrically they're registers)

;; 0.0.32   8/ 6/21     start-diagnostics button

;; 0.0.31   8/ 4/21     change name of button-selection-callback to s79-...
;;                         as the generic is used by the examples (so while
;;                         debugging got name collisions :-)

;; 0.0.30   8/ 2/21     fix typo preventing int-rq from being indicated

;; 0.0.29   7/29/21     Cosmetics

;; 0.0.28   7/26/21     fix some abbrevs and headers
;;                        add internal-freeze indicator

;; 0.0.27   7/16/21     take advantage of optional argument to redraw fn on
;;                        dso to avoid using *dso* (which isn't declared yet)

;; 0.0.26   7/15/21     take advantage of new optional argument to
;;                        read-address to get an integer back (for display)

;; 0.0.25   6/23/21     when we step or u-step, tell the DSO to update if it's running

;; 0.0.24   6/14/21     more cosmetics

;; 0.0.23   5/28/21     increase ucode source column width

;; 0.0.22   4/26/21     print microcode source (from disassembler) as a string, 
;;                        not symbols to avoid printing the package
;;                      Get rid of *freeze-state* and use the actual pad instead

;; 0.0.21   4/21/21     update-ucode-status now uses find-likely-microcode-tag to find nearest one

;; 0.0.20   4/20/21     increase ucode-value-column-width

;; 0.0.19   3/27/21     force redraw of indicators after checking their value each clock phase

;; 0.0.18   3/18/21     add indicator when *reset* has been raised (internally or externally)

;; 0.0.17   3/16/21     redo ucode state presentation (apparently the
;;                          generator can't pass a function so made it
;;                          an offset) print top *ucode-depth*
;;                          elements of stack (if non-zero pointer) in
;;                          Ucode State - only marginally the ucode
;;                          state, may want a new section for stuff
;;                          related to the "lisp" interpretation

;; 0.0.16   3/15/21     fix it again ;-) have ucode state print what *stack*
;;                          *args* and *intermediate-argument* point
;;                          to (TBD: should check it's valid) force
;;                          redraw of ucode and ncode rows when we
;;                          refresh their respective panels and do so
;;                          on every utick for now limit ucode and
;;                          ncode rows to *xcode-depth*

;; 0.0.15   3/14/21     fix lookup of ucode instruction

;;          2/16/21     delete some old debug code

;; 0.0.14   2/ 2/21     fix per-tick updates to show more low level changes

;; 0.0.13   2/ 1/21     show 3 words at and following the content of the *address* pseudo register

;; 0.0.12   1/26/21     update external bits on each nano cycle (yeah it's expensive but you think channeling
;;                        all that information to old-time consoles was cheap? ;-)
;; 0.0.11   1/20/21     when focused on a register with a pointer, show what's in memory (car and cdr)
;;                        fixup nanocode display, add PC (state) to nanocode and ucode displays,
;;                        fix focus function for ucode and nanocode (ignore)
;; 0.0.10   1/19/21     when focused on a register, also display the type symbolically
;; 0.0.9    1/15/21     also start to add current ncode display (may make it optional since not sure will be
;;                        needed once mapping debugged!)
;; 0.0.8    1/12/21     start to add a new grid showing the current ucode instruction and some symbolic
;;                        information on state
;; 0.0.7    1/ 9/21     invalidate register presentations after step/micro-step so they get updated
;; 0.0.6    1/ 6/21     implement step and micro-step buttons
;; 0.0.5    1/ 4/21     prompt for new values for registers when focused; add additional panels for external interfaces
;; 0.0.4    1/ 4/21     also allow sense lines to be declared disabled (well not enabled anyway) and supress printing them
;; 0.0.3    1/ 3/21     add disabled feature for checkboxes and macro-ize building functions
;; 0.0.2    1/ 2/21     change control lines to checkboxes and allow user to set them (still not validated)
;; 0.0.1   12/28/20     Add (start-console) function
;; 0.0.0   12/26/2020   New

;; define a CAPI console to help with debugging the microcode simulation

;; This code originally from Lispworks examples to use the
;; grid-impl.lisp functions. By the time you see this, little to none
;; of the original code should be left, but we do want to acknowledge
;; the help! (Note that we do use a patched version of the file
;; grid-impl.lisp
;;
;; since we don't have permission to redistribute that, we will
;; include instructions on how to patch the version you should have
;; with your lispworks release)

;; EVENTUALLY I plan to add more physical control-like entities
;; (toggle switches, dials, etc. as one would have seen on a mainframe
;; in the day, and for similar reasons: to debug the processor
;; itself!) to mimic a physical instantiation, so consider this a work
;; in progress


;; these will be defined by microcode file
(declaim (special **pointer-types** **non-pointer-types** **pointer** **machine-registers**))


(defun start-console ()
  ;; don't run console unless we are ready as display depends on
  ;; having microcode loaded and validated.
  (when (machine-ready-p t) 
    (capi:display (setq *console* (make-instance 's79-console-interface)))
    (update-ucode-status)
    (update-ncode-status)))

;; some parameters to make sizing the display easier. These were set empircally.

;; images
(gp:register-image-translation
 'on-image
 #.(gp:read-external-image (current-pathname "images/16x16/green_light.bmp")
                           :transparent-color-index 0))

(gp:register-image-translation
 'off-image
 #.(gp:read-external-image (current-pathname "images/16x16/red_light.bmp")
                           :transparent-color-index 0))

(gp:register-image-translation
 'error-image
 #.(gp:read-external-image (current-pathname "images/16x16/yellow_light.bmp")
                           :transparent-color-index 0))

(defun status-drawing-function (row column pane width height)
  (declare (ignorable width height))

  (let* ((register (cell-data-object row column))
         (displayer (cell-displayer row column))
         (check-box-fn (intern (format nil "REGISTER-~A-FUNCTION" (name displayer)) (find-package :s79-console)))
         (status (funcall check-box-fn register)))
    (unless (eql status :disabled)
      (gp:draw-image pane
                     ;; temp - try with known ok images
                     (gp:load-image pane (if status 'plus-image 'minus-image))
                     10 10))))

(defun s79-console-rows ()
   (let ((registers *s79-register-metadata*))
     (mapcar #'(lambda (register)
                 (make-instance 'grid-data-row
                                :data-object register
                                :focus-function 'set-register-value
                                :size *sense-and-control-height*))
             registers)))

(defun update-register-description-pane (interface text)
  (let ((editor-pane (slot-value interface 'register-desc-pane)))
    (capi:modify-editor-pane-buffer
     editor-pane :contents text)))

(defun clear-register-description-pane (interface)
  (update-register-description-pane interface ""))

(defun describe-register (interface)
  (let*-non-null ((reg (focused-register interface))
                  (value (symbol-value (register-symbol reg)))
                  (reg-size (length value)))
    (when reg
      (if (eql reg-size *register-size*) ; normal register
        (mlet (mark ptr type disp frame addr) (break-out-bits-as-integers value)
          (update-register-description-pane
           interface 
           (format nil "~30a : mark: ~b ptr: ~b type: ~2,'0o disp: ~4,'0o frame: ~4,'0o~%~32t address: ~8,'0o~%~32t type: ~a~%~32t mem:-> ~a"
                   (register-name reg)
                   mark ptr type disp frame addr
                   (int->type-name type)
                   ;; might want to put this in it's own pane to allow poking around... (TBD)
                   (if (zerop ptr)        ; it's a pointer
                     (if (invalid-address-p addr)
                       (format nil "-> ~s" (invalid-address-p addr))
                       (let ((*warn-when-beyond-memtop* nil))
                         (format nil "(~11,'0o . ~11,'0o)"
                                 (read-address addr nil t) ; car
                                 (read-address addr t t)))))))) ; cdr
        ;; odd register (likely a PC)
        (update-register-description-pane
         interface
         (format nil "~30a : ~o"
                 (register-name reg)
                 (bit-vector->integer value)))))))

(defun set-register-value (row column on)
  (when on 
    (let* ((reg (cell-data-object row column))
           (displayer (cell-displayer row column))
           (writer (band-data-writer displayer))
           (interface (capi:element-interface (top-level-widget row)))
           (raw-value (symbol-value (register-symbol reg)))
           (current-value (bit-vector->integer raw-value)))
      
      (setf (focused-register interface) reg)
      (describe-register interface)

      (when (and writer
                 ;; make sure we clicked on a register and not a control line else we'll get the "wrong" writer function
                 (eql (name displayer) :value))
        ;; might want to let the read base be configurable - hex may be more convenient in the future?
        (let ((new-value (capi:prompt-for-string "Enter a new value for the register (in octal)"
                                                 :value-function #'(lambda (x)
                                                                     (let* ((*read-base* 8)
                                                                            (integer (ignore-errors
                                                                                      (read-from-string x))))
                                                                       (values integer (or (not (integerp integer))
                                                                                           (minusp integer)
                                                                                           (> integer #xffffffff)))))
                                                 :initial-value current-value)))
          (when (and new-value ; check if aborted
                     (not (= current-value new-value)))
            (funcall writer reg new-value)
            (redraw-cell row column)))))))

        
;; this allows the user to interactively set the control lines and
;; single step the machine to see what would happen. Specific existing
;; microcode instructions may or may not exist for the combination
;; selected, of course!

(eval-when (:compile-toplevel :load-toplevel :execute)
  (mapc #'(lambda (control)
            (eval `(register-check-box-fn ,control))) ; a macro that creates a defun...
        *control-lines*)
  (mapc #'(lambda (sense)
            ;; ignore some of the sense lines as we implement them specially
            (unless (member sense '(type-not-pointer mark-bit not-mark-bit))
              (eval `(register-sense-fn ,sense))))
        *sense-lines*))

;; common macro won't generate the sense needed for this
(defun register-mark-bit-function (register-metadata)
  (cl:if (member 'mark-bit (valid-sense-lines (register-symbol register-metadata)))
         (plusp (mark-bit (symbol-value (register-symbol register-metadata))))
         :disabled))

(defun print-control-status (x) ; x is the value of the data-reader
  (format nil "~a" (if x (if (eql x :disabled) " " "T") "NIL")))

(defun print-register-contents (x) ; x is the value of the data-reader

  ;; this won't work because we don't have the metadata (to call register-size upon)
  #||(format nil "~?"
          (concatenate 'string
                       "~"
                       (princ-to-string
                        ;; 3 bits per octal digit
                        (ceiling (/ (register-size x) 3)))
                       ",'0o")
          (list (bit-vector->integer x))) ||#
  ;; so just have to use the default register-size, or 11 octal bits
  (format nil "~11,'0o" (bit-vector->integer x)))

(defun make-register-widget ()
  (make-instance 
   'grid-widget
    :name :s79-machine-console
    :title "Machine State Info (Key: FR = from ADR = address DSP = displacement FRAM = frame INC = incremented DEC = decremented)"
    :font *header-row-font*
    
    :column-sections
    (list
     ;; The first column section is the row header, displaying the
     ;; name of each register
     (make-instance 'grid-column-section
                    :name :register
                    :scrollable-p nil
                    ;;:proportional-p nil
                    :size *register-name-column-width*
                    :bands
                    (list (make-instance 'grid-display-column
                                         :header-title "Register Name(s)"
                                         :name :name
                                         :data-reader 'register-name
                                         :band-dragging t 
                                         :proportional-p t
                                         :right-border-spec 1
                                         :size *register-name-column-width*)))

     ;; the second column is the value of the register. Eventually
     ;; we will do this in binary graphics, but for now, just print
     ;; it in octal
     (make-instance 'grid-column-section
                    :name :register-value
                    :scrollable-p nil
                    :size *register-value-column-width*
                    :bands
                    (list (make-instance
                           'grid-display-column
                           :header-title "Value"
                           :name :value
                           :data-reader #'(lambda (x)
                                            (symbol-value (register-symbol x)))
                           :data-writer #'(lambda (x y)
                                            (copy-field
                                             (integer->bit-vector
                                              y
                                              :result-bits (register-size x))
                                             (symbol-value (register-symbol x))))
                           :size *register-value-column-width*
                           :print-function 'print-register-contents
                           )))

     ;; Add a column indicating the decoded type of the register contents
     (make-instance 'grid-column-section
                    :name :register-symbolic-type
                    :scrollable-p nil
                    :size *register-name-column-width*
                    :bands
                    (list (make-instance 'grid-display-column
                                         :header-title "Type-Field"
                                         :name :symbolic-type
                                         :data-reader #'(lambda (x)
                                                          (let ((type-field-sym (intern (format nil "*~A-TYPE*"
                                                                                                (strip-register-name (register-symbol x)))
                                                                                        *scheme-mach*)))
                                                            (cl:if (boundp type-field-sym)
                                                              (symbol-value type-field-sym))))
                                         :size *register-name-column-width*
                                         :print-function #'(lambda (x)
                                                             (cl:if x
                                                               (let ((type-num (bit-vector->integer x)))
                                                                 (cl:if (< type-num **pointer**)
                                                                   (string (int->pointer-type-name type-num))
                                                                   (string (int->non-pointer-type-name type-num))))
                                                               "")))))
              
     ;; The third columnm section is a button grid conveying the
     ;; control and sense line states of the register
     (flet ((make-button-column (control-p name-string
                                 &optional (name-symbol (make-keyword (string-upcase name-string))))
              (let ((fn-symbol (line-name-to-fn-symbol name-symbol)))
                (make-instance 'grid-display-column
                               :name name-symbol
                               :header-title name-string
                               :data-reader fn-symbol
                               :data-writer (cl:if control-p fn-symbol nil)
                               :drawing-function 'status-drawing-function
                               :size *sense-and-control-column-width*
                               :print-function nil))))
       (make-instance 'grid-column-section
                      :name :state
                      :scrollable-p nil
                      :size *sense-and-control-total-column-width*
                      :right-border-spec nil
                      :bands
                      (list
                       ;; so far, this can't be "automated" based on
                       ;; *control-lines* and *sense-lines* so we can
                       ;; define abbreviations. Once we move that data
                       ;; into the metadata (or definition structure),
                       ;; though... (TBD)

                       ;; (Key: ADR = address DSP = displacement FRAM = frame INC = incremented DEC = decremented)
                       ;; control lines
                       (make-button-column t "To")
                       (make-button-column t "To-Type")
                       (make-button-column t "To-ADR" :to-address)
                       (make-button-column t "To-DSP" :to-displacement)
                       (make-button-column t "To-FRAM" :to-frame)
                       (make-button-column t "Mark!")
                       (make-button-column t "UnMark!")
                       (make-button-column t "Type!")
                       (make-button-column t "Ptr!" :pointer!)
                       (make-button-column t "From")
                       (make-button-column t "FR-DEC" :from-decremented)
                       (make-button-column t "FR-INC" :from-incremented)
                       (make-button-column t "FR-Type" :from-type)

                       ;; sense lines
                       (make-button-column nil "ADR=Bus" :address=bus)
                       (make-button-column nil "Typ=Bus" :type=bus)
                       (make-button-column nil "=Bus")
                       (make-button-column nil "Mark" :mark-bit)
                       (make-button-column nil "Typ=Typ" :type=type)
                       (make-button-column nil "Typ=Ptr" :type=pointer)
                       (make-button-column nil "FRAM=0" :frame=0)
                       (make-button-column nil "DSP=0" :displacement=0)
                       (make-button-column nil "ADR=0" :address=0)
                       ;; (Key: ADR = address DSP = displacement FRAM = frame INC = incremented DEC = decremented)
                       ))))
       
    :row-sections
    (list
     
     ;; The first row section consists of the column headers
     (make-instance 'grid-special-row
                    :name :header
                    
                    :data-getter :other-band  ;; so sw get the column as the data-object
                    :data-reader 'header-title ;; and reads the column title
                    :band-dragging t
                    :size *sense-and-control-height*
                    :bottom-border-spec 4)
     
     ;; The second row section contains the actual register rows.
     ;; Add the data rows to this row section at runtime individually,
     (make-instance 'grid-row-section
                    :name :body
                    :scrollable-p nil
                    :size *sense-and-control-total-height*
                    :resizable-p nil
                    :top-border-spec '(:thickness 4 :foreground :green) ;; show that it works
                    :bands (s79-console-rows)))))


(defun create-register-grid ()
  (make-register-widget))

(defun nth-in-stack (n)
  (cond
    ((or (zerop n) (zerop (bit-vector->integer *stack-address*)))
     *stack-address*)
    (t
     (let ((current-element *stack-address*))
       (dotimes (i n)
         (unless (zerop (bit-vector->integer current-element)) ; cdr of NIL is the OBTAB
           (setq current-element (read-address current-element t)))) ; follow spaghetti stack
       current-element))))

(defun ucode-rows ()
  (let ((result nil))
    (dotimes (n *ucode-depth*)
      (push (make-instance 'grid-data-row
                           :data-object n
                           :accepts-focus-p nil
                           :size *ucode-height*)
            result))
    (nreverse result)))

(defun ncode-rows ()
  (let ((result nil))
    (dotimes (n *ncode-depth*)
      (push (make-instance 'grid-data-row
                           :data-object n
                           :accepts-focus-p nil
                           :size *ucode-height*)
            result))
    (nreverse result)))

(defun print-pointer (x)
  (let ((*warn-when-beyond-memtop* nil))
    (cond
      ((plusp (bit-vector->integer x))
       (format nil "~o:(~o . ~o)" (bit-vector->integer x) (read-address x nil t) (read-address x t t)))
      (t
       "NIL"))))

(defun make-ucode-widget ()
  (make-instance 
   'grid-widget
    :name :s79-machine-console-ucode
    :title "Ucode State Info"
    :font *widget-font*
    :column-sections
    (list
       ;; The first column section is the row header
       (make-instance 'grid-column-section
                      :name :ucode-offset
                      :scrollable-p nil
                      :size *ucode-offset-column-width*
                      :bands
                      (list (make-instance 'grid-display-column
                                           :header-title "UAddr (oct)"
                                           :name :ucode-offset
                                           :data-reader #'(lambda (n) (car (nth n (ucode-display-array *console*))))
                                           :band-dragging t 
                                           :proportional-p t
                                           :right-border-spec 1
                                           :size *ucode-offset-column-width*
                                           :print-function #'(lambda (x)
                                                               (let ((tag (microop-symbol x)))
                                                                 (format nil "~o:~a" x (if tag tag "")))))))



       ;; the second column is the ucode instruction at that offset. 
       (make-instance 'grid-column-section
                      :name :ucode-value
                      :scrollable-p nil
                      :size *ucode-value-column-width*
                      :bands
                      (list (make-instance 'grid-display-column
                                           :header-title "Code (oct)"
                                           :name :ucode-value
                                           :data-reader #'(lambda (n)
                                                            (cdr (nth n (ucode-display-array *console*))))
                                           :size *ucode-value-column-width*
                                           :print-function #'(lambda (x) (format nil "(~{~o ~})" x)))))

       (make-instance 'grid-column-section
                      :name :ucode-symbolic
                      :scrollable-p nil
                      :size *ucode-symbolic-column-width*
                      :bands
                      (list (make-instance 'grid-display-column
                                           :header-title "Source"
                                           :name :ucode-symbolic
                                           :data-reader #'(lambda (n)
                                                            (scheme-79-mcr-i:disassemble-microcode
                                                             (cdr (nth n (ucode-display-array *console*)))))
                                           :size *ucode-symbolic-column-width*
                                           :print-function #'(lambda (x)
                                                               (format nil "~a" x)))))

              (make-instance 'grid-column-section
                      :name :comments
                      :scrollable-p nil
                      :size *ucode-comment-column-width*
                      :bands
                      (list
                       (make-instance 'grid-display-column
                                      :header-title "Info"
                                      :size *ucode-comment-column-width*
                                      :data-reader #'(lambda (n) (nth n (ucode-comment-array *console*)))
                                      :print-function #'(lambda (x) (format nil "~a" x)))))

              ;; probably want to step through the n-levels (*ucode-depth*) of  stack
              (make-instance 'grid-column-section
                      :name :stack
                      :scrollable-p nil
                      :size *ucode-ptr-column-width*
                      :bands
                      (list
                       (make-instance 'grid-display-column
                                      :name :stack
                                      :header-title "Stack"
                                      :data-reader #'nth-in-stack
                                      :size *ucode-ptr-column-width*
                                      :print-function 'print-pointer
                                      )))
       )
       
    :row-sections
    (list
     
     ;; The first row section consists of the column headers
     (make-instance 'grid-special-row
                    :name :header
                    
                    :data-getter :other-band  ;; so sw get the column as the data-object
                    :data-reader 'header-title ;; and reads the column title
                    :band-dragging t
                    :size *ucode-height*
                    :bottom-border-spec 4)
     
     ;; The second row section contains the microcode description rows.
     ;; Add the data rows to this row section at runtime individually,
     (make-instance 'grid-row-section
                    :name :ucode-body
                    :scrollable-p nil
                    :size *ucode-total-height*
                    :resizable-p nil
                    :top-border-spec '(:thickness 4 :foreground :green) ;; show that it works
                    :bands (ucode-rows)))))

(defun make-ncode-widget ()
  (make-instance 
   'grid-widget
    :name :s79-machine-console-ncode
    :title "Ncode State Info"
    :font *widget-font*
    :column-sections
    (list
       ;; The first column section is the row header
       (make-instance 'grid-column-section
                      :name :ncode-offset
                      :scrollable-p nil
                      :size *ncode-offset-column-width*
                      :bands
                      (list (make-instance 'grid-display-column
                                           :header-title "nAddr (oct) "
                                           :name :ncode-offset
                                           :data-reader #'(lambda (n) (car (nth n (ncode-display-array *console*))))
                                           :proportional-p t
                                           :right-border-spec 1
                                           :print-function #'(lambda (x) (format nil "~o" x))
                                           :size *ncode-offset-column-width*
                                           )))

       ;; the second column is the nanocode operation name (if any)
       (make-instance 'grid-column-section
                      :name :ncode-name
                      :scrollable-p nil
                      :size *ncode-name-column-width*
                      :bands
                      (list (make-instance 'grid-display-column
                                           :header-title "Name"
                                           :name :ncode-name
                                           :size *ncode-name-column-width*
                                           :print-function #'(lambda (n)
                                                               (format nil "~a"
                                                                       (or
                                                                        (car (rassoc (car (nth n (ncode-display-array *console*))) *nanocontrol-symtab*))
                                                                        "")))
                                           )))
                      
                      
       ;; the third column is the ncode instruction at that offset. 
       (make-instance 'grid-column-section
                      :name :ncode-value
                      :scrollable-p nil
                      :size *ncode-value-column-width*
                      :bands
                      (list (make-instance 'grid-display-column
                                           :header-title "Code (oct)"
                                           :name :ncode-value
                                           :data-reader #'(lambda (n) (cdr (nth n (ncode-display-array *console*))))
                                           :size *ncode-value-column-width*
                                           ;; eventually pad to bit size (once we've controlled for that)
                                           :print-function #'(lambda (x) (format nil "(~{~o ~})" x))
                                           )))

       ;; THE next column is the decoded controls
       (make-instance 'grid-column-section
                      :name :controls
                      :scrollable-p nil
                      :size *control-total-column-width*
                      :bands
                      (list
                       ;; (Key: ADR = address DSP = displacement FRAM = frame INC = incremented DEC = decremented)
                       (make-instance 'grid-display-column
                                      :name :pad-controls
                                      :header-title "Pad Controls"
                                      :data-reader #'(lambda (n) (cdr (nth n (ncode-display-array *console*))))
                                      :size *control-column-width*
                                      :print-function #'(lambda (x)
                                                          (format nil "(~{~a ~})"
                                                                  (decode-nanocontrol-pads x)))
                                      )
                       (make-instance 'grid-display-column
                                      :name :register-controls
                                      :header-title "Register Controls"
                                      :size *control-column-width*
                                      :data-reader #'(lambda (n) (cdr (nth n (ncode-display-array *console*))))
                                      :print-function #'(lambda (x)
                                                          (format nil "(~{~a ~})"
                                                                  (decode-nanocontrol-registers x)))
                                      )
                       (make-instance 'grid-display-column
                                      :name :next-state
                                      :header-title "Next NanoInst"
                                      :size *nano-next-column-width*
                                      :data-reader #'(lambda (n) (cdr (nth n (ncode-display-array *console*))))
                                      :print-function #'(lambda (x)
                                                          (format nil "~o"
                                                                  (third x)))
                                      ))))
    :row-sections
    (list
     ;; The first row section consists of the column headers
     (make-instance 'grid-special-row
                    :name :header
                    :data-getter :other-band  ;; so sw get the column as the data-object
                    :data-reader 'header-title ;; and reads the column title
                    :band-dragging t
                    :size *ucode-height*
                    :bottom-border-spec 4)
     
     ;; The second row section contains the actual nanocontrol rows.
     ;; Add the data rows to this row section at runtime individually,
     (make-instance 'grid-row-section
                    :name :body
                    :scrollable-p nil
                    :size *ncode-total-height*
                    :resizable-p nil
                    :top-border-spec '(:thickness 4 :foreground :blue) 
                    :bands (ncode-rows)))))

(defun create-ucode-grid ()
  (make-ucode-widget))

(defun create-ncode-grid ()
  (make-ncode-widget))

(defun set-button-selected (button-name interface selection-state)
  ;; button-name should be the keyword we're using to track indicators
  (if selection-state
      (pushnew button-name (status interface))
      (setf (status interface) (delete button-name (status interface))))
  (setf (capi:button-selected (slot-value interface (intern (string button-name) (find-package :s79-console))))
        selection-state))

(defun redraw-indicators ()
  (dolist (i *all-indicators*)
    (set-button-selected i *console* (member i (status *console*)))))

(defun halt-processing (interface)
  (declare (special *test-suite*)) ; we have to refer to the diagnostics interface so we use the global
  "The original scheme-79 chip doesn't have a HALT instruction, but just loops (generally in the microcode
at location DONE). This is to do some extra work when we notice we are at DONE, e.g. to mark the test has 
been run (if any) and check if it was successful (if such an evaluation function was declared)"
  (when (and (not (null *halt-address*))
             (eql (bit-vector->integer *micro-pc*) *halt-address*))
    (note "We have reached the halt address!")
    (when *test-suite* ; we were running a test suite
      (mark-test-run *test-suite*)
      (when (test-result-interpreter *test-suite*)
        (let* ((result (funcall (test-result-interpreter *test-suite*)))
               (result-msg (if result " Success! " "Failure...")))
          (note-banner (list "Test Suite Result:" " " result-msg) 3)
          ;; also report in the register description pane (clear focused register)
          (setf (focused-register interface) nil)
          (update-register-description-pane interface (format nil "***** TEST SUITE RESULT: ~a *****" result-msg))
          
          (if result
              (mark-all-tests-successful *diagnostics-interface*)
              (mark-all-tests-failure *diagnostics-interface*))))
        
      (setq *test-suite* nil)) ; so we don't keep doing this unless we rerun the test
    t))  ; we should halt

;;    :PH1   :PH2 :FRZ :NANO :ALE    :RD :WR :CDR :RDI :int-rq :GCR :RST :RD-State :LD-State
;;    :Step  :Run :Run-until :Stop :Freeze :RD-State :LD-State :INT-RQ

(defun get-breakpoint-input-internal (pane)
  (let* ((text (text-input-pane-text pane))
         (result (parse-integer text :junk-allowed t)))
    (and (typep result '(integer 0 *)) result))) ; might want to narrow to valid addresses

(defun get-breakpoint-input-internal-1 (layout)
  ;; we're ok if any of the values passes
  (let ((panes (layout-description layout)))
    (cond-binding-predicate-to foo
       ((some #'get-breakpoint-input-internal panes)
        foo)
       (t
        (values nil t)))))

(defun get-breakpoint-input ()
  (let* ((pane1 (make-instance 'capi:text-input-pane
                               :title "tick to break on?"
                               :callback 'exit-confirmer
                               :text-change-callback :redisplay-interface
                               ))
         (pane2 (make-instance 'capi:text-input-pane
                               :title "microaddress to break on?"
                               :callback 'exit-confirmer
                               :text-change-callback :redisplay-interface
                               ))
         (layout (make-instance 'capi:column-layout :description (list pane1 pane2))))
    (mlet (res okp) (capi:popup-confirmer layout "Set breakpoint" :value-function 'get-breakpoint-input-internal-1)
      (declare (ignore res))
      (when okp
        (values (get-breakpoint-input-internal pane1)
                (get-breakpoint-input-internal pane2))))))

(defun redraw-console () ; redraw everything
  (redraw-registers) ; finish the current cycle if we were u-stepping
  (update-ucode-status)
  (update-ncode-status)
  (when (dso-running-p)
    (dso-update-and-redraw))
  (let ((diagnostics-interface (diagnostics-running-p)))
    (when diagnostics-interface
      (redraw-diagnostics diagnostics-interface))))

(defun s79-button-selection-callback (data interface)
  (ecase data
    ;; NB: when we implement run/stop we'll REALLY have to have the
    ;; clock updates in their own process else we'd never see the
    ;; "stop" button press...
    (:step ; momentary on
     ;; right now we end up running the simulation from inside the UI
     ;; process - we probably should have this in it's own process!
     (when (halt-processing interface)
       (note "stepping past HALT"))
     (single-step t)
     (redraw-console))

    (:u-step ; momentary on
     ;; right now we end up running the simulation from inside the UI
     ;; process - we probably should have this in it's own process!
     (when (halt-processing interface)
       (note "microstepping past HALT"))
     (update-clock)
     (redraw-console))

    (:freeze ; toggle on/off
     ;; should be a toggle switch, but for now it's momentary on - off
     (if (check-pad 'microlisp-shared:*freeze*)
       (clear-pad 'microlisp-shared:*freeze*)
       (set-pad 'microlisp-shared:*freeze*))
     ;; update
     (set-button-selected :frz interface (check-pad 'microlisp-shared:*freeze*)))

    (:reset ; momentary on
     ;; should be momentary-on
     (reset)) ; simulates the pad being raised for one clock

    (:stop
     (stop)
     (redraw-console))
      
    (:run
     ;; we define our own version of run here so we can check for halting
     (set-running-p t)
     (while (running-p)
       (update-clock)
       (when (halt-processing interface)
         (stop)
         (redraw-console))))

    (:run-until
     (when (halt-processing interface)
       (note "will run past HALT"))
     (clear-breakpoints) ; for now
     (mlet (end-tick break-uaddress) (get-breakpoint-input)
       (if (not (null end-tick))  ; right now does one or the other not both
         (run-until-tick end-tick)
         (run-until-breakpoint break-uaddress)))
     (redraw-console))

    ;; these require some support from external simulation, or we
    ;; need to add additional panel(s) to configure. Note that rd-state and
    ;; ld-state aren't really needed, given the console, but were part of the
    ;; chip to allow it to be initialized and debugged.
    (:rd-state
     (tbd))
      
    (:ld-state
     (tbd))
      
    (:int-rq
     (tbd))

    ;; these are for additional tools
    (:dso
     (start-dso))
    
    (:diagnostics
     (start-diagnostics))

    ;; for debugging window (temporary)
    (:debug
     (format hcl:*background-output* "~&Bindings: Error-output, standard-output, background-output ~s ~s ~s~%"
             *error-output* *standard-output* hcl:*background-output*))))

;; I wasn't able to get my mac to invoke the alternate-action-callback, so I've added a button for this for now.
#||(defun s79-button-alternate-callback (data interface)
    (case data
      ((:run :step)
       (when (halt-processing interface)
         (note "will run past HALT"))
       (let ((end-tick (prompt-for-integer "Tick to break on?" :min *tick* :initial-value (1+ *tick*))))
         (run-until-tick end-tick))
       (redraw-console)))) ||#

;; some redraw functions
(defun redraw-registers ()
  (gp:invalidate-rectangle (register-grid *console*))
  (describe-register *console*)) ; redraw the focused register description if valid

(defun redraw-ucode ()
  ;; update the data then force redraw
  (let ((ucode (firstn (collect-ucode (bit-vector->integer *micro-pc*)) *ucode-depth*)))
    (dotimes (i *ucode-depth*)
      (cond
       (ucode
        (setf (nth i (ucode-display-array *console*)) (car ucode))
        (pop ucode))
       (t
        (setf (nth i (ucode-display-array *console*)) '(0 0 0 0 0)))))
    (setf (ucode-comment-array *console*)
          (mapcar #'(lambda (uaddr)
                      (let*-non-null ((comment (get-uc-annotation uaddr)))
                          (subseq comment 3))) ; get rid of leading ";; "
                  (mapcar #'car (ucode-display-array *console*)))))

  (gp:invalidate-rectangle (ucode-grid *console*)))

(defun redraw-ncode ()
  ;; update the data then force redraw
  (let ((ncode (firstn (collect-ncode (bit-vector->integer *nano-pc*)) *ncode-depth*)))
    (dotimes (i *ncode-depth*)
      (cond
       (ncode
        (setf (nth i (ncode-display-array *console*)) (car ncode))
        (pop ncode))
       (t
        (setf (nth i (ncode-display-array *console*)) '(0 0 0 0))))))
  
  (gp:invalidate-rectangle (ncode-grid *console*)))

(defun update-data-status ()
  (let ((address-data-panel (slot-value *console* 'address-data-panel))
        (*warn-when-beyond-memtop* nil)) ; supress warnings
    (mlet (mark ptr type disp frame) (break-out-bits-as-integers microlisp-shared:*memory-pads*)
          ;; also want to show next three addresses - not necessarily in same list!
          (let* ((a1 (logand (bit-vector->integer microlisp-shared:*address*) *address-field-mask*))
                 (a1-car (read-address a1 nil t))
                 (a1-cdr (read-address a1 t t))
               
                 (a2 (1+ a1))
                 (a2-car (read-address a2 nil t))
                 (a2-cdr (read-address a2 t t))

                 (a3 (1+ a2))
                 (a3-car (read-address a3 nil t))
                 (a3-cdr (read-address a3 t t))

                 (a4 (1+ a3))
                 (a4-car (read-address a4 nil t))
                 (a4-cdr (read-address a4 t t))

                 (a5 (1+ a4))
                 (a5-car (read-address a5 nil t))
                 (a5-cdr (read-address a5 t t))

                 ;; special cells
                 (a-nil 0) ; nil always points to 0
                 (a-nil-car (read-address a-nil nil t))
                 (a-nil-cdr (read-address a-nil t t))) ; global property list
          
            (capi:modify-editor-pane-buffer
             address-data-panel
             :contents               
             (setf (external-data-text *console*)
                   (format nil 
"EXTERNAL BUS ADDRESS/DATA      EXTERNAL MEMORY                             EXTERNAL MEMORY SPECIAL CELLS
Current        ---Address---   #o~8,'0o : #o~11,'0o . #o~11,'0o             NIL (0)   : #o~11,'0o . #o~11,'0o  
mark ptr  type displ  frame    #o~8,'0o : #o~11,'0o . #o~11,'0o
#b~b  #b~b  #o~2,'0o #o~4,'0o #o~4,'0o   #o~8,'0o : #o~11,'0o . #o~11,'0o
                               #o~8,'0o : #o~11,'0o . #o~11,'0o
                               #o~8,'0o : #o~11,'0o . #o~11,'0o"
                           a1 a1-car a1-cdr a-nil-car a-nil-cdr
                           a2 a2-car a2-cdr
                           mark ptr type disp frame
                           a3 a3-car a3-cdr
                           a4 a4-car a4-cdr
                           a5 a5-car a5-cdr)))))

    ;; redraw it
    (gp:invalidate-rectangle address-data-panel)))

(defun update-ucode-status ()
  (let ((ucode-state-panel (slot-value *console* 'ucode-general-state))
        pc-class
        pc-symbolic-name)
    (cond
      ((setq pc-symbolic-name (car (rassoc (bit-vector->integer *micro-pc*) **pointer-types** :key #'car :test #'=)))
       (setq pc-class :pointer-type))
      ((setq pc-symbolic-name (car (rassoc (bit-vector->integer *micro-pc*) **non-pointer-types** :key #'car :test #'=)))
       (setq pc-class :non-pointer-type))
      (t
       (setq pc-class :code-entry)
       (mlet (tag adder) (find-likely-microcode-tag (bit-vector->integer *micro-pc*))
         (setq pc-symbolic-name
               (if tag  
                   (format nil "~a+~d" tag adder)
                   "?")))))
    
    (capi:modify-editor-pane-buffer   ucode-state-panel
                                      :contents               
                                      (setf (ucode-data-text *console*)
                                            (format nil " Micro PC: ~o (~a: ~a)~%~A~A"
                                                    (bit-vector->integer *micro-pc*)
                                                    pc-class
                                                    pc-symbolic-name
                                                    ;; add reminders - we could just note the indicators near the bottom, but
                                                    ;; they're easy to miss...
                                                    (if (test-pad-immediate 'microlisp-shared:*freeze*)
                                                        "External Freeze! "
                                                        "")
                                                    (if (test-pad-immediate 'microlisp-shared:*run-nano*)
                                                        "Nanocontrol Running! "
                                                        "")
                                                    )))
    (gp:invalidate-rectangle ucode-state-panel)
    (redraw-ucode))) ; also redraw the ucode panel

(defun execution-blocks-in-phase (phase-name)
  (let ((results ""))
    (labels ((check-block (blockparam)
               (when (phase-equal phase-name (symbol-value blockparam))
                 (setq results (concatenate 'string results (string blockparam) " ")))))
      ;; now just list out the things we want to note from clock-triggers.lisp
      (mapc #'check-block
            '(*run-microcontroller*
              *run-external-data-transfer*
              *run-nanocontroller-p1*
              *run-register-controls*
              *update-sense-lines*
              *run-nanocontroller-p2*)))
    results))

(defun update-ncode-status ()
  (let ((ncode-state-panel (slot-value *console* 'ncode-general-state)))
    (capi:modify-editor-pane-buffer   ncode-state-panel
                                      :contents               
                                      (setf (ncode-data-text *console*)
                                            (format nil " Nano PC: ~o (~a) nTick: ~a~%Next: ~a"
                                                    (bit-vector->integer *nano-pc*)
                                                    (or (car (rassoc (bit-vector->integer *nano-pc*)
                                                                     *nanocontrol-symtab*))
                                                        "?")
                                                    *symbolic-clock-phase*
                                                    (execution-blocks-in-phase (next-phase *symbolic-clock-phase*)))))
    (gp:invalidate-rectangle ncode-state-panel)
    (redraw-ncode)))


(capi:define-interface s79-console-interface ()
  ((register-grid-widget :initform (create-register-grid)
                         :accessor register-grid)
   (ucode-status-widget :initform (create-ucode-grid)
                        :accessor ucode-grid)
   (ncode-status-widget :initform (create-ncode-grid)
                        :accessor ncode-grid)
   (external-status :initform nil
                    :accessor status) ; should tie to simulator's idea of what's on the pins
   (external-data-text :initform ""
                       :accessor external-data-text)
   (ucode-data-text :initform ""
                    :accessor ucode-data-text)
   (ncode-data-text :initform ""
                    :accessor ncode-data-text)
   (ucode-display-array :initform '((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0))
                        :accessor ucode-display-array)
   ;; this is temporary, eventually we will fill in with symbolic information on what the original
   ;; microcode source looked like
   (ucode-comment-array :initform '("<- Just Executed" "<- Next to Execute")
                        :accessor ucode-comment-array)
   (ncode-display-array :initform '((0 0 0 0) (0 0 0 0) (0 0 0 0)) ; so far, we don't have any ncode longer than 3
                        :accessor ncode-display-array)
   (focused-register :initform nil
                     :accessor focused-register)
   )
  
  (:panes
   (register-desc-pane
    capi:editor-pane
    :contents ""
    :enabled nil ;; don't want the user to edit this
    :buffer-name "Register Description")

   (address-data-panel
    capi:editor-pane
    :text external-data-text
    :visible-min-height '(:character 7)
    :enabled nil ; may eventually want to be able to enter in numbers, e.g. as the interrupt address (?)
    :buffer-name "Address/Data (external)")

   (ucode-general-state
    capi:editor-pane
    :text ucode-data-text
    :visible-min-height '(:character 3)
    :buffer-name "Microcode Info"
    :enabled nil)

   (ncode-general-state
    capi:editor-pane
    :text ncode-data-text
    :visible-min-height '(:character 3)
    :buffer-name "Nanocode Info"
    :enabled nil)

   (ph1
    capi:check-button
    :data :ph1
    :print-function 'capitalize-if-symbol
    :enabled nil)

   (ph2
    capi:check-button
    :data :ph2
    :print-function 'capitalize-if-symbol
    :enabled nil)

   (frz
    capi:check-button
    :data :frz
    :print-function 'capitalize-if-symbol
    :enabled nil)

   (nano
    capi:check-button
    :data :nano
    :print-function 'capitalize-if-symbol
    :enabled nil)

   (ale
    capi:check-button
    :data :ale
    :print-function 'capitalize-if-symbol
    :enabled nil)

   (rd
    capi:check-button
    :data :rd
    :print-function 'capitalize-if-symbol
    :enabled nil)

   (wr
    capi:check-button
    :data :wr
    :print-function 'capitalize-if-symbol
    :enabled nil)

   (cdr
    capi:check-button
    :data :cdr
    :print-function 'capitalize-if-symbol
    :enabled nil)

   (rdi
    capi:check-button
    :data :rdi
    :print-function 'capitalize-if-symbol
    :enabled nil)

   (int-rq
    capi:check-button
    :data :int-rq
    :print-function 'capitalize-if-symbol
    :enabled nil)

   (gcr
    capi:check-button
    :data :gcr
    :print-function 'capitalize-if-symbol
    :enabled nil)

   (rst
    capi:check-button
    :data :rst
    :print-function 'capitalize-if-symbol
    :enabled nil)

   (rd-state
    capi:check-button
    :data :rd-state
    :print-function 'capitalize-if-symbol
    :enabled nil)

   (ld-state
    capi:check-button
    :data :ld-state
    :print-function 'capitalize-if-symbol
    :enabled nil)

   (push-button-panel
    capi:push-button-panel
    :items '(:Step :Run :Run-until :Stop :Freeze :RD-State :LD-State :INT-RQ :u-Step :reset :dso :diagnostics :debug)
    :print-function 'capitalize-if-symbol
    :callback-type :data-interface
    :selection-callback 's79-button-selection-callback
    #|| :alternative-action-callback 's79-button-alternate-callback ||#
    :layout-args '(:x-uniform-size-p t))
    )
  
  (:layouts
   (indicator-panel
    capi:row-layout
    '(ph1 ph2 frz nano ale rd wr cdr rdi int-rq gcr rst rd-state ld-state))
   (internal-status
    capi:row-layout
    '(ucode-general-state ncode-general-state))
   (main-layout
    capi:column-layout
    '(register-grid-widget
      register-desc-pane
      address-data-panel
      internal-status
      ucode-status-widget 
      ncode-status-widget 
      indicator-panel
      push-button-panel)))
  (:default-initargs
   :layout 'main-layout
   :title "Scheme-79 Machine Console"))

(defun capitalize-if-symbol (x)
  (string-capitalize
   (if (symbolp x)
       (symbol-name x)
     x)))

;; status updates - blink the lights for various conditions

;; ph1, ph2 (two phase clock, non-overlapping); during ph1 data
;;   manipulated per current microcode state and computes new microcode
;;   state specified by MICRO and NANO PLAs. chip-bus valid by the END
;;   of ph1. External data being read should be valid soon after the
;;   beginning of ph1 so any microcode branch will have time to
;;   propagate through both PLAs. During ph2 the chip transitions to
;;   the new state and the control outputs change accordingly.


(add-initialization "console ph1 rising"
                    '(when *console*
                      (pushnew :ph1 (status *console*))
                      (setf (capi:button-selected (slot-value *console* 'ph1)) t))
                    ()
                    '*ph1-rising-list*)

(add-initialization "console ph2 rising"
                    '(when *console*
                      (pushnew :ph2 (status *console*))
                      (setf (capi:button-selected (slot-value *console* 'ph2)) t))
                    ()
                    '*ph2-rising-list*)

(add-initialization "console ph1 falling"
                    '(when *console*
                      (setf (status *console*) (delete :ph1 (status *console*)))
                      (setf (capi:button-selected (slot-value *console* 'ph1)) nil)
                      )
                    ()
                    '*ph1-falling-list*)

(add-initialization "console ph2 falling"
                    '(when *console*
                      (setf (status *console*) (delete :ph2 (status *console*)))
                      (setf (capi:button-selected (slot-value *console* 'ph2)) nil)
                      )
                    ()
                    '*ph2-falling-list*)

(add-initialization "console updates"
                    '(when *console*
                      (update-data-status))
                   ()
                   '*all-ph-list*)

;; sense lines are updated on ph2 falling

(defun translate-pad-to-indicator (pad-name)
  (case pad-name
    ('microlisp-shared:*freeze*
     :frz)
    ('microlisp-shared:*run-nano*
     :nano)
    ('microlisp-shared:*read-state*
     :rd-state)
    ('microlisp-shared:*load-state*
     :ld-state)
    ('microlisp-shared:*reset*
     :rst)
    ('microlisp-shared:*interrupt-request*
     :int-rq)
    ('microlisp-shared:*ale*
     :ale)
    ('microlisp-shared:*read*
     :rd)
    ('microlisp-shared:*write*
     :wr)
    ('microlisp-shared:*cdr*
     :cdr)
    ('microlisp-shared:*read-interrupt*
     :rdi)
    ('microlisp-shared:*gc-needed*
     :gcr)))

(defun check-pad (name)
  ;; don't go through test-pad since we want to see what the current pad is NOW
  (let ((indicator (translate-pad-to-indicator name))
        (pad-logical-value (eql (bit (symbol-value name) 0) 1)))
    (if pad-logical-value
      (pushnew indicator (status *console*))
      (setf (status *console*)
            (delete indicator (status *console*))))
    pad-logical-value))

(add-initialization "update-pad settings"
                    '(when *console*
                      (check-pad 'microlisp-shared:*freeze*)
                      (check-pad 'microlisp-shared:*run-nano*)
                      (check-pad 'microlisp-shared:*ale*)
                      (check-pad 'microlisp-shared:*read*)
                      (check-pad 'microlisp-shared:*write*)
                      (check-pad 'microlisp-shared:*cdr*)
                      (check-pad 'microlisp-shared:*read-interrupt*)
                      (check-pad 'microlisp-shared:*interrupt-request*)
                      (check-pad 'microlisp-shared:*gc-needed*)
                      (check-pad 'microlisp-shared:*reset*)
                      (check-pad 'microlisp-shared:*read-state*)
                      (check-pad 'microlisp-shared:*load-state*)
                      ;; force redraw
                      (redraw-indicators))
                    ()
                    '*all-ph-list*)     ; this is going to be expensive if 8x per clock, so
                                        ; we might want to make this conditional on pressing
                                        ; ustep, step or run?? On the other hand, that's why
                                        ; those lights blinked so much on old machines :-)
 
