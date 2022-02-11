(in-package :s79-console)

(scheme-79:scheme-79-version-reporter "Scheme Mech Console Defs" 0 3 2
                                      "Time-stamp: <2022-02-09 12:46:21 gorbag>"
                                      "line disambiguation")

;; 0.3.2   2/ 9/22 way too many things (fns, variables) with "line" in their name
;;                    and it's ambiguous.  Splitting so "line" refers to,
;;                    e.g. an output (log) line, "expression" refers to a
;;                    'line' of code (single expression in nano or microcode
;;                    land typically, and because we used (READ) it wasn't
;;                    confined to a single input line anyway) and "wire" to
;;                    refer to, e.g., a control or sense 'line' on a register.

;; 0.3.1   1/24/22 add mask-interrupt button

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.5  1/ 6/22 use property accessor fns

;; 0.1.4 12/15/21 use existing register-flag-accessor function to get
;;                   the interned function name for the register-sense-fn

;; 0.1.3 10/26/21 add size parameter to register meta-data and use for
;;                   *micro-pc*; move defn for
;;                   sense-and-control-total-height so we can use the
;;                   length of *s79-register-metadata*
;;                use *control-wires* and *sense-wires* to help automate 
;;                   updates to them

;; 0.1.2 10/25/21 make *micro-pc* display as a register too, add
;;                   from-type as a sense-and-control wire.
;;                Rename *sense-and-control-total-wires* as
;;                   -total-columns* to avoid confusion

;; 0.1.1  9/24/21 Update *sense-and-control-total-wires* for the
;;                   additional wires added (automate? TBD)

;; 0.1.0  9/13/21 Split from s79-console for compilation issues.

(defvar *console* nil)

(defparameter *header-row-font* (gp:make-font-description :family "courier" :weight :medium :slant :roman :size 14))

(defparameter *widget-font* (gp:make-font-description :family "courier" :weight :medium :slant :roman :size 12))

(defparameter *register-name-column-width* 250)

(defparameter *register-value-column-width* 110)

(defparameter *sense-and-control-column-width* 65)

(defparameter *sense-and-control-total-columns* 22) ; to, to-type, to-adr, etc.

(defparameter *sense-and-control-total-column-width*
  (* *sense-and-control-column-width* *sense-and-control-total-columns*)) ; multiply by number of sense and control wires displayed

(defparameter *sense-and-control-height* 30)

(defclass-x register-metadata ()
  ;; domain object for metadata about each register, used to fill in grid rows by register
  ((name :initarg :name
         :reader register-name
         :initform "anonymous register")
   (symbol :initarg :symbol ; if we evaluate the symbol we get the value of the register
           :reader register-symbol
           :initform nil)
   (size :initarg :size
         :reader register-size 
         :initform *register-size*) ; typically, but we can override
   (color :initarg :color
          :reader register-color ; used for presentation purposes only
          :initform :black) ; pick from ??
   ;; flags for the different control and sense wires that are enabled (note their value has to be retrieved elsewhere)
   (flags :initform 0 :accessor register-metadata-flags :type fixnum)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (eval `(defflags register-metadata ,@*control-wires* ,@*sense-wires*)))

(defparameter *ucode-offset-column-width* 150)

(defparameter *ucode-value-column-width* 250)

(defparameter *ucode-symbolic-column-width* 650)

(defparameter *ucode-ptr-column-width* 200)

(defparameter *ucode-comment-column-width* *ucode-symbolic-column-width*)

(defparameter *ucode-depth* 5
  "Number of microcode instructions to show")

(defparameter *ucode-height* 30)

(defparameter *ucode-total-height* (* (1+ *ucode-depth*) *ucode-height*))

;; for nanocode

(defparameter *ncode-offset-column-width* 105)

(defparameter *ncode-name-column-width* 360)

(defparameter *ncode-value-column-width* 150)

(defparameter *control-column-width* 450)

(defparameter *nano-next-column-width* 120)

(defparameter *control-total-column-width* (+ (* *control-column-width* 2) *nano-next-column-width*)
  "2 is number of control fields")

(defparameter *ncode-depth* 3
  "Number of nanocode instructions to show")

(defparameter *ncode-total-height* (* (1+ *ncode-depth*) *ucode-height*))

(defparameter *s79-register-metadata*
  (list (make-instance 'register-metadata
                       :name "Bus"
                       :symbol '*bus*)
        (make-instance 'register-metadata
                       :name "Memtop"
                       :symbol '*memtop*)
        (make-instance 'register-metadata
                       :name "Newcell / scan-up"
                       :symbol '*newcell*)
        (make-instance 'register-metadata
                       :name "Exp / scan-down"
                       :symbol '*exp*)
        (make-instance 'register-metadata
                       :name "Val / stack-top / rel-tem-2"
                       :symbol '*val*)
        (make-instance 'register-metadata
                       :name "Retpc-Count-Mark"
                       :symbol '*retpc-count-mark*)
        (make-instance 'register-metadata
                       :name "Args / Leader / rel-tem-1"
                       :symbol '*args*)
        (make-instance 'register-metadata
                       :name "Stack"
                       :symbol '*stack*)
        (make-instance 'register-metadata
                       :name "Display / node-pointer"
                       :symbol '*display*)
        (make-instance 'register-metadata
                       :name "Intermediate-Argument"
                       :symbol '*intermediate-argument*)
        ;; not sure we need this one... I guess to see the From control
        (make-instance 'register-metadata
                       :name "Nil (Pseudo-Reg)"
                       :symbol '*nil*)
        (make-instance 'register-metadata
                       :name "Address (Pseudo-Reg)"
                       :symbol '*address*)
        (make-instance 'register-metadata
                       :name "Memory (Pseudo-Reg)"
                       :symbol '*memory*)
        (make-instance 'register-metadata
                       :name "Interrupt (Pseudo-Reg)"
                       :symbol '*interrupt*)
        (make-instance 'register-metadata
                       :name "Micro PC"
                       :symbol '*micro-pc*
                       :size *micro-pc-size*)
        ))

(defparameter *numregs-to-display* (length *s79-register-metadata*)) ; number of registers +1 (see *s79-register-metadata*)

(defparameter *sense-and-control-total-height* (* *numregs-to-display* (1+ *sense-and-control-height*))) 

(defun wire-name-to-fn-symbol (wire-name-symbol)
  (intern (format nil "REGISTER-~A-FUNCTION" wire-name-symbol) (find-package :s79-console)))

;; each flag has it's own reader/writer, such as 
;; register-metadata-to-type-p, and the actual value in the simulation is register-to-p

(defmacro register-check-box-fn (wire-name-symbol)
  "generate a function for the :check-box-function below depending on the control wire we are interested in"
  (let ((fn-symbol (wire-name-to-fn-symbol wire-name-symbol)) ;new
        (accessor (register-flag-accessor wire-name-symbol))) ; should have been defined already
    `(defun ,fn-symbol (register-metadata &optional (value nil valp))
       (let ((enabled-p (member ',wire-name-symbol (valid-control-wires (register-symbol register-metadata)))))
         (if enabled-p
             (if valp
                 (setf (,accessor (register-symbol register-metadata)) value)
                 (,accessor (register-symbol register-metadata)))
             :disabled)))))

;; similarly we want something for sense wires, but no need to have a
;; setter (as that is an effect of stepping the machine and having the
;; state determined). Rather than just using the flag function
;; directly, we want to detect if the sense wire is present, so set up
;; a similar macro as register-check-box-fn for sense wires

(defmacro register-sense-fn (wire-name-symbol)
  "generate a function for the :check-box-function below depending on the control wire we are interested in"
  (let ((fn-symbol (wire-name-to-fn-symbol wire-name-symbol)) ;new
        (accessor (register-flag-accessor wire-name-symbol))) ; should have been defined already
    `(defun ,fn-symbol (register-metadata)
       (let ((enabled-p (member ',wire-name-symbol (valid-sense-wires (register-symbol register-metadata)))))
         (if enabled-p
             (,accessor (register-symbol register-metadata))
             :disabled)))))

(defun ignore-focus (&rest args)
  (declare (ignore args))
  (values))

(defvar *all-indicators* '(:frz :nano :rst :ale :rd :wr :cdr :int-rq :rdi :m-int :gcr :rd-state :ld-state)
  "used for redraw")


