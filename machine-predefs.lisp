(in-package :scheme-mach)

(scheme-79:scheme-79-version-reporter "Scheme Machine Predefs" 0 4 2
                                      "Time-stamp: <2022-05-02 16:43:04 gorbag>"
                                      "define defubus since vhdl is different than register")

;; 0.4.2   5/ 2/22 define defubus (currently just alias for defureg) since vhdl for bus is
;;                    different than register!

;; 0.4.1   4/12/22 move get-sense-wire-encoding to fpga-support library

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.4.0   3/18/22 snapping a line: 0.4 release of scheme-79 supports test-0 thru test-3. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.3.6   3/15/22 add optional argument to defureg to force the creation of a
;;                    type field for presentation purposes by the console.

;; 0.3.5   2/ 9/22 way too many things (fns, variables) with "line" in their name
;;                    and it's ambiguous.  Splitting so "line" refers to,
;;                    e.g. an output (log) line, "expression" refers to a
;;                    'line' of code (single expression in nano or microcode
;;                    land typically, and because we used (READ) it wasn't
;;                    confined to a single input line anyway) and "wire" to
;;                    refer to, e.g., a control or sense 'line' on a register.

;; 0.3.4   1/24/22 add support for from-decremented-field and
;;                    from-decremented-displacement

;; 0.3.3   1/18/22 cleanup obsolete code: removing special treatment of
;;                    registers which required multiple control wires
;;                    for TO as new covering set computation deals
;;                    with it.

;; 0.3.2   1/14/22 flip order of symbols in nanocontrol constants for
;;                     consistancy with AIM
;;                 fix debug message for increment/decrement fields 
;;                     to show value being placed on the bus

;; 0.3.1   1/13/22 update alist of control and sense wires in defureg

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.19  1/10/22 move defchip-pad, defchip-pads etc. to support/clocked/pads.lisp

;; 0.1.18  1/ 6/22 move *special-registers* to project defs, but continue 
;;                     to set it here
;;                 use (new) property accessor fns

;; 0.1.17 12/15/21 use *project-machine-pkg*

;; 0.1.16 12/14/21 move execute-during-clock,
;;                     execute-once-during-clock and
;;                     execute-now-or-during-clock to support

;; 0.1.15 12/13/21 simplify export code to call export-ulisp-symbol instead

;; 0.1.14 12/10/21 import/export register names, etc. to *ulang-pkg*,
;;                     and export from *scheme-mach* as well

;; 0.1.13 10/25/21 handle from-type

;; 0.1.12 10/20/21 defchip-special-reg to define internal
;;                     non-programmer-model registers for synchronous
;;                     processing on FPGA.
;;                 increment *nanocontrol-wire-next-initial-value* to
;;                     account for new from*-type constant

;; 0.1.11 10/15/21 use *scheme-mach*

;; 0.1.10 10/ 1/21 update *nanocontrol-wire-next-initial-value* to
;;                   account for supporting from-const

;; 0.1.9   9/30/21 make sure defureg exports sense and control wire names

;; 0.1.8   9/23/21 add support for not-mark-bit
;;                   make sure we set/clear mark and pointer bit depending on the (new) control wires

;; 0.1.7   9/16/21 Export latch-name if created, 
;;                 Ignore clear-pad on an already cleared latched pad

;; 0.1.6   9/ 7/21 Aesthetics

;; 0.1.5   9/ 4/21 explicitly reset fill-pointer on microcontrol-array

;; 0.1.4   9/ 1/21 sense-wire-real-name: handle list of registers (car
;;                    of list register is used for the sense-wire
;;                    determination - this is because some defupreds
;;                    rely on multiple registers such as
;;                    &scan-up=scan-down?

;; 0.1.3   8/31/21 added cases for sense wires type=type and type=pointer
;;                    which make more sense than type-not-pointer as the
;;                    sense wire. NB: type=pointer-bus is in the paper!!

;; 0.1.2   8/26/21 register-p

;; 0.1.1   8/23/21 cond vs. cl:cond

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.43     8-18-21 clear-annotations clear-microcontrol-array - getting more serious about
;;                       being able to run multiple tests

;; 0.0.42     8-17-21 implement sense allocation (never linked in to defufn)
;;                    use note-if

;; 0.0.41     8-16-21 from-to*

;; 0.0.40     8-13-21 add debug-flag for dataflow which puts the register 
;;                       assignments into the log (particularly useful when
;;                       we fast-forward or "run" a test instead of single
;;                       stepping)

;; 0.0.39     8-11-21 set-pad now deletes an initialization for clear-pad
;;                       that is scheduled for the current clock
;;                       phase.  This can happen when an automatic
;;                       clear has been scheduled from the prior tick
;;                       (typically lasting a whole tick, for example
;;                       *read*. We can get two *read*s in a row
;;                       without an ALE in between when we are
;;                       grabbing both the CAR and CDR of a memory
;;                       word (see paper).

;; 0.0.38     8- 7-21 add *microcontrol-annotations* to keep original
;;                       microinstruction during compilation

;; 0.0.37     8- 2-21 since we don't have separate increment and decrement
;;                       registers (as the chip did), we add a
;;                       normal-run-p check to the FROM controls so
;;                       they are evaluated only the first time in a
;;                       clock cycle to prevent multiple increments or
;;                       decrements. Adding the additional registers
;;                       would be more true to the original chip and
;;                       needed for synthesis! (Adding a TODO ;-)

;; 0.0.36     7-31-21 put sense inits on their own list and schedule them
;;                       as a group in machine-nano. This makes them work
;;                       like the control inits.

;; 0.0.35     7-27-21 execute-once-during-clock didn't delete the
;;                       initialization for deferral (oops)
;;                       
;; 0.0.34     7-26-21 don't add bus to all-bus-names if it's an alias

;; 0.0.33     7-20-21 clear-pad on latched pad should check for one less
;;                       than valid-for to determine if OK to clear
;;                       (last phase of minimum interval is OK to
;;                       clear on since we're already in it)

;; 0.0.32     7-19-21 move register copy to machine-nano.lisp since that's
;;                       the related timing

;; 0.0.31     7- 6-21 move init-list-name to clock.lisp

;; 0.0.30     6-26-21 Consolodate *debug-pad-timing* into scheme-79-defs
;;                       along with other debug flags

;; 0.0.29     6-17-21 add total-clock-phases var so we don't hardcode in '8'
;;                       everywhere

;; 0.0.28     6- 5-21 add defchip-pad names to *all-pad-names* for use by
;;                       DSO, defchip-pads names to *all-bus-names*

;; 0.0.27     5-28-21 new phase-equal fn fix set-pad to check we are in
;;                      the assert-during clock not the valid-for
;;                      period (which is for testing). We will have to
;;                      add a second period to allow sloppy
;;                      assertions, but the current code allowed the
;;                      assert and clear to potentially happen during
;;                      same phase.

;; 0.0.26     4-21-21 move register control execution to :ph1-high so it's
;;                      after the nanocontroller runs to set the
;;                      controls (may want to redo this so everything
;;                      is triggered on a rising cycle, or presume
;;                      that when we get to the hardware
;;                      implementation we divide down the clock)

;; 0.0.25     4-20-21 fix setting up register accessor and setter - remove
;;                      the extra quote

;; 0.0.24     3-29-21 fix in-phase-interval-p to use the raw input to
;;                      check for :any or :all argument force package
;;                      (scheme-mach) in register-flag-accessor use
;;                      validity (valid-for) when calling
;;                      in-phase-interval-p on test-pad

;; 0.0.23     3-27-21 set-pad use in-phase-interval-p

;; 0.0.22     3-18-21 some cases were missed for (execution-done) clause

;; 0.0.21     3-11-21 establish initial size for *microcontrol-array* so
;;                      we can initialize the jump table before we
;;                      know the offsets.

;; 0.0.20     3- 3-21 slight code simplification: introduce
;;                      execute-during-clock macros

;; 0.0.19     3- 2-21 create a global for the nanocontrol and microcontrol
;;                      stored as a bitvector (suitable for use on the
;;                      FPGA!)

;; 0.0.18     2-26-21 strip-register-name (consolodate prior code)

;; 0.0.17     2-22-21 encode-sense-wire etc.

;; 0.0.16     2-19-21 cl:progn (differentiate from our ucode version)

;; 0.0.15     2-17-21 export sense-wire names suitable for the uPLA assembler

;; 0.0.14     2- 6-21 add microcontrol-array

;; 0.0.13     2- 3-21 fix export for pad (wrap with eval-when)

;; 0.0.12     2- 1-21 pad and control wire specs to support nanocontroller

;; 0.0.11     1-25-21 pad support - test/clear/set pads

;; 0.0.10     1-23-21 flip TNP logic - had it right before ;-)

;; 0.0.9      1-22-21 move some nanocontrol globals here so defined before use

;; 0.0.8      1-20-21 extend defchip-pad(s) to include clock validity
;;                      information (i.e. when to assert and when to
;;                      test for inputs). This is to prepare for
;;                      extending the macro with additional
;;                      functionality to request state changes and
;;                      having them put on the correct queue to
;;                      synchronize those changes with the clock
;;                      (during simulation).

;; 0.0.7      1-17-21 defchip-reg now sets up a property list for (newly)
;;                      defined registers that can be used to gather
;;                      statistics when compiling the microcode.

;; 0.0.6      1-15-21 extend defureg to define bit controls for registers
;;                      used by the nanocontrol store also add
;;                      :latched-output type to defchip-pad
;;                      (specifically for *gc-needed*)

;; 0.0.5      1-13-21 Add macros for defining pads on the chip (h/w
;;                      interface)

;; 0.0.4      1-10-21 fix bus field operations to limit to the field being
;;                      copied to, also mark-bit and type-not-pointer
;;                      return 0/1 not t/nil so fix logic

;; 0.0.3      1- 9-21 separate 'from' and 'to' register initializations

;; 0.0.2      1- 8-21 moved register-flag-accessor here to help with
;;                      recompilations

;; 0.0.1      1- 7-21 add initializations for sense wires (end of ph2 for
;;                      now)

;; 0.0.0     12-31-20 moved from machine-defs to make sure these are
;;                      defined and loaded before use there

;; Closer to a hardware simulator corresponding with AIM 559
;; based on the published microcode and description

;; here we implement the various described mechanisms on the chip
;; which we are simulating.

;; we could read in the microcode and get the **machine-registers**
;; from there, but I think that's really more about mapping than
;; defining... similarly for defreg being a bridge between the actual
;; hardware and what the microcode can do. One issue is that the
;; microcode version may actually be impoverished - it doesn't say how
;; wide the registers are, for instance!

;; in this file I'm defining support functions for the operations in
;; sim-machine-external and sim-machine-internal. So most validity
;; checking isn't going to be done here, but there (if not prechecked
;; by the validator?)

;; pointer is 32 bits with 3 fields: 24 bit data, 7 bit type, and 1
;; bit (in-use mark) used by storage allocator.

;; registers are all 32 bits at this point

;; NB: the sbit/bit operation puts the high order bit (the one with
;;     the largest offset) into the lowest bit based on
;;     integer->bit-vector.  SO we try to be consistent here:
;;
;; (s)BIT: 0     1    2..7   8     ..  19  20-31
;;       Mark  ~ptr   TYPE   DISPLACEMENT  FRAME
;;                           ----  ADDRESS  ----

(defmacro mark-bit (x)
  `(sbit ,x 0))

(defmacro pointer-bit (x)
  `(sbit ,x 1))

(defvar *control-wires-needing-type-field* '(to-type))

;; address field is also the "data" field - consists of displacement
;; and frame together.
(defvar *control-wires-needing-address-field* '(to-address from-incremented from-decremented))

(defvar *control-wires-needing-displacement-field* '(to-displacement from-decremented-displacement))

(defvar *control-wires-needing-frame-field* '(to-frame from-decremented-frame))


(defun make-nanocontrol-wire-symbol (register-name-symbol control-name-symbol)
  ;; the control name may include the field, e.g. to-type
  (intern (format nil "+RR-~A-~A+"
                  (string control-name-symbol)
                  (strip-register-name register-name-symbol))))

(defparameter *nanocontrol-wire-next-initial-value* #o100
  "Because we reset it in machine-defs")

(defparameter *nanocontrol-wire-next* *nanocontrol-wire-next-initial-value*
  "#o1, #o2 and #o4 are reserved for from*, to* and from-to*")

(defvar *nanocontrol-to-spec* nil
  "Alist whose key is the nanocontrol constant and the cdr is a
key/value list of specifications (register, controls, etc.) to make it
simpler for the simulator to convert from a wire signal to a set of
data manipulations")


;; while we tie things to the clock (initialization lists), we don't
;; really have an electrical circuit that would simultaneously connect
;; the FROM registers to the TO registers via the bus. So we split the
;; initialization lists and schedule them in order.

(defvar *from-controls* nil)
(defvar *to-controls* nil)
(defvar *sense-controls* nil)

;; we also need to establish bits (wires) to be used by the
;; nanocontroller of the form +rr-<control-wire>-<register-name>+ so
;; when that particular bit in the nanocontrol array is set the right
;; thing can happen. (Note that will be offset into it's appropriate
;; field in the horizontal nanocontrol encoding). See machine-nano.lisp

(defvar *sense-wire-encoding* #o1)
(defvar *sense-wire-encoding-alist* nil)

(defun reset-sense-wire-encoding ()
  (setq *sense-wire-encoding-alist* nil)
  (setq *sense-wire-encoding* #o1))

(defun sense-wire-encoding-to-symbol (encoding)
  (car (rassoc encoding *sense-wire-encoding-alist* :test #'=)))

(defun sense-wire-real-name (sense-wire-name register)
  ;; some will get back multiple registers from the defupred
  (let ((target-register (cl:if (consp register) (car register) register))) 
    (intern (format nil "~A-~A" 
                    (string sense-wire-name) 
                    (strip-register-name target-register))
            *project-machine-pkg*)))
    
(defun encode-sense-wire (sense-wire-name register)
  ;; since sense wires are associated with registers, we adjust the name to include the register. 
  (let ((sense-wire-rn (sense-wire-real-name sense-wire-name register)))
    (export-ulisp-symbol sense-wire-rn)
    (format *error-output* "~&; Setting up ~s~%" sense-wire-rn) ; happens when we compile

    (update-alist sense-wire-rn *sense-wire-encoding* *sense-wire-encoding-alist*)
    (setf (sense-wire-encoding sense-wire-rn) *sense-wire-encoding*)
    (setf (sense-wire-name sense-wire-rn) sense-wire-name)
    (setf (sense-wire-register sense-wire-rn) register)
    (setq *sense-wire-encoding* (ash *sense-wire-encoding* 1)))) ; so we can specify multiple and OR them

(defmacro defureg (register-name control-wires sense-wires &optional force-type-field-p)
  "force-type-field-p creates a type field even if unneeded to allow for presentation in the console if desired"
  `(cl:progn
     (eval-when (:load-toplevel :execute)
       (setf (valid-control-wires ',register-name) ',control-wires)
       (declare-register-control-wires ',register-name ',control-wires) ; put into alist as well
       (setf (valid-sense-wires ',register-name) ',sense-wires)
       (declare-register-sense-wires ',register-name ',sense-wires) ; put into alist as well
       (setf (register-flags ',register-name) 0))
     ,@(when (or force-type-field-p (intersection control-wires *control-wires-needing-type-field*))
        `((defvar ,(make-register-field-symbol register-name 'type)
            (make-type-field ,register-name))
          (eval-when (:load-toplevel :execute)
            (export-ulisp-symbol ',(make-register-field-symbol register-name 'type)))))
     ,@(when (intersection control-wires *control-wires-needing-address-field*)
         `((defvar ,(make-register-field-symbol register-name 'address)
             (make-data-field ,register-name))
           (eval-when (:load-toplevel :execute)
             (export-ulisp-symbol ',(make-register-field-symbol register-name 'address)))))
     ,@(when (intersection control-wires *control-wires-needing-displacement-field*)
         `((defvar ,(make-register-field-symbol register-name 'displacement)
             (make-displacement-field ,register-name))
           (eval-when (:load-toplevel :execute)
             (export-ulisp-symbol ',(make-register-field-symbol register-name 'displacement)))))
     ,@(when (intersection control-wires *control-wires-needing-frame-field*)
         `((defvar ,(make-register-field-symbol register-name 'frame)
             (make-frame-field ,register-name))
           (eval-when (:load-toplevel :execute)
             (export-ulisp-symbol ',(make-register-field-symbol register-name 'frame)))))
     ;; define control wire constants for nanocontrol array
     ,@(mapcar #'(lambda (control-wire)
                   (prog1 `(cl:progn
                             (defconstant ,(make-nanocontrol-wire-symbol register-name control-wire)
                               ,*nanocontrol-wire-next*)
                             (update-alist
                              ,*nanocontrol-wire-next*
                              '((:register ,register-name)
                                (:control ,control-wire)
                                (:accessor ,(register-flag-accessor control-wire))
                                (:setter (lambda (x)
                                           (setf (,(register-flag-accessor control-wire)
                                                  ',register-name)
                                                 x))))
                              *nanocontrol-to-spec*)) ; create a spec we can use when running the nanocontroller
                     (setq *nanocontrol-wire-next* (ash *nanocontrol-wire-next* 1))))
               control-wires)
     (mapc #'export-ulisp-symbol ',control-wires)

     ;; define sense wire constants for the nanocontrol array
     ,@(mapcar #'(lambda (sense-wire) ; e.g. address=bus
                   `(encode-sense-wire ',sense-wire ',register-name))
               sense-wires)
     (mapc #'export-ulisp-symbol ',sense-wires)

     ;; next are initializations for the control wires. These should
     ;; be run, I think, as "ph1" is raised, unless "freeze" has been
     ;; asserted.
     ,@(when control-wires
         `((cl:progn
             (add-initialization
              (format nil "~A control wires TO" (string ',register-name))
              '(cl:progn
                ,@(mapcar
                   #'(lambda (control)
                       (let ((accessor (register-flag-accessor control)))
                         (case control
                           (to
                            `(when (,accessor ',register-name)
                               (note-if *debug-dataflow*
                                        "register ~s filled from bus: ~s" ',register-name *bus*)
                               (copy-field *bus* ,register-name)))
                           (to-type
                            (let ((field (make-register-field-symbol register-name 'type)))
                              `(when (,accessor ',register-name)
                                 (note-if *debug-dataflow*
                                          "register ~s type filled from bus: ~s" ',register-name *bus-type*)
                                 (copy-field *bus-type* ,field))))
                           (to-address
                            (let ((field (make-register-field-symbol register-name 'address)))
                              `(when (,accessor ',register-name)
                                 (note-if *debug-dataflow*
                                          "register ~s address filled from bus: ~s" ',register-name *bus-address*)
                                 (copy-field *bus-address* ,field))))
                           (to-displacement
                            (let ((field (make-register-field-symbol register-name 'displacement)))
                              `(when (,accessor ',register-name)
                                 (note-if *debug-dataflow*
                                          "register ~s displacement filled from bus: ~s" ',register-name *bus-displacement*)
                                 (copy-field *bus-displacement* ,field))))
                           (to-frame
                            (let ((field (make-register-field-symbol register-name 'frame)))
                              `(when (,accessor ',register-name)
                                 (note-if *debug-dataflow*
                                          "register ~s frame filled from bus: ~s" ',register-name *bus-frame*)
                                 (copy-field *bus-frame* ,field))))
                           (mark!
                            `(when (,accessor ',register-name)
                               (setf (mark-bit ,register-name) 1)))
                           (unmark!
                            `(when (,accessor ',register-name)
                               (setf (mark-bit ,register-name) 0)))
                           (pointer!
                            `(when (,accessor ',register-name)
                               (setf (pointer-bit ,register-name) 0))) ; type-not-pointer
                           (type!
                            `(when (,accessor ',register-name)
                               (setf (pointer-bit ,register-name) 1)))
                           ;; not TO wires
                           ((from from-decremented from-incremented
                            from-type from-decremented-displacement
                            from-decremented-frame)
                            nil) ; ok to ignore - we pick up the from
                                 ; wires on a different init list
                           (t
                            (note-if *debug-dataflow*
                                     "register ~s control wire ~s ignored" register-name control)))))
                   control-wires))
              ()
              '*to-controls*)
             (add-initialization
              (format nil "~A controls wires FROM" (string ',register-name))
              '(let ((normal-run-p (phase-equal *symbolic-clock-phase* *run-register-controls*)))
                ;; normal-run-p lets us distinguish between the
                ;; programmed run of the controls and repeats
                ;; to fake electrical conductivity (which would
                ;; do multiple increments and decrements since
                ;; we don't use an internal increment or
                ;; decrement register for this simulation,
                ;; though the original chip did so we may want
                ;; to add those! 8/2/21 BWM
                ,@(mapcar #'(lambda (control)
                              (let ((accessor (register-flag-accessor control)))
                                (case control
                                  (from
                                   `(when (,accessor ',register-name)
                                      (note-if *debug-dataflow*
                                               "register ~s placed on bus: ~s"
                                               ',register-name ,register-name)
                                      (copy-field ,register-name *bus*)))
                                  (from-decremented
                                   `(when (and (,accessor ',register-name)
                                               normal-run-p)
                                      (note-if *debug-dataflow*
                                               "decremented register ~s placed on bus: ~s"
                                               ',register-name (decrement-field ,register-name))
                                      (copy-field (decrement-field ,register-name) *bus*)))
                                  (from-decremented-frame
                                   (let ((field (make-register-field-symbol register-name 'frame)))
                                     `(when (and (,accessor ',register-name)
                                                 normal-run-p)
                                        (note-if *debug-dataflow*
                                                 "decremented register frame of ~s placed on bus: ~s"
                                                 ',register-name (decrement-field ,field))
                                        (copy-field (decrement-field ,field) *bus*))))
                                   (from-decremented-displacement
                                    (let ((field (make-register-field-symbol register-name 'displacement)))
                                      `(when (and (,accessor ',register-name)
                                                  normal-run-p)
                                         (note-if *debug-dataflow*
                                                  "decremented register displacment of ~s placed on bus: ~s"
                                                  ',register-name (decrement-field ,field))
                                         (copy-field (decrement-field ,field) *bus*))))
                                  (from-incremented
                                   `(when (and (,accessor ',register-name)
                                               normal-run-p)
                                      (note-if *debug-dataflow*
                                               "incremented register ~s placed on bus: ~s"
                                               ',register-name (increment-field ,register-name))
                                      (copy-field (increment-field ,register-name) *bus*)))
                                  (from-type
                                   (let ((field (make-register-field-symbol register-name 'type)))
                                     `(when (and (,accessor ',register-name)
                                                 normal-run-p)
                                        (note-if *debug-dataflow*
                                                 "register ~s type field placed on bus: ~s"
                                                 ',register-name ,field)
                                        (copy-field ,field *bus*))))
                                  )))
                   control-wires))
              ()
              '*from-controls*))))
                                    
     ;; sense wires need initializations to set them correctly. 
     ,@(when sense-wires
         `((add-initialization (format nil "~A sense wires" (string ',register-name))
              '(cl:progn ,@(mapcar #'(lambda (sense)
                            (let ((accessor (register-flag-accessor sense)))
                              (ecase sense
                                (mark-bit
                                 `(setf (,accessor ',register-name)
                                        (not (zerop (mark-bit ,register-name)))))
                                ;; added this since we should have inverters
                                (not-mark-bit
                                 `(setf (,accessor ',register-name)
                                        (zerop (mark-bit ,register-name))))
                                (type-not-pointer
                                 `(setf (,accessor ',register-name)
                                        ; true means it's a type and not a pointer - the bit is 1 if not a pointer.
                                        (not (zerop (pointer-bit ,register-name))))) 
                                ;; I inferred the following two from
                                ;; the paper since type-not-pointer
                                ;; doesn't yeild the negation
                                (type=type
                                 `(setf (,accessor ',register-name)
                                        (not (zerop (pointer-bit ,register-name)))))
                                (type=pointer
                                 `(setf (,accessor ',register-name)
                                        (zerop (pointer-bit ,register-name))))
                                (frame=0
                                 `(setf (,accessor ',register-name)
                                        (bv-zerop ,(make-register-field-symbol register-name 'frame))))
                                (displacement=0
                                 `(setf (,accessor ',register-name)
                                        (bv-zerop ,(make-register-field-symbol register-name 'displacement))))
                                (address=0
                                 `(setf (,accessor ',register-name)
                                        (bv-zerop ,(make-register-field-symbol register-name 'address))))
                                (address=bus
                                 `(setf (,accessor ',register-name)
                                        (equal ,(make-register-field-symbol register-name 'address)
                                               ,(make-register-field-symbol '*bus* 'address))))
                                (type=bus
                                 `(setf (,accessor ',register-name)
                                        (equal ,(make-register-field-symbol register-name 'type)
                                               ,(make-register-field-symbol '*bus* 'type))))
                                (=bus
                                 `(setf (,accessor ',register-name)
                                        (and (equal ,(make-register-field-symbol register-name 'type)
                                                    ,(make-register-field-symbol '*bus* 'type))
                                             (equal ,(make-register-field-symbol register-name 'address)
                                                    ,(make-register-field-symbol '*bus* 'address))))))))
                            sense-wires))
              ()
              '*sense-controls*)))))

(defmacro defubus (register-name control-wires sense-wires)
  "special version of defureg for defining a bus"
  ;; at the moment, it's just like defureg
  `(defureg ,register-name ,control-wires ,sense-wires))
