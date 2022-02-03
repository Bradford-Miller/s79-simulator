(in-package :scheme-mach)

(scheme-79:scheme-79-version-reporter "Scheme Machine Nano" 0 3 5
                                      "Time-stamp: <2022-01-25 14:44:22 gorbag>"
                                      "add support for setting and clearing interrupt mask")

;; 0.3.5   1/25/22 add support for from-decremented-frame-exp and
;;                     from-decremented-displacement-exp

;; 0.3.4   1/24/22 add support for setting and clearing interrupt mask

;; xxxxx   1/19/22 remove some TBDs in the comments (they were done already)

;; 0.3.3   1/18/22 cleanup obsolete code: removing special treatment of
;;                    registers which required multiple control lines
;;                    for TO as new covering set computation deals
;;                    with it.

;; 0.3.2   1/14/22 flip order of symbols in nanocontrol constants for
;;                    consistancy with AIM

;; 0.3.1   1/13/22 change references from internal-freeze to run-nano
;;                    for consistancy with AIM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.12  1/ 7/22 use new get-pad-defn-value function

;; 0.1.11  1/ 6/22 add methods for linking functions to support library

;; 0.1.10 12/15/21 use export-ulisp-symbol

;; 0.1.9  12/13/21 import/export nanoop names into *ulang-pkg* instead
;;                     of *scheme-mach*

;; 0.1.8  10/25/21 use pla-read instead of aref on plas

;; 0.1.7  10/21/21 nano-pc now a bit vector add 'to-micro-pc' into
;;                     parse-defnano table (really should make this
;;                     more automatic from the macros! (TBD)

;; 0.1.6  10/20/21 micro-pc now a bit vector

;; 0.1.5  10/ 5/21 since we don't have actual continuous update until a
;;                     latch operation, some actions have to
;;                     be rerun on phase changes or other events to
;;                     correctly reflect their state. In this case we
;;                     add a run-sense-controls immediately after
;;                     run-register-controls.

;; 0.1.4  10/ 1/21 add +rr-from-const*+ to distinguish from from-type-const field
;;                 Also extend defnano to allow forcing a nanocontrol to be added
;;                     even if an apparent duplicate

;; 0.1.3   9/ 7/21 if +rr-from-to*+ is set, use the FROM anaphor table
;;                    instead of the TO anaphor table to decode.
;;                    (support added to code generation, disassembly,
;;                    and a parameter set up in s79-nanocode)

;; 0.1.2   8/24/21 differentiate microcode 'cond' from cl:cond, if from
;;                  cl:if

;; 0.1.1   8/21/21 +rr-from-type-const*+ - handle type constants

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.32     8-17-21 use note-if

;; 0.0.31     8-16-21 from-to*

;; 0.0.30     8-11-21 cosmetic/clarify code

;; 0.0.29     8- 3-21 cosmetic

;; 0.0.28     7-31-21 schedule sense simulation explicitly here;
;;                      rerun control simulation before p2 so we capture any pad sync changes

;; 0.0.27     7-19-21 move register copy from machine-predefs.lisp since
;;                      that's the related timing; change to :ph2-high; use variable from clock-triggers.lisp

;; 0.0.26     7- 1-21 Better debug message for deferred actions

;; 0.0.25     6-26-21 Consolodate debug-flags to scheme-79-defs.lisp

;; 0.0.24     6-14-21 add additional debug messages to nanocontroller

;; 0.0.23     5-29-21 set-pad should set up clear-pad, not the
;;                         nanocontroller (that should ensure proper
;;                         minimum duration

;; 0.0.22     4-26-21 fix split - dangling variable reference

;; 0.0.21     4-21-21 split run-nanocontroller into two parts, so we can
;;                         run on rising and falling clock (at least
;;                         for now, seems bad practice in general)

;; 0.0.20     4-20-21 make sure we funcall the compiled setter fix
;;                         debug-nanocontroller print statement format
;;                         and add one for when the nano-pc changes

;; 0.0.19     3-29-21 exec-nano-line: compile the setter so it's
;;                         acceptable to funcall

;; 0.0.18     3-15-21 modify run-nanocontroller so only a single cycle is
;;                         run during :ph1-rising, then it waits (if
;;                         there is more) until next :ph1-rising

;; 0.0.17     3- 8-21 debug-nanocontroller

;; 0.0.16     3- 7-21 rename translate-*-anaphor functions to
;;                         *-code-to-anaphor to make clearer?

;; 0.0.15     3- 2-21   define finalize-nanocontrol-array
;; 0.0.14     2-27-21   check for register alias when looking up anaphor
;; 0.0.13     2-25-21   add retpc-count-mark forms to defnano
;; 0.0.12     2-24-21   export defnano symbols
;; 0.0.11     2-23-21   pad-conditional (pseudo pad)
;; 0.0.10     2-19-21   cl:progn (differentiate from our ucode version)
;; 0.0.9      2-16-21   finish adding register specs to parse-defnano
;; 0.0.8      2-14-21   run-nanocode - handle multi-nanocode ops
;; 0.0.7      2- 7-21   lookup from and to anaphors
;; 0.0.6      2- 1-21   first cut at nanocontroller simulator
;; 0.0.5      1-22-21   move defnano forms to own file (s79-nanocode.lisp)
;; 0.0.4      1-20-21   rewrite defnano to do most of the work at load-time
;; 0.0.3      1-19-21   Add first of defnano forms
;; 0.0.2      1-17-21   Set freeze (via pad) for multi-step nano instructions
;; 0.0.1      1-16-21   First cut defnano
;; 0.0.0      1-14-21   new

;; Closer to a hardware simulator corresponding with AIM 559
;; based on the published microcode and description

;; here we implement the various described mechanisms on the chip
;; which we are simulating.

;; here we specify the nanocontrol array and define
;; nanoinstructions. Note that the array is populated by the code in
;; ucode-compiler once we've interpreted the microcode and calculated
;; symbolic bit values for registers, etc.

;; Nano-words are selected by state information from the micro control
;; and any next state generated from the nano control. The micro
;; control specifies to and from registers (as bits per above). The
;; nano control generates pad and register controls (as well as the
;; next nano state). Note that freeze may be asserted for multi-word
;; nano sequences to prevent additional micro words from being
;; decoded. Example multiple word nano instructions are given in the
;; paper but are not exhaustive, and rely on microinstructions that
;; differ from the documented microcode (so may be either output from
;; the microword compiler based on that input, or are from a different
;; version of the same - their intent is to communicate the flavor of
;; thier implementation not document it for reprodution in the future
;; after all).

;; (defnano (do-car)
;;    ((from*) (ale))
;;    ((to*) (read)))

;; (defnano (do-cdr)
;;    ((from*) (ale))
;;    ((to*) (read cdr)))

;; (defnano (do-restore)
;;    (() (from-stack ale))
;;    ((to*) (read))
;;    (() (read cdr to-address-stack to-type-stack)))

;; Here the from* and to* forms are meant to be taken as pronouns that
;; refer to the values passed from the microcode, so "do-restore" can
;; be seen to ignore the from* value.

;;  To translate the above into what (I think) actually happens, the
;; first do-car instruction will take whatever is in the FROM register
;; and put it on the bus, and raise ALE (so it's transmitted to the
;; external memory as an address). The second instruction reads the
;; response from memory onto the bus and gates it into the TO
;; register. So our job is to make sure we do this gating during the
;; appropriate PH1 and PH2 (simulated) clock cycles, have the external
;; memory respond to it, etc. in the hardware emulation, but for the
;; work below it's just to capture our representations for registers
;; that will be used for both the micro and nano code, and then to
;; compile the "defnano" terms into the nanocontrol array generating a
;; symbol table for linking the microcode to nanocontrol array offsets
;; (states).

;; so first let's define the terms that can come inside of a defnano
;; form: from* and to* are anaphors for the from and to fields of the
;; current microcode instruction, while other things are operations
;; that trigger control lines to be set and then shifted out at the
;; right time based on the clock.  We'll incorporate the example,
;; above into our definitions.

;; from* - anaphor for whatever register is in the FROM field of the
;;         current microinstruction. It should be moved to the bus,
;;         so set the from control line for that register.

;; to*   - anaphor for whatever register is in the TO field of the
;;         current microinstruction. It should be filled from the bus,
;;         so set the to control line for that register.

;; ale   - put the current content of the bus into the *address*
;;         register, set *address-pads* at the right time, and set
;;         *ale* for a clock

;; read  - drop *ale* and raise *read*, move contents of the external
;;         *memory-pads* to *memory* and the bus

;; cdr   - set *cdr* in conjunction with either *read* or *write*
;;         (else should be clear)

;; from-stack
;; to-address-stack
;; to-type-stack
;;       - these all specify a control line and a particular register,
;;         i.e. a particular control line.

;; gc-needed
;;       - set *gc-needed* output and latch high

;; clear-gc
;;       - clear *gc-needed* output and latch low (called by anything
;;         setting mark as a gc has been triggered)

(defun make-nanocontrol-array-element (nanocontrol-tuple)
  "create a bitvector containing the nanocontrol array row (tuple ->bitvector)"
  (declare (ignore nanocontrol-tuple))
  (tbd))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; reset nanocontrols
  (setf (fill-pointer *nanocontrol-array*) 0)
  (setq *nanocontrol-symtab* nil))

;; generate nano-instructions in the form:

;;   <pad-controls>  <register-controls> <next-state>

;;  (which follows Figure 6 in AIM 559) where the pad definitions are
;;  in sim-machine-external.lisp, and the register definitions are in
;;  machine-defs.lisp.

;;  Because of the use of anaphor in the nanoinstructions and to
;;  reduce the number of nanoinstructions that need to be generated,
;;  we will have fake register numbers for those that refer to
;;  microcode fields. We need to create (invariant) register numbers
;;  to refer to these registers which can be later translated to a
;;  control MUX but here will just translate back to appropriate
;;  simulator code. We can't just use the codes the microcode
;;  determined, because those have been compressed based on usage (to
;;  limit the number of bits needed) separately for the FROM and TO
;;  fields. That is, FROM register 3 and TO register 7 might represent
;;  the same register. So here we define an "objective" function for
;;  registers to specify a unique register, and can then set up a
;;  "translation" table between the microcode version(s) of register
;;  numbers and ours.

;; make sure we  always count *intermediate-argument* which is generated by the compiler...
(defparameter *default-from-registers* '(*intermediate-argument*)
  "Additional from registers that are typically generated by the compiler")

(defparameter *default-to-registers* '(*intermediate-argument*)
  "Additional to registers that are typically generated by the compiler")

;; setting up the anaphors and allowing the nanocode to access them:
(let ((from-anaphor-table nil)
      (to-anaphor-table nil))
  (defun clear-anaphors ()
    (setq from-anaphor-table nil)
    (setq to-anaphor-table nil))

  (defun debug-anaphors ()
    (values :from from-anaphor-table :to to-anaphor-table))
  
  ;; assumes the code is an int, eventually will be a bit vector

  (defun setup-anaphors ()
    (let ((to-code 1) ; start at 1
          (from-code 1))

      (dolist (to (union *to-registers-used* *default-to-registers*)) 
        (update-alist to-code to to-anaphor-table)
        (incf to-code))

      (dolist (from (union *from-registers-used* *default-from-registers*))
        (update-alist from-code from from-anaphor-table)
        (incf from-code))))

  (defun to-code-to-anaphor (code)
    "given a code, look up the to register"
    (cdr (assoc code to-anaphor-table)))

  (defun from-code-to-anaphor (code)
    "given a code, look up the from register"
    (cdr (assoc code from-anaphor-table)))
  
  (defun lookup-to-anaphor (register)
    "given a register, look up the anaphor code"
    (car (rassoc (translate-alias register)
                 to-anaphor-table)))
  
  (defun lookup-from-anaphor (register)
    (car (rassoc (translate-alias register)
                 from-anaphor-table)))
  )

(defmethod clear-project-specific-tables :after ()
  (clear-anaphors))

(defmethod initialize-project-specific-tables :after ()
  (setup-anaphors))
      
;; use rr as shorthand for real register :-). Note we want something
;; bit-decoded since this will end up on hardware. After all, a wide
;; nanocontrol is saving us a bunch of space in the microcontrol
;; representation so essentially we're decoding down to specific
;; control lines here. And we need to integrate the control lines on
;; the register since we can, for instance, generate an instruction
;; that goes from register A to register B and C in the same cycle.
;; (This is hardware!)

;; so go back and "fix" defureg to add appropriate constants:
;; +rr-<control-line>-<register-name>+ by bit starting with #o4.

;; NB: if we add more predefined lines here, make sure to update
;; *nanocontrol-line-next-initial-value* in machine-predefs.lisp
;;
;; these are anaphoric references in the nanocode to the appropriate
;; field in the microcode (thus a language feature, not generated
;; based on defining control lines and so a manual process at this
;; point)

(defconstant +rr-from*+ #o1) ; anaphor - from in microcode represents
                             ; the from register
(defconstant +rr-to*+ #o2) ; anaphor - to in microcode represents the
                           ; to register
(defconstant +rr-from-to*+ #o4) ; anaphor - to in microcode represents
                                ; (a) from register (typically from
                                ; field is address and this is what to
                                ; write to that address)
(defconstant +rr-from-type-const*+ #o10) ; use from field in microcode
                                         ; as a type constant
(defconstant +rr-from-const*+ #o20) ; use the from field in microcode
                                    ; as a reguslar constant
(defconstant +rr-from*-type+ #o40) ; anaphor from in microcode
                                   ; represents a register from which
                                   ; we want the type field

;; in addition (different field) we need to specify pads. Unlike the
;; control lines on the registers, we haven't defined a bit level
;; representation anywhere so we do so here. Right now I'm only
;; defining pins that can output - inputs wouldn't be tied to the
;; nanocontroller since it is incapable of conditions (or need to be
;; tied directly to the nanocontroller next state). My reading is
;; these inputs are tied to the microcontroller state instead, so
;; we'll deal with them there.

;; note that these could (should?) be created in a similar manner to
;; register controls via the defchip-pad macro, but since there aren't
;; that many of them I'm doing it manually for now (TBD)

;; also while the particular bit vectors are project specific, we
;; should make tools to generate/interpret them generic and move to
;; fpga-support! (TBD)

(defconstant +pad-run-nano+ #o1) ; this is *run-nano* a :latched-IO pseudo pad
(defconstant +pad-ale+ #o2) ; output is content of *address*
(defconstant +pad-read+ #o4) ; input is sent to *memory* and *bus*
(defconstant +pad-write+ #o10) ; output is content of *memory*
(defconstant +pad-cdr+ #o20) ; which half of a memory cons to read or write. (Is our word length 32 or 64? ;-)
(defconstant +pad-read-interrupt+ #o40) ; input is sent to *interrupt* 
(defconstant +pad-gc-needed+ #o100) ; should latch on until cleared
(defconstant +pad-clear-gc+ #o200) ; pseudo-pad to clear the latched gc-needed pad

(defconstant +pad-conditional+ #o400) ; pseudo-pad to deal with conditionals
(defconstant +pad-mask-interrupts+ #o1000) ; pseudo-pad to prevent responding to additional interrupts
(defconstant +pad-clear-mask-interrupts+ #o2000) ;clear above pad

;; set up an alist to allow us to associate the pad control line with
;; the appropriate (symbolic) functions
(defvar *nanocontrol-pad-spec*
  `((,+pad-run-nano+ (:name *run-nano*))
    (,+pad-ale+ (:name *ale*))
    (,+pad-read+ (:name *read*))
    (,+pad-write+ (:name *write*))
    (,+pad-cdr+ (:name *cdr*))
    (,+pad-read-interrupt+ (:name *read-interrupt*))
    (,+pad-gc-needed+ (:name *gc-needed*))
    (,+pad-clear-gc+ (:name *gc-needed*) (:clear-latch t))
    (,+pad-conditional+ (:name *conditional*))
    (,+pad-mask-interrupts+ (:name *mask-interrupts*))
    (,+pad-clear-mask-interrupts+ (:name *mask-interrupts*) (:clear-latch t))))

(defun rr-value (sym)
  (symbol-value (intern (format nil "+RR-~a+" (string sym)))))

(defun parse-defnano (instructions)
  "Run at compile time to parse a defnano form into a bit vector we
can use to populate the nano control array."
  (macrolet ((ior-bit (var bit)
               `(setq ,var (logior ,var ,bit))))
    (let ((result nil))
      (dolist (line instructions)
        (assert (endp (cddr line)) (line) "defnano line: ~s is malformed" line)
        (let ((register-spec (car line))
              (register-bit-spec 0)
              (control-spec (cadr line))
              (pad-bit-spec 0))
          ;; set control lines on register. Note that some control
          ;; specs appear to include the register (not sure why not
          ;; just put on register spec but I didn't design this
          ;; language!)
          (cl:cond
            ((null register-spec)
             ;; one of the controls should specify a register, unless this is a no-op
             )
            (t
             (setq register-bit-spec
                   (reduce #'logior register-spec 
                           :initial-value 0
                           :key #'(lambda (r) 
                                    (rr-value r))))))
          (cl:cond
            ((null control-spec)
             ;; must be a no-op
             )
            (t
             ;; remember the flags field we defined for register? Let's
             ;; reuse that number scheme here for simplicity ignoring
             ;; the sense lines; they're of the form
             ;; +register-<control>+ to specify the bit. The register
             ;; itself is just *<register-name>* of course though later
             ;; when we interpret the nanoinstruction we'll need both to
             ;; have an argument for the register-<control-line>-p
             ;; function.

             ;; we can't be quite as clever as we were with the register
             ;; names (above) since there are pad controls, register
             ;; references, etc. Well I guess we could be but we'd need
             ;; to define a bunch of new constants. For now let's just
             ;; use a case statement to get the right bits and or them.

             ;; since some of these terms may actually include register
             ;; references process one at a time rather than a reduction
             ;; (for now, anyway)
             (flet ((code-reg (bit-pattern)
                      (ior-bit register-bit-spec bit-pattern))
                    (code-pad (bit-pattern)
                      (ior-bit pad-bit-spec bit-pattern)))
               (dolist (spec control-spec)
                 (case spec
                   (set-condition       ; special
                    (code-pad +pad-conditional+)) ; pseudo-pad to switch how u-pc is calculated
                   (ale
                    (code-reg +rr-to-address+) ;; move what's on the bus to the address pseudo-register
                    (code-pad +pad-ale+)) ; should trigger moving *address* to the pads
                   (read
                    (code-pad +pad-read+) ; should trigger moving the pads to *memory*
                    (code-reg +rr-from-memory+)) ; and from there to the bus
                   (write
                    (code-reg +rr-to-memory+) ; move bus to *memory*
                    (code-pad +pad-write+)) ; and signal it should be written
                   (cdr
                    (code-pad +pad-cdr+))
                   (read-interrupt
                    (code-pad +pad-read-interrupt+))
                   (gc-needed
                    (code-pad +pad-gc-needed+)) ; should be latched
                   (clear-gc
                    (code-pad +pad-clear-gc+)) ; to clear the latch
                   (mask-interrupts
                    (code-pad +pad-mask-interrupts+)) ; should be latched
                   (clear-mask-interupts
                    (code-pad +pad-clear-mask-interrupts+))
                   (t
                    (code-reg (rr-value spec))))))))

          ;; don't convert to a bit-vector yet to make it easier to work with (TBD)
          (setq result (append result (list (list pad-bit-spec register-bit-spec nil)))))) 
      result)))

(defun decode-nanocontrol-pads (whole-line)
  "given a line from the nanocontrol, decode pad references into symboloic components (e.g., for printing)"
  (let ((result nil)
        (line (first whole-line))) ; right now line is a triple of integers, first one is the pad encoding
    ;; test each bit
    (cl:if (plusp (logand line +pad-run-nano+))
           (push 'run-nano result))
    (cl:if (plusp (logand line +pad-ale+))
           (push 'ale result))
    (cl:if (plusp (logand line +pad-read+))
           (push 'read result))
    (cl:if (plusp (logand line +pad-write+))
           (push 'write result))
    (cl:if (plusp (logand line +pad-cdr+))
           (push 'cdr result))
    (cl:if (plusp (logand line +pad-read-interrupt+))
           (push 'read-interrupt result))
    (cl:if (plusp (logand line +pad-gc-needed+))
           (push 'gc-needed result))
    (cl:if (plusp (logand line +pad-clear-gc+))
           (push 'clear-gc result))
    (cl:if (plusp (logand line +pad-conditional+))
           (push 'set-conditional result))
    (cl:if (plusp (logand line +pad-mask-interrupts+))
           (push 'mask-interrupts result))
    (cl:if (plusp (logand line +pad-clear-mask-interrupts+))
           (push 'clear-mask-interrupts result))
    result))
        
(defun decode-nanocontrol-registers (whole-line)
  "Given a line from the nanocontrol, decode register references into symbolic components (e.g., for printing)"
  ;; right now, just deal with the control side as an integer - fix later
  ;; maybe can get defureg to automate building this?
  (let ((result nil)
        (line (second whole-line))) ; right now line is a triple of integers, pick the second one
    ;; test each bit
    (cl:if (plusp (logand line +rr-from*+))
        (push 'from* result))
    (cl:if (plusp (logand line +rr-to*+))
        (push 'to* result))
    (cl:if (plusp (logand line +rr-from-to*+))
           (push 'from-to* result))
    (cl:if (plusp (logand line +rr-from*-type+))
           (push 'from*-type result))
    (cl:if (plusp (logand line +rr-from-type-const*+))
           (push 'from-type-const* result))
    (cl:if (plusp (logand line +rr-from-const*+))
           (push 'from-const* result))
    (cl:if (plusp (logand line +rr-to-memtop+))
        (push 'to-memtop result))
    (cl:if (plusp (logand line +rr-from-memtop+))
        (push 'from-memtop result))
    (cl:if (plusp (logand line +rr-to-type-newcell+))
        (push 'to-type-newcell result))
    (cl:if (plusp (logand line +rr-to-address-newcell+))
        (push 'to-address-newcell result))
    (cl:if (plusp (logand line +rr-from-newcell+))
        (push 'from-newcell result))
    (cl:if (plusp (logand line +rr-from-incremented-newcell+))
        (push 'from-incremented-newcell result)) 
    (cl:if (plusp (logand line +rr-to-type-exp+))
        (push 'to-type-exp result))
    (cl:if (plusp (logand line +rr-to-displacement-exp+))
        (push 'to-displacement-exp result))
    (cl:if (plusp (logand line +rr-to-frame-exp+))
        (push 'to-frame-exp result))
    (cl:if (plusp (logand line +rr-from-exp+))
        (push 'from-exp result))
    (cl:if (plusp (logand line +rr-from-decremented-exp+))
           (push 'from-decremented-exp result))
    (cl:if (plusp (logand line +rr-from-decremented-frame-exp+))
           (push 'from-decremented-frame-exp result))
    (cl:if (plusp (logand line +rr-from-decremented-displacement-exp+))
        (push 'from-decremented-displacement-exp result))
    (cl:if (plusp (logand line +rr-to-type-val+))
        (push 'to-type-val result))
    (cl:if (plusp (logand line +rr-to-address-val+))
        (push 'to-address-val result))
    (cl:if (plusp (logand line +rr-from-val+))
        (push 'from-val result))
    (cl:if (plusp (logand line +rr-to-args+))
        (push 'to-args result))
    (cl:if (plusp (logand line +rr-from-args+))
        (push 'from-args result))
    (cl:if (plusp (logand line +rr-to-type-stack+))
        (push 'to-type-stack result))
    (cl:if (plusp (logand line +rr-to-address-stack+))
        (push 'to-address-stack result))
    (cl:if (plusp (logand line +rr-from-stack+))
        (push 'from-stack result))
    (cl:if (plusp (logand line +rr-to-display+))
        (push 'to-display result))
    (cl:if (plusp (logand line +rr-from-display+))
        (push 'from-display result))
    (cl:if (plusp (logand line +rr-to-intermediate-argument+))
        (push 'to-intermediate-argument result))
    (cl:if (plusp (logand line +rr-from-intermediate-argument+))
        (push 'from-intermediate-argument result))
    (cl:if (plusp (logand line +rr-to-type-retpc-count-mark+))
        (push 'to-type-retpc-count-mark result))
    (cl:if (plusp (logand line  +rr-to-address-retpc-count-mark+))
        (push 'to-address-retpc-count-mark result))
    (cl:if (plusp (logand line  +rr-from-retpc-count-mark+))
        (push 'from-retpc-count-mark result))
    (cl:if (plusp (logand line +rr-from-nil+))
        (push 'from-nil result))
    (cl:if (plusp (logand line +rr-to-address+))
        (push 'to-address result))
    (cl:if (plusp (logand line +rr-from-address+))
        (push 'from-address result))
    (cl:if (plusp (logand line +rr-to-memory+))
        (push 'to-memory result))
    (cl:if (plusp (logand line +rr-from-memory+))
           (push 'from-memory result))
    (cl:if (plusp (logand line +rr-to-micro-pc+))
           (push 'to-micro-pc result))
    (cl:if (plusp (logand line +rr-from-interrupt+))
        (push 'from-interrupt result))
    result))

;; for debugging
(defun decode-nanocontrol-line (line)
  (list (decode-nanocontrol-registers line)
        (decode-nanocontrol-pads line)))

;; decode the whole array
(defun decode-nanocontrol-array ()
  (let ((result nil))
    (dotimes (i (fill-pointer *nanocontrol-array*))
      (push (decode-nanocontrol-line (elt *nanocontrol-array* i)) result))
    (nreverse result)))

(defun find-nanocontrol-content (line-spec &optional (starting-offset 0) (ending-offset nil))
  ;; for now line-spec should be a triple, pad-spec control-spec next-state
  ;; if next-state is NIL it hasn't been assigned yet (as we expect from the passed line-spec but not the content of the array)
  (let ((posn (position-if #'(lambda (entry)
                               (and (= (car entry) (car line-spec))
                                    (= (cadr entry) (cadr line-spec))))
                           *nanocontrol-array*
                           :start starting-offset
                           :end ending-offset)))
    (cl:if (not (null posn))
        (values (elt *nanocontrol-array* posn) posn)
        nil)))

;; this should reproduce the behavior described in the AIM for compression of the nanocontrol
(defun match-into-nanocontrol (array-lines &optional (current-offset 0) (ending-offset nil))
  ;; if all the array lines match a sequence into nanocontrol, return the offset of the first such control line
  (let ((nline nil)
        (line (car array-lines)))
    (msetq (nline current-offset) (find-nanocontrol-content line current-offset ending-offset))
    (unless nline
      (return-from match-into-nanocontrol nil)) ; fail
    
    ;; does the rest of the entry match as well?
    (cl:cond
      ((endp (cdr array-lines))
       (values nline current-offset)) ; done matching
      ((match-into-nanocontrol (cdr array-lines) (third nline) (1+ (third nline))) ; forces match on only the next-state for that instruction.
       (values nline current-offset))
      (t
       ;; try another entry note current-offset was updated by the
       ;;  last find, so this should find another one if it exists
       (match-into-nanocontrol array-lines (1+ current-offset) nil)))))

(defun update-nanocontrol (array-lines &optional force-p)
  "If force-p is non-nil, add even if a duplicate (because our system
is currently sensitive to the nanocontrol name, should typically only
be used for one-line defnano"
  
  ;; do we already have such a set of lines in our control array?
  (mlet (match posn) (match-into-nanocontrol array-lines)
    (cl:cond
      ((and match (not force-p))
       ;; just use that one - optionally print a diagnostic so we know what's going on
       (note-if *debug-defnano* ";;debug-defnano: found match at offset ~d" posn)
       posn)
      (t
       ;; no so just add all (?) the lines.
       ;; check each cdr to see if we can save space by jumping into another nanocontrol sequence (just like in the AIM)
       (let ((first-instruction nil)) ; keep track of entry point so we can bind it to the name
         (block done
           (do ((current-line (car array-lines) (car remaining-lines))
                (remaining-lines (cdr array-lines) (cdr remaining-lines))
                (next-entry (fill-pointer *nanocontrol-array*) (1+ next-entry)))
               ((endp remaining-lines)
                (cl:progn
                  (setf (third current-line) 0)
                  (when *debug-defnano*
                    (format t "~%debug-defnano: final output: ~s~%" current-line))
                  (let ((posn (vector-push-extend current-line *nanocontrol-array*)))
                    (or first-instruction posn))))

           ;; we know there must be more lines else we'd be in the
           ;; return part of the do loop. So check to see if they match
           ;; something already in the control store so we can compress
           ;; things by just jumping there (thanks Knuth ;-)
           (mlet (matchcdr posncdr) (match-into-nanocontrol (cdr array-lines))
             (cl:cond
               (matchcdr
                (setf (third current-line) posncdr) ; let that implement the rest of this nano instruction
                (when *debug-defnano* 
                  (format t "~%debug-defnano: matchcdr return: ~s~%" current-line))
                (return-from done (or first-instruction (vector-push-extend current-line *nanocontrol-array*))))
               (t
                (setf (third current-line) (1+ next-entry))
                (let ((new-posn (vector-push-extend current-line *nanocontrol-array*)))
                  (unless first-instruction
                    (setq first-instruction new-posn)))
                (when *debug-defnano* 
                  (format t "~%debug-defnano: adding: ~s~%" current-line)))))))))))) ; go loop

  
(defmacro defnano ((name &key force-add) &body instructions)
  "note this macro expands based on the current state of
*nanocontrol-array* which is probably not the best approach (instead
we should probably move the lookup of already existing entries to
load-time instead of macro-expansion-time). If force-add is true, we
create a new nanocontrol entry for the form even if it would otherwise
have been a duplicate so we can distinguish the uses"
  
  (let* ((array-lines (gensym))
         (array-lines-actual (parse-defnano instructions)))
    ;; each array line, above, is in the form of (<pad-bit-spec>
    ;; <register-bit-spec> <control-bit-spec>) (where the
    ;; control-bit-spec and register-bit-spec can be considered as
    ;; combined as in the paper)

    ;; so our job here is to add the array lines to the control array
    ;; 'as we go', but also check for already existing array lines as
    ;; we can just make those our next state instead of duplicating
    ;; states. Note that the array-lines won't have filled in the
    ;; next-state field, so we also do that here (generally some
    ;; offset to the next empty line and then sequential unless we get
    ;; a dup) Note that duplicate detection needs to check that the
    ;; following states are also dupes. Next-state 0 means we're done
    ;; with a nanoinstruction and we can return control to the
    ;; microinstruction level.

    ;; add freeze state to all but last line
    ;; this should keep the microcontroller from running while the subsequent nano operations can run

    (mapc #'(lambda (line)
              (setf (first line) (logior (first line) +pad-run-nano+)))
          (butlast array-lines-actual))

    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (export-ulisp-symbol ',name)
       (let ((,array-lines ',array-lines-actual)) ; capture the expansion
         (note-if *debug-defnano* ";;debug-defnano: defining ~A" ',name)
         
         (update-alist ',name (update-nanocontrol ,array-lines ,force-add) *nanocontrol-symtab*)))))


;; now define the nanocontroller (emulator), that gets called to
;; interpret a line of microcode (just as the microcode gets called to
;; interpret the objective S-code).

;; collect-ncode is probably only needed for debugging and console support...
(defun collect-ncode (offset)
  "Return a list of pairs of the offsets and the nanocodes starting from
a given offset. Since nanocode doesn't handle conditionals, it's just a
linked list."
  (let* ((result nil)
         (current-code (elt *nanocontrol-array* offset))
         (next-state (third current-code)))
    (cl:cond
      ((not current-code)
       (warn "Offset ~d does not represent a valid nanocode!" offset))
      (t
       (push (cons offset current-code) result)
       (while (plusp next-state) ; terminates with a 0
         (setq current-code (elt *nanocontrol-array* next-state))
         (assert current-code () "Invalid next state in position ~D of nanocontrol-array" next-state)
         (push (cons next-state current-code) result)
         (setq next-state (third current-code)))
       (nreverse result)))))

;;; finalize the nanocontrol-array
;;
;; the nanocontrol array is intially a triple of pad-spec control-spec
;; next-state, and we know the maximum next-state is one less than the
;; fill-pointer (since that points to the next array element to be
;; filled), *nanocontrol-pad-spec* contains the associations between
;; the bits in the pad control field and the varibles that represent
;; pads, and *nanocontrol-line-next* has the next bit to be assigned
;; if there were a new (unique) register control to be established. So
;; that lets us determine the size of each of these fields if
;; represented as bit vectors, and thus the total size for a "line" in
;; *nanocontrol-array-bitvectors*

(defun check-for-duplicate-nanocontrol-definitions ()
  (let ((dups (duplicates-p *nanocontrol-symtab* :key #'cdr)))
    (when (not (null dups))
      (setq dups (delete-duplicates dups :key #'cdr)) ; in case we have multiple hits
      ;; now for each hit, find all the matches and print them
      (mapc #'(lambda (hit)
                (warn "Duplicates found in nanocontrol symtab!
;; We will not be able to distinguish between nanooperations: ~S"
                      (mapcan #'(lambda (entry)
                                  (if (eql (cdr hit) (cdr entry))
                                      (list (car entry))
                                      nil))
                              *nanocontrol-symtab*)))
            dups)
      (warn ";; if all entries are expected to be distinct, look for the force-add option on defnano"))))
  
(defun finalize-nanocontrol-array ()
  "We've loaded all of the relevant defnano forms, so now we just need
  to convert into an array of bitvectors that are suitable for FPGA usage"
  ;; quick sanity check
  (check-for-duplicate-nanocontrol-definitions)
  ;; make sure we don't blow our pc representation
  (assert (<= (fill-pointer *nanocontrol-array*) *nano-pc-max-address*) ()
          "Nanocontrol is too large to be addressed by *nano-pc*. Increase *nano-pc-size* in machine-defs.lisp and recompile simulator.")
  
  ;; (tbd) - need to turn into a binary array for download to FPGA
  
  (values)) ; for the moment we will work with the unfinalized forms as it's easier to debug!

;;; code to run the nanocontroller simulation

(defvar *nanocontroller-deferred* nil
  "List of things to do before we start the next nanocycle")

(defun defer-nanocode (fn description)
  "Set the fn to be deferred until the start of the next nanocycle"
  (push (list fn description) *nanocontroller-deferred*))

(defun run-deferred-nanocode ()
  (mapc #'(lambda (x)
            (note-if *debug-nanocontroller* "debug-nanocontroller: running deferred action: ~s" (cadr x))
            (funcall (car x))
            (note-if *debug-nanocontroller* "debug-nanocontroller: post-deferred action; from-incremented: ~s" (register-from-incremented-p '*newcell*)))
        *nanocontroller-deferred*) ; generally turn off control lines from prior cycle
  (setq *nanocontroller-deferred* nil)) ; clear it

(defvar *nanocontroller-ran-p* nil
  "A flag to tell the microcontroller that the nanocontroller in fact has run")

(defun run-nanocontroller-p1 ()
  "Given a new nanocontroller state, run until we reach state 0 (quiescence) which would return control to the microcontroller state machine."
  ;; decode the state. Right now it's a triple of integers, eventually
  ;; it should be a bit vector with fields mimicking what we would have
  ;; in the hardware implementation.

  ;; microcode right now is 4 integers (eventually a single bit-vector
  ;; with fields) (see machine-micro.lisp):
  
  ;; ustate: <next-ustate> <nano-operation> <from> <to>

  (let ((microcode-line (pla-read *microcontrol-array* *micro-pc*)))
    ;; RIGHT now using quadruples instead of bitvectors (TBD)
    (destructuring-bind (next-ucode nop from to) microcode-line
      (declare (ignore next-ucode))

      (setq *nanocontroller-ran-p* t) ; so microcontroller can modify the upc when it runs next cycle

      ;; do we already have a nano-pc?
      (unless (plusp (bit-vector->integer *nano-pc*))
        (note-if *debug-nanocontroller* "debug-nanocontroller: setting nano-pc to #o~o" nop)
        (copy-field (integer->bit-vector nop) *nano-pc*)) ; (tbd) should be stored as a bit vector
      (note-if *debug-nanocontroller*
               "*** executing nanoinstruction ~a at #o~o ***"
               (car (rassoc (bit-vector->integer *nano-pc*) *nanocontrol-symtab*)) (bit-vector->integer *nano-pc*))
      (exec-nano-line (elt *nanocontrol-array* (bit-vector->integer *nano-pc*)) from to)))) 

(defun run-nanocontroller-p2 ()
  "Second part of run-nanocontroller - clear out the control lines on
registers and update to the next nanoinstruction if needed"

  (run-deferred-nanocode)

  ;; now set up next nano-instruction

   (destructuring-bind (pad-controls register-controls next-state) (elt *nanocontrol-array* (bit-vector->integer *nano-pc*))
     (declare (ignore pad-controls register-controls))
     (note-if *debug-nanocontroller*
              "debug-nanocontroller-p2: setting nano-pc for next cycle to: #o~o"
              next-state)
  
     (copy-field (integer->bit-vector next-state) *nano-pc*))) ; (tbd) should be stored as a bit vector

(defvar *memoization-fns* nil)
(defvar *cmemoization-fns* nil)

;; this will need to be transformed into a hardware PLA based approach...
;; but for simulation it's fine (TBD)
(defun exec-nano-line (controller-entry from to)
  ;; lookup the state in the control table
  ;; again, using triples at this point.
  (destructuring-bind (pad-controls register-controls next-state) controller-entry
    (declare (ignore next-state))

    (let ((decoded-register-controls (decode-nanocontrol-registers controller-entry)))
      (declare (ignore decoded-register-controls)) ; for debugging
      ;; execute the current nanocontroller entry

      ;; treat from* and to* special as they are anaphors for using the reference in the microcode
      (when (and (plusp (logand register-controls +rr-from*+))
                 (plusp from)) ; if zero, then no control set so will be from the bus
        ;; do we have a saved version?
        (let* ((anaphor (from-code-to-anaphor from))
               (key (list 'from anaphor))
               (setter (or (cdr (assoc key *memoization-fns* :test #'equal))
                           (and (update-alist key (set-control-line-fn 'from anaphor) *memoization-fns* :test #'equal)
                                (cdr (assoc key *memoization-fns* :test #'equal)))))
               (csetter (or (cdr (assoc key *cmemoization-fns* :test #'equal))
                            (and (update-alist key (compile nil setter) *cmemoization-fns* :test #'equal)
                                 (cdr (assoc key *cmemoization-fns* :test #'equal))))))
          (note-if *debug-nanocontroller*
                   "debug-nanocontroller: setting control line FROM for ~a"
                   anaphor)
          (funcall csetter t)
          (defer-nanocode #'(lambda () (funcall csetter nil))
              (format nil "Clear FROM control line for ~a" anaphor))))

      (when (and (plusp (logand register-controls +rr-to*+))
                 (plusp to))
        (let* ((anaphor (to-code-to-anaphor to))
               (key (list 'to anaphor))
               (setter (or (cdr (assoc key *memoization-fns* :test #'equal))
                           (and (update-alist key (set-control-line-fn 'to anaphor) *memoization-fns* :test #'equal)
                                (cdr (assoc key *memoization-fns* :test #'equal)))))
               (csetter (or (cdr (assoc key *cmemoization-fns* :test #'equal))
                            (and (update-alist key (compile nil setter) *cmemoization-fns* :test #'equal)
                                 (cdr (assoc key *cmemoization-fns* :test #'equal))))))
          (note-if *debug-nanocontroller*
                   "debug-nanocontroller: setting control line TO for ~a"
                   anaphor)
          (funcall csetter t)
          (defer-nanocode #'(lambda () (funcall csetter nil))
              (format nil "Clear TO control line for ~a" anaphor))))

      ;; 9-7-21 modified to use the FROM anaphor table, with support in code generation!
      (when (and (plusp (logand register-controls +rr-from-to*+))
                 (plusp to))
        ;; hack for dealing with the (rare?) case where we use the TO
        ;; field in the microcode as a FROM address (e.g. for ALE)
        (let* ((anaphor (from-code-to-anaphor to))
               (key (list 'from-to anaphor))
               (setter (or (cdr (assoc key *memoization-fns* :test #'equal))
                           (and (update-alist key (set-control-line-fn 'from anaphor) *memoization-fns* :test #'equal)
                                (cdr (assoc key *memoization-fns* :test #'equal)))))
               (csetter (or (cdr (assoc key *cmemoization-fns* :test #'equal))
                            (and (update-alist key (compile nil setter) *cmemoization-fns* :test #'equal)
                                 (cdr (assoc key *cmemoization-fns* :test #'equal))))))
          (note-if *debug-nanocontroller*
                   "debug-nanocontroller: setting control line FROM (via TO field) for ~a"
                   anaphor)
          (funcall csetter t)
          (defer-nanocode #'(lambda () (funcall csetter nil))
              (format nil "Clear FROM control line (via TO field) for ~a" anaphor))))

      ;; handle from*-type 10/21/21
      (when (and (plusp (logand register-controls +rr-from*-type+))
                 (plusp from))
        ;; the type field of the register named the FROM field should be placed onto the bus
        (let* ((anaphor (from-code-to-anaphor from))
               (key (list 'from-type anaphor))
               (setter (or (cdr (assoc key *memoization-fns* :test #'equal))
                           (and (update-alist key (set-control-line-fn 'from-type anaphor) *memoization-fns* :test #'equal)
                                (cdr (assoc key *memoization-fns* :test #'equal)))))
               (csetter (or (cdr (assoc key *cmemoization-fns* :test #'equal))
                            (and (update-alist key (compile nil setter) *cmemoization-fns* :test #'equal)
                                 (cdr (assoc key *cmemoization-fns* :test #'equal))))))
          (note-if *debug-nanocontroller*
                   "debug-nanocontroller: setting control line FROM-TYPE for ~a (csetter ~s)"
                   anaphor csetter)
          (funcall csetter t)
          (defer-nanocode #'(lambda () (funcall csetter nil))
              (format nil "Clear FROM-TYPE control line for ~a (csetter ~s)" anaphor csetter))))
      
      ;; ok, now based on the other bits, get the specs and set the control bits on the appropriate registers
      (mapc #'(lambda (entry)
                (when (plusp (logand (car entry) register-controls))
                  (let* ((setter (cadr (assoc :setter (cdr entry))))
                         ;; force it into a form suitable for funcall (rather than eval)
                         (csetter (or (cdr (assoc :csetter (cdr entry)))
                                      (and (update-alist :csetter (compile nil setter) (cdr entry))
                                           (cdr (assoc :csetter (cdr entry)))))))
                    (note-if *debug-nanocontroller*
                             "debug-nanocontroller: setting control line ~a for ~a (csetter: ~s)"
                             (cadr (assoc :control (cdr entry))) (cadr (assoc :register (cdr entry))) csetter)
                    (funcall csetter t)
                    ;; clear the register bits before the next nanocycle starts
                    (defer-nanocode #'(lambda () (funcall csetter nil))
                        (format nil "Clear control line ~a for ~a (csetter: ~s)"
                                (cadr (assoc :control (cdr entry)))
                                (cadr (assoc :register (cdr entry)))
                                csetter)))))
            *nanocontrol-to-spec*)
      ;; now set pads. Presumably nanocode runs during :ph2? So some
      ;; things might have to be delayed until ph1 (which is also
      ;; when the register control lines get checked)
      (mapc #'(lambda (entry)
                (let* ((pad-bit (car entry))
                       (pad-name (cadr (assoc :name (cdr entry))))
                       (pad-clears-latch-p (cadr (assoc :clear-latch (cdr entry))))
                       (pad-type (get-pad-defn-value pad-name :type)))
                  (cl:cond
                    ((and pad-clears-latch-p
                          (plusp (logand pad-bit pad-controls)))
                     (note-if *debug-nanocontroller*
                              "debug-nanocontroller: clearing pad ~a" pad-name)
                     (clear-pad pad-name))
                    ((plusp (logand pad-bit pad-controls))
                     (note-if *debug-nanocontroller*
                              "debug-nanocontroller: setting pad ~a" pad-name)
                     (set-pad pad-name))
                    ((equal pad-type :latched-io) ; e.g. *run-nano*
                     ;; explicitly clear it since not setting it... but only if we set it.
                     (cl:if (and (equal pad-name '*run-nano*) ; yes we set it, (OK this is currently the only one)
                              (test-pad-immediate pad-name)) ; and it's actually set
                         (clear-pad pad-name))))))
            *nanocontrol-pad-spec*))))

;; link running the nanocontroller into the clock cycle
(defun run-register-controls ()
  (note-if *debug-dataflow* "Running register data flow simulation")                   
  (initializations '*from-controls*)
  (initializations '*to-controls*)
  (reset-initializations '*from-controls*)
  (reset-initializations '*to-controls*))

(defun run-sense-controls ()
  (note "Running register sense simulation")
  (initializations '*sense-controls*)
  (reset-initializations '*sense-controls*))

(execute-during-clock ("run-nanocontroller part 1" *run-nanocontroller-p1*)
  (run-nanocontroller-p1))

;; not ideal - want everything on rising clocks?
(execute-during-clock ("run-nanocontroller part 2" *run-nanocontroller-p2*) 
  (run-register-controls) ; rerun in case changes happened during external pad sync
  (run-nanocontroller-p2))

;; now we have to add an initialization to run the from and to controls the first time
(execute-during-clock ("Run control inits" *run-register-controls*)
                      (progn (run-register-controls)
                             (run-sense-controls))) ; after register controls run, run sense controls

(execute-during-clock ("Run sense inits" *update-sense-lines*)
                      ;; rerun sense controls (really would be
                      ;; continuous until we latch, so this is really
                      ;; the "final run" for a particular tick
                      (run-sense-controls)) 
