(in-package :scheme-mach)

(scheme-79:scheme-79-version-reporter "Scheme Machine Wire Defs" 0 4 1
                                      "Time-stamp: <2022-04-07 12:24:59 gorbag>"
                                      "repatriated from machine-nano")

;; 0.4.1   4/ 7/22 separating defintions that have a machine-implementation
;;                    impact (such as wires) from those that are only about the
;;                    PLA generation (such as nanocode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.4.0   3/18/22 snapping a line: 0.4 release of scheme-79 supports test-0 thru test-3. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use rr as shorthand for real register :-). Note we want something
;; bit-decoded since this will end up on hardware. After all, a wide
;; nanocontrol is saving us a bunch of space in the microcontrol
;; representation so essentially we're decoding down to specific
;; control wires here. And we need to integrate the control wires on
;; the register since we can, for instance, generate an instruction
;; that goes from register A to register B and C in the same cycle.
;; (This is hardware!)

;; so go back and "fix" defureg to add appropriate constants:
;; +rr-<control-wire>-<register-name>+ by bit starting with #o4.

;; NB: if we add more predefined wires here, make sure to update
;; *nanocontrol-wire-next-initial-value* in machine-predefs.lisp
;;
;; these are anaphoric references in the nanocode to the appropriate
;; field in the microcode (thus a language feature, not generated
;; based on defining control wires and so a manual process at this
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
;; control wires on the registers, we haven't defined a bit level
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


;; set up an alist to allow us to associate the pad control wire with
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

