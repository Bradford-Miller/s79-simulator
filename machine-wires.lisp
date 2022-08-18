(in-package :scheme-mach)

(scheme-79:scheme-79-version-reporter "Scheme Machine Wire Defs" 0 4 2
                                      "Time-stamp: <2022-06-01 11:40:03 Bradford W. Miller(on Boromir)>"
                                      "add clear-run-nano bit")

;; 0.4.2   6/ 1/22 add clear-run-nano bit as run-nano should be latched.
;;                 Add set and get on the external bus pad bits too,
;;                     following where the VHDL is heading (though we don't
;;                     connect them up to a register yet as that requires
;;                     more info??)

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
;;
;; note that some of these are mutex and thus could be condensed into binary
;; codes (i.e., you can't use from-to and eitehr from or to, so doesn't need
;; to be a distinguished bit). BUT we're not as concerned as the authors
;; about efficient use of space so ignoring for the time being.

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
(defconstant +rr-from*-type+ #o40) ; anaphor from in microcode represents a
                                   ; register from which we want the type
                                   ; field (which should be placed on the low
                                   ; order bits of the bus as a value).

;; in addition (different field) we need to specify pads. Unlike the
;; control wires on the registers, we haven't defined a bit level
;; representation anywhere so we do so here. Right now I'm only
;; defining pins that can output - inputs wouldn't be tied to the
;; nanocontroller since it is incapable of conditions (or need to be
;; tied directly to the nanocontroller next state). My reading is
;; these inputs are tied to the microcontroller state instead, so
;; we'll deal with them there.

;; note that these could (should?) be created in a similar manner to register
;; controls via the defchip-pad macro, but since there aren't that many of
;; them I'm doing it manually for now; once we need to also generate VHDL
;; we'll have to make it automatic (TBD)

;; also while the particular bit vectors are project specific, we
;; should make tools to generate/interpret them generic and move to
;; fpga-support! (TBD)

(defconstant +pad-run-nano+ #o1) ; this is *run-nano* a :latched-IO pseudo pad
(defconstant +pad-clear-run-nano+ #o2) ; clear latch
(defconstant +pad-ale+ #o4) ; output is content of *address*
(defconstant +pad-read+ #o10) ; input is sent to *memory* and *bus*
(defconstant +pad-write+ #o20) ; output is content of *memory*
(defconstant +pad-cdr+ #o40) ; which half of a memory cons to read or write. (Is our word length 32 or 64? ;-)
(defconstant +pad-read-interrupt+ #o100) ; input is sent to *interrupt* 
(defconstant +pad-gc-needed+ #o200) ; should latch on until cleared
(defconstant +pad-clear-gc+ #o400) ; pseudo-pad to clear the latched gc-needed pad

(defconstant +pad-conditional+ #o1000) ; pseudo-pad to deal with conditionals
(defconstant +pad-mask-interrupts+ #o2000) ; pseudo-pad to prevent responding to additional interrupts
(defconstant +pad-clear-mask-interrupts+ #o4000) ;clear above pad

(defconstant +pad-set-memory-pads+ #o10000) ; connect FROM internal TO external bus
(defconstant +pad-get-memory-pads+ #o20000) ; connect FROM external TO internal bus

;; set up an alist to allow us to associate the pad control wire with
;; the appropriate (symbolic) functions
(defvar *nanocontrol-pad-spec*
  `((,+pad-run-nano+ (:name *run-nano*))
    (,+pad-clear-run-nano+ (:name *run-nano*) (:clear-latch t))
    (,+pad-ale+ (:name *ale*))
    (,+pad-read+ (:name *read*))
    (,+pad-write+ (:name *write*))
    (,+pad-cdr+ (:name *cdr*))
    (,+pad-read-interrupt+ (:name *read-interrupt*))
    (,+pad-gc-needed+ (:name *gc-needed*))
    (,+pad-clear-gc+ (:name *gc-needed*) (:clear-latch t))
    (,+pad-conditional+ (:name *conditional*))
    (,+pad-mask-interrupts+ (:name *mask-interrupts*))
    (,+pad-clear-mask-interrupts+ (:name *mask-interrupts*) (:clear-latch t))
    ;; not yet sure how to encode connecting *bus* as it will have to specify direction! (TBD)
    ))


(defun rr-value (sym)
  (symbol-value (intern (format nil "+RR-~a+" (string sym)))))

