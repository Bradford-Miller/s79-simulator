(in-package :scheme-mach)

(scheme-79:scheme-79-version-reporter "S79 Sim Ext Defs" 0 3 0
                                      "Time-stamp: <2022-01-11 16:56:34 gorbag>"
                                      "0.3 release!")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.0.0   1/10/22 Split from sim-machine-external.lisp to ensure
;;                    definitions get compiled before use

;; [for now, internally generate and output rather than have pads drive them]

;; inputs:

;; ph1, ph2 (two phase clock, non-overlapping); during ph1 data
;;   manipulated per current microcode state and computes new microcode
;;   state specified by MICRO and NANO PLAs. chip-bus valid by the END
;;   of ph1. External data being read should be valid soon after the
;;   beginning of ph1 so any microcode branch will have time to
;;   propagate through both PLAs. During ph2 the chip transitions to
;;   the new state and the control outputs change accordingly.

(defchip-pad *ph1* :output :ph1-rising :any 2) ; input in original. Test is any because we want to know when it's off
(defchip-pad *ph2* :output :ph2-rising :any 2) ; input in original. Test is any because we want to know when it's off

;; freeze; causes chip to inhibit any state change, it must be stable
;;   during ph1. Inhibits from-x and to-x controls in the register
;;   array, and causes the two state machines to refech their current
;;   state. Control outputs should be ignored during frozen cycles.

;; freeze has full tick (minimum) validity, i.e. asserted DURING
;; ph2, so drop ph2 is 1, neutral is 2, raise ph1 is 3, ph1 high is 4,
;; and drop ph1 is 5.  note it can be held as long as needed, and is
;; used both the freeze the microcode during multiple-cycle nanocode
;; execution and by the external memory to insert a wait state or for
;; debugging!

;; changed to freeze assert to :ph2-rising from :ph2 6/27/21
;; changed to new :latched-io so it won't be auto-cleared 7/7/21 (should depend on external signal!)
(defchip-pad *freeze* :input :any :any 8) 

;; read-state; should be used with freeze asserted. reads out current
;;   MICRO state onto 9 bits of the chip bus. (bits 23, 22,
;;   30-24). The chip can be single stepped by lowering freeze for one
;;   cycle and the internal data observed on the chip-bus. There are
;;   special sequences in the microcode that have been provided just
;;   for scanning in and out the internal registers, so that a
;;   load-state to the right state followed by single stepping the
;;   chip will accompish an examine or deposit from the "switches".

(defchip-pad *read-state* :input :ph2-rising :ph2 3)

;; load-state; should be used with freeze asserted. Sets the microcode
;;   state number from 9 bits of the chip-bus. (bits 23, 22, 30-24)

(defchip-pad *load-state* :input :ph2-rising :ph2 3)

;; [reset] ; I don't see this in the AIM, but there is an associated
;;   microaddress for reset, and I'm not sure we can always count on
;;   the chip powering up in a reset state (or we may want to force a
;;   reset, no?) Perhaps they used "load-state" to force a reset, but
;;   for simplicity, I'm adding it and will add a reset switch to the
;;   console as well. - BWM 1/13/21]

(defchip-pad *reset* :input :any :ph1-rising 5)

(add-initialization "Check Reset Pad"
                    '(when (test-pad '*reset*)
                      (do-reset))
                    nil
                    '*ph1-falling-list*)

;; interrupt-request; when raised and when microcode checks, an
;;   interrupt point is created which encapsulates the state of the
;;   current process, and a read-interrupt cycle is performed to
;;   obtain an address from the interrupting device. The CAR of this
;;   address should be a user procedure of one argument (the
;;   interrupt-point) which will handle the interrupt. It is assumed
;;   there are i/o locations that control interrupt priority level and
;;   re-enabling the interrupt system.

;; note this should be held until read-interrupt is raised so 3 is just a minimum:
(defchip-pad *interrupt-request* :input :any :any 3) 

;; outputs:

;; "The external world is conceptualized as a set of registers with
;; special capabilities."

;; "The external ADDRESS register is used for accessing memory and can
;; be set from the bus."

;; [looks like a register but it's not?!]

;; these are moved to machine-defs so we setup the bit offsets correctly in the face of recompilation.
;(defchip-reg *address*)
;(defureg *address* (to from) ())

;; "The pseudo-register MEMORY can be read onto the bus or written
;; from the bus. The actual access is preformed to the list cell
;; addressed by the ADDRESS register. The CDR bit controls which half
;; of the cell is being accessed."

;(defchip-reg *memory*)
;(defureg *memory* (to from) ())

;; [but we treat the pads as separate so we can enforce timing constraints on them]
(defchip-pads *memory-pads* 32 :io *put-memory-content-onto-pads* *get-memory-content-from-pads* 1)

(defchip-pads *address-pads* 24 :io *put-memory-content-onto-pads*  *get-address-from-pads* 1 *memory-pads*) ; since we reuse the low order bits, just pretend they're the same

;; "One more external register, INTERRUPT, which can be read onto the
;; bus, contains the address of a global symbol whose value (its CAR)
;; is an appropriate interrupt handler."

;(defchip-reg *interrupt*)
;(defureg *interrupt* (from) ())

(defchip-pads *interrupt-pads* 24 :input *put-memory-content-onto-pads* *get-address-from-pads* 1 *memory-pads*) ; like the address pads

;; ale; address-latch-enable; 24 bit node address on low-order bits of
;;   chip bus and ALE asserted, then a following cycle asserts either
;;   read or write to access the node, simultaneously specifying which
;;   half (CAR or CDR) of the node with the cdr signal. If the memory is
;;   slower than the chip, it should assert freeze until the cycle where
;;   the memory can complete the memory operation. See figures 8 & 9.

(defchip-pad *ale* :output *run-nanocontroller-p1* *run-external-data-transfer* 8) ; keep high for full tick

;; read (see above)

(defchip-pad *read* :output *run-nanocontroller-p1* *run-external-data-transfer* 8) ; during the following phase 1 the data read should be placed on the pads

;; write (see above)

(defchip-pad *write* :output *run-nanocontroller-p1* *run-external-data-transfer* 8) ; similar to read but the following ph1 will have the data to write on the pads

;; cdr (see above)

(defchip-pad *cdr* :output *run-nanocontroller-p1* *run-external-data-transfer* 8)

;; read-interrupt (see interrupt-request - interrupting device should
;;   supply an address)

(defchip-pad *read-interrupt* :output *run-nanocontroller-p1* *run-external-data-transfer* 8) ; treat like a read

;; gc-needed ; tied to allocation = memory limit, and allows external
;;   interrupt to supply an address of a procedure to handle gc. The
;;   s-code primitive 'mark' clears the internal gc-needed flip-flop
;;   and performs a gc.

;; because gc-needed is a latch, also set up a variable to store the state of that latch (*gc-needed-latch*)

(defchip-pad *gc-needed* :latched-output *run-nanocontroller-p1* *run-external-data-transfer* 7)

;; I/O:
;;  i/o devices are mapped into a high part of the node space that is
;;  never reached by allocation/collection, and accessed by CAR, CDR
;;  and RPLACx operations on self-evaluating-immediate quantities
;;  (numbers) that point to the i/o locaition. The chip interface
;;  should supply data with valid and consistent data types to read
;;  references. Other pseudo-memory locations are assumed to exist
;;  that provide those primitive functions not implemented internally
;;  to the SCHEME-79 prcessors, such as arithmetic.

;; BOOT LOAD
;; see AIM-559 pg 20.

;; MEMORY:

;; here we simulate the chip's external memory. Eventually I hope to
;; make it easier to load/dump as well, but for now we just establish
;; enough for the boot sequence.

(defparameter *maximum-memory-size* (expt 2 15) ; 16 bits
  "Maximum number of cons cells. Should not exceed 24 bits (8,388,608) based on scheme79 representation limits")

(defparameter *maximum-memory-content* (1- (expt 2 32))
  "Maximum (integer) value that can be stored in the CAR or CDR")

(defparameter *initial-memtop* (expt 2 7)) ; start with 8 bits of cons memory  for debugging


;;                                 CAR    CDR
(defvar *cold-boot-memory-array* '(#o0    #o0) ; address 2, maybe initial expression pointer will point here?
  "Can bind this to whatever we need to initialize the external
memory. Should be a list or vector that will start after address 2")
