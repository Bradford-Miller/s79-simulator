(in-package :scheme-mach)

(scheme-79:scheme-79-version-reporter "S79 Nanocode" 0 4 0
                                      "Time-stamp: <2022-03-18 15:30:57 gorbag>"
                                      "disambiguate")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.4.0   3/18/22 snapping a line: 0.4 release of scheme-79 supports test-0 thru test-3. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.3.3   2/ 9/22 way too many things (fns, variables) with "line" in their name
;;                    and it's ambiguous.  Splitting so "line" refers to,
;;                    e.g. an output (log) line, "expression" refers to a
;;                    'line' of code (single expression in nano or microcode
;;                    land typically, and because we used (READ) it wasn't
;;                    confined to a single input line anyway) and "wire" to
;;                    refer to, e.g., a control or sense 'line' on a register.

;; 0.3.2   1/25/22 add do-decrement-frame and do-decrement-displacement
;;                     nanocodes

;; 0.3.1   1/13/21 fix symbols of form <reg>-to-<field>; should be
;;                     to-<field>-<reg> following TR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.8  11/30/21 define writei-and-mark-car-of-newcell; instead of
;;                     predefining all the variants, I'm going to
;;                     define as these are needed until such time as I
;;                     can either automate their production or come up
;;                     with a better way of handling the
;;                     specialization, e.g. through multiple matching
;;                     nanoinstructions that separtely handle the
;;                     from/to fields from the instruction field and
;;                     or the results together?

;; 0.1.7  10/ 1/21 force some defnanos to create array entries even if
;;                     duplicates either for debugging
;;                     (e.g. write-car, write-and-unmark-car) or
;;                     because run-microcontroller is sensitive to the
;;                     nanooperation (will need to be addressed as
;;                     this should happen as a result of a control
;;                     wire!) (TBD)

;; 0.1.6   9/28/21 unmark in-use or car-being-traced bit (the mark bit)
;;                     whenever we write the car or cdr unless we are
;;                     setting the mark. This (previously overlooked)
;;                     behavior is from the AIM! (see transcript in
;;                     microcode.mcr)

;; 0.1.5   9/ 7/21 differentiate setting type from constant or register
;;                     define *from-to-nano-operations*

;; 0.1.4   9/ 2/21 define more constant varients for branch

;; 0.1.3   8/26/21 do-decrement-scan-down, write-and-mark-cdr, etc.

;; 0.1.2   8/24/21 writei-and-mark-car-* instructions to support marking

;; 0.1.1   8/21/21 do-set-type-* instructions

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.9   8-14-21 from-to*: sometimes the TO position in the microcode
;;                      must be used as the FROM address (e.g. write-cdr)

;; 0.0.8      8-11-21   add specialized version of get-interrupt-pointer for stack (not TO acceptable)

;; 0.0.7      3- 2-21   call finalize-nanocontrol-array after all the defnano's have been defined (on load)

;; 0.0.6      3- 1-21   more specialized instructions for restore, write-cdr, write-car

;; 0.0.5      2-25-21   add specialized move nanocode for registers without a TO control

;; 0.0.4      2-22-21   conditional

;; 0.0.3      2-20-21   delete do-get-interrupt-routine-pointer

;; 0.0.2      2-15-21   add gc-needed! nano op

;; 0.0.1      1-23-21   add no-op nano operation for address 0 so we can loop there if need be when done.

;; 0.0.0      1-22-21   split from machine-nano.lisp so the macro is defined and loaded before we try to expand

;; Closer to a hardware simulator corresponding with AIM 559
;; based on the published microcode and description

;; define nanoinstructions. Note that the array is populated by the
;; code in ucode-compiler once we've interpreted the microcode and
;; calculated symbolic bit values for registers, etc.

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
;; that trigger control wires to be set and then shifted out at the
;; right time based on the clock.  We'll incorporate the example,
;; above into our definitions.

;; from* - anaphor for whatever register is in the FROM field of the
;;         current microinstruction. It should be moved to the bus,
;;         so set the from control wire for that register.

;; to*   - anaphor for whatever register is in the TO field of the
;;         current microinstruction. It should be filled from the bus,
;;         so set the to control wire for that register.

;; ale   - put the current content of the bus into the *address*
;;         register, set *address-pads* at the right time, and set
;;         *ale* for a clock

;; read  - drop *ale* and raise *read*, move contents of the external
;;         *memory-pads* to *memory* and the bus

;; cdr   - set *cdr* in conjunction with either *read* or *write*
;;         (else should be clear)

;; note that the orignal microcode seemed to have a (TO PADS) [and
;; presumably (FROM PADS) signal to the nanocode to set up writing
;; from the bus to the pads. We have pseudo registers *address* and
;; *memory* set up like actual registers to act as buffers (as well as
;; make debugging somewhat easier though it does throw timing off) and
;; we trigger such transfers based on the read and write signals; we
;; may want to change that in the future (TBD).



;; from-stack
;; to-address-stack
;; to-type-stack
;;       - these all specify a control wire and a particular register,
;;         i.e. a particular control wire.

;; gc-needed
;;       - set *gc-needed* output and latch high

;; clear-gc
;;       - clear *gc-needed* output and latch low (called by anything
;;         setting mark as a gc has been triggered)

;; here we define nano operations. From what I can understand given the
;; text in the AIM, these were originally generated by looking for
;; patterns in the original (unpublished) microcode and 'hand
;; optimizing' common operations into the nanocode. Here I'm starting
;; with the operations they published as examples, and will add
;; additional operations while writing the microcode compiler.


;; [TODO]note that I suspect most of these can be automated based on the
;; register declarations we already have, but for now I've manually
;; created them.

;; [8/27/21 BWM] Note: based on the TR I think most of the
;; single-instruction nanocodes were automatically generated. Also
;; they provided that multiple nanocodes would be active and ORed
;; togeter so they could separate the decoding for the FROM and TO
;; fields of the microcode into their own small arrays limiting
;; nanocode width. While I like the idea of being as faithful as
;; possible to the original design, nanocode width is not currently an
;; issue so I dispensed with the ORing mechanism and each nanocode
;; decodes its own TO and FROM fields as necessary. That also means
;; that there are probably more explicit nanocodes here than are
;; strictly necessary but again, limiting the size of the
;; nanocontrol-array isn't currently a priority (given our goals
;; here), though that could be revisited for more complex projects in
;; the future.

;; this should be nanocode 0, so we can make sure the nanocode isn't
;; off doing anything when we don't want it to.
(defnano (microlisp-shared::no-op)
  (() ()))

(defnano (microlisp-shared::mover) ; move between registers (specified by the microcode)
    ((from* to*) ()))

(defnano (microlisp-shared::do-car)
  ((from*) (ale))
  ((to*) (read)))

;; should be able to automatically generate this within defnano (microlisp-shared::TBD)
(defparameter *from-to-nano-operations* '(microlisp-shared::write-car
                                          microlisp-shared::write-and-mark-car
                                          microlisp-shared::write-and-unmark-car
                                          microlisp-shared::write-and-mark-cdr
                                          microlisp-shared::write-and-unmark-cdr
                                          microlisp-shared::write-cdr)
  "Keep track of which nano operations use from-to*")

;; note that the notes in the GC section indicate that rplaca/rplacd operations (below) always clear the mark bit.

;; only generated in rplaca
(defnano (microlisp-shared::write-car)
  ((from-to*) (ale))
  ((from* unmark!-bus) (write)))

(defnano (microlisp-shared::write-and-mark-car)
  ((from-to*) (ale clear-gc)) ; setting mark means we can clear the gc-needed pad
  ((from* mark!-bus) (write)))

(defnano (microlisp-shared::write-and-unmark-car :force-add t) ; technically not needed but distinguish for debugging
  ((from-to*) (ale))
  ((from* unmark!-bus) (write)))

(defnano (microlisp-shared::write-and-mark-cdr) ; car in use
  ((from-to*) (ale clear-gc)) ; setting mark means we can clear the gc-needed pad
  ((from* mark!-bus) (write cdr)))

(defnano (microlisp-shared::write-and-unmark-cdr)
  ((from-to*) (ale))
  ((from* unmark!-bus) (write cdr)))

(defnano (microlisp-shared::do-cdr)
   ((from*) (ale))    
  ((to*) (read cdr)))

;; only generated in rplacd
(defnano (microlisp-shared::write-cdr :force-add t) ; distinguish from write-and-unmark-cdr for debugging
    ((from-to*) (ale))
  ((from* unmark!-bus) (write cdr)))

(defnano (microlisp-shared::do-restore) ; pop the stack
   (() (from-stack ale))
   ((to*) (read))
  (() (read cdr to-address-stack to-type-stack)))

;; note these are destination specific; we probably should add
;; mechanism to allow them to be handled via to* destination. (TBD)

(defnano (microlisp-shared::do-set-type-exp)
    ((from*) (to-type-exp)))

(defnano (microlisp-shared::do-set-type-newcell)
    ((from*) (to-type-newcell)))

(defnano (microlisp-shared::do-set-type-val)
    ((from*) (to-type-val)))

(defnano (microlisp-shared::do-set-type-retpc-count-mark)
    ((from*) (to-type-retpc-count-mark)))

(defnano (microlisp-shared::do-set-type-stack)
    ((from*) (to-type-stack)))

(defnano (microlisp-shared::do-set-typec-exp)
    (() (from-type-const* to-type-exp)))

(defnano (microlisp-shared::do-set-typec-newcell)
    (() (from-type-const* to-type-newcell)))

(defnano (microlisp-shared::do-set-typec-val)
    (() (from-type-const* to-type-val)))

(defnano (microlisp-shared::do-set-typec-retpc-count-mark)
    (() (from-type-const* to-type-retpc-count-mark)))

(defnano (microlisp-shared::do-set-typec-stack)
    (() (from-type-const* to-type-stack)))

;; note a push is harder (save) since it involves a cons so it's broken down in pseudo-microcode

;; OK conditionals. Note that the nanomachine doesn't handle
;; conditionals, but what it does do is set up the microcontroller for
;; conditionals. When we have a conditional, if there is a FROM
;; register it gets loaded onto the bus (allowing comparison). The TO
;; field in the microcode is taken as the condition(s) instead of a
;; register. These sense wires are ORed together and affect the low
;; order bit of the microcontroller's NEXT field (what it's PC will be
;; loaded to for the next instruction). For symmetry, the
;; nanocontroller does this setup.

(defnano (microlisp-shared::sense-and-branch)
    ((from*) (set-condition)))

(defnano (microlisp-shared::sense-and-branch-const)
    ((from-const*) (set-condition)))

;; force-add because same as sense-and-branch until we are
;; interpreting; need to modify so that happens via appropriate wires
;; (e.g. elaborate on set-condition or from* form) (TBD)
(defnano (microlisp-shared::sense-type-and-branch :force-add t) 
    ((from*) (set-condition)))

(defnano (microlisp-shared::sense-type-and-branch-const)
    ((from-type-const*) (set-condition)))

(defnano (microlisp-shared::do-simple-get-interrupt-pointer)
    (() (read-interrupt))
  ((to*) (from-interrupt)))

;; I think this is the only specialized one we need BWM 8-11-21
(defnano (microlisp-shared::get-interrupt-pointer-into-stack)
    (() (read-interrupt))
  ((to-address-stack to-type-stack) (from-interrupt)))

;; I wonder if we can do this with a single nanoinstruction if we buffer the increment register?
;; (that would keep it stable during the load of itself with the increment). Since the simulation
;; runs from->bus before bus->to we get the effect of such a buffer so we'll implement it that way.

;; note that in the "original" scheme-79 chip, two nano operations were needed to perform an increment
;; or decrement, but they noted this was because they couldn't add a latch given their constraints.
(defnano (microlisp-shared::do-increment-scan-up)
    (() (from-incremented-newcell to-address-newcell))) ; scan-up is alias for newcell

(defnano (microlisp-shared::do-increment-newcell) ; should be the same, just clearer as to programmer intent
    (() (from-incremented-newcell to-address-newcell))) 

(defnano (microlisp-shared::do-decrement-scan-down)
    (() (from-decremented-exp to-frame-exp to-type-exp to-displacement-exp))) ; scan-down is alias for exp

(defnano (microlisp-shared::do-decrement-frame)
    (() (from-decremented-frame-exp to-frame-exp)))

(defnano (microlisp-shared::do-decrement-displacement)
    (() (from-decremented-displacement-exp to-displacement-exp)))

;; I believe Scheme-79 had a PADS source/destination so read would have PADS in the from field

(defnano (microlisp-shared::do-read-from-pads)
  ((to*) (read))) ; no address - the pads are loaded by external circuitry for debugging

(defnano (microlisp-shared::do-write-to-pads)
  ((from*) (write))) ; no address - the pads are read by external circuitry for debugging
  
(defnano (microlisp-shared::gc-needed!)
    (() (gc-needed))) ; latches until handled

(defnano (microlisp-shared::clear-gc!)
    (() (clear-gc)))

;; dispatch. This pulls from a register onto the bus then sets a
;; control wire that will push the appropriate field into the
;; *micro-pc* (which maybe should be a register itself?)
(defnano (microlisp-shared::type-dispatch)
    ;; want to use the microcode's FROM register but pull out the type
    ;; field and set our micro-pc to that (which should be a jump
    ;; table)
    ((from*-type) (to-micro-pc)))

;; that's all the nano definitions - finalize the array in bitvector form
(eval-when (:load-toplevel)
  (finalize-nanocontrol-array))
