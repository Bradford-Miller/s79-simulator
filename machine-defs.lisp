(in-package :scheme-mach)

(scheme-79:scheme-79-version-reporter "Scheme Machine Sim Defs" 0 4 0
                                      "Time-stamp: <2022-03-18 15:29:06 gorbag>"
                                      "force type field on intermediate-argument for console presentation")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.4.0   3/18/22 snapping a line: 0.4 release of scheme-79 supports test-0 thru test-3. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.3.9   3/15/22 force type field on intermediate-argument for console
;;                     presentation purposes (debugging)

;; 0.3.8   2/24/22 add get-type-bits, get-frame-bits, get-displacement-bits,
;;                     get-address-bits

;; 0.3.7   2/23/22 add from-type to retpc-count-mark

;;         2/21/22 add comment about FROM-TYPE specialness

;; 0.3.6   2/18/22 break out displacement and frame field lengths as parameters
;;                    add from-type to *exp*

;; 0.3.5   2/11/22 increase micro-pc size to 11 bits (from 10) to support larger
;;                    microprogram of test-2.

;; 0.3.4   2/ 9/22 way too many things (fns, variables) with "line" in their name
;;                    and it's ambiguous.  Splitting so "line" refers to,
;;                    e.g. an output (log) line, "expression" refers to a
;;                    'line' of code (single expression in nano or microcode
;;                    land typically, and because we used (READ) it wasn't
;;                    confined to a single input line anyway) and "wire" to
;;                    refer to, e.g., a control or sense 'line' on a register.

;; 0.3.3   1/26/22 add *breakpoint-descs* to have different kinds of
;;                     breakpoints

;; 0.3.2   1/24/22 add from-decremented-frame and
;;                     from-decremented-displacement to *exp*

;; 0.3.1   1/24/22 add from-displacement and from-frame to *val*
;;                     register to implement &val-frame-to-exp-frame
;;                     and &val-displacement-to-exp-displacement.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.15 12/ 3/21 repatriate some vars and fns to support

;; 0.1.14 11/23/21 move zerop-field, decrement-field and increment-field to common

;; 0.1.13 11/12/21 move break-out-bits-as-integers here (from s79-console)

;; 0.1.12 10/26/21 set up *control-wires* *sense-wires* to help
;;                     "automate" some definition generation

;; 0.1.11 10/21/21 nano-pc now a bit vector

;; 0.1.10 10/20/21 make-register now takes an optional size argument
;;                     (in bits)
;;                 define *micro-pc* as a register (needed
;;                     for clocked logic anyway), with TO control
;;                 add from-type register flag so we can move things
;;                     from the type filed using nanocode

;; 0.1.9  10/12/21 move shift-right fn to common.lisp

;; 0.1.8  10/ 7/21 add *breakpoints* a list of u-addresses that stop
;;                    'run' processing.

;; 0.1.7   9/24/21 add type and address fields for *memory*
;;                    pseudo-register for the console

;; 0.1.6   9/21/21 add not-mark-bit flag

;; 0.1.5   9/14/21 *address-field-mask*

;; 0.1.4   9/13/21 define (setf) register-mark!-p etc. to use existing
;;                    mark-bit but have the accessors used by the
;;                    nanocode

;; 0.1.3   9/ 7/21 type field was off by one bit (displacement)

;; 0.1.2   8/31/21 type=pointer on bus (from paper)

;; 0.1.1   8/22/21 differentiate microcode 'cond' from cl:cond

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.17     8-16-21 *nanocontrol-wire-next-initial-value* instead of a constant

;; 0.0.16     7-20-21 *input-pad-types* *output-pad-types*

;; 0.0.15     7- 6-21 Move test-pad-immediate here from external-support.lisp

;; 0.0.14     6-26-21 Consolodate clock related fns and variables to clock.lisp

;; 0.0.13     6- 5-21 *all-ph-pre-update-list* *all-pad-names*

;; 0.0.12     4-26-21 eliminate *freeze-state* - just read the pad

;; 0.0.11     3- 2-21 delete some obsolete functions (get increment and decrement)

;; 0.0.10     2-22-21 reset-sense-wire-encoding

;; 0.0.9      2-19-21 cl:progn (differentiate from our ucode version)

;; <na>       2-14-21 comments on get-pc-ucode and other microcode symbol lookup fns.

;; 0.0.8      1-25-21 add *tick* so we can count number of major cycles

;; 0.0.7      1-22-21 declaim **pointer-types** etc. as special as they
;;                      won't be established until later

;; 0.0.6      1-19-21 implement symbol->int and int->symbol operations for
;;                      types (from microcode defschip declarations)

;; 0.0.5      1-16-21 move pseudo register declarations here to insure
;;                      correct reset of nanocontrol wires and expansion

;; 0.0.4      1-12-21 PCs for micro and nano machines, as well as access
;;                       functions for the micro and nano code (skeletal)

;; 0.0.3      1- 8-21 moved register-flag-accessor to predefs

;; 0.0.2      1- 7-21 clock

;; 0.0.1     12-23-20 define registers

;; Closer to a hardware simulator corresponding with AIM 559
;; based on the published microcode and description

;; here we implement the various described mechanisms on the chip which we are simulating.

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

(defvar *halt-address* nil
  "If this is set to a microcode address, it will cause a STOP if the
machine was in the RUN state")

(defvar *breakpoints* nil
  "List of microcode addresses that should cause a break. This is
similar to a HALT but does not attempt to update statistics, warn
about further processing, etc. Set a breakpoint using the 'Run-Until'
button or the set-breakpoint fn.")

(defvar *breakpoint-descs* nil
  "List of breakpoint descriptors. This is to allow us to set 
'permanent' breakpoints in the microcode for debugging certain ufns.")

;; pointer is 32 bits with 3 fields: 24 bit data, 7 bit type, and 1
;; bit (in-use mark) used by storage allocator.

;; NB: the sbit/bit operation puts the high order bit (the one with the largest
;;     offset) into the lowest bit based on integer->bit-vector.  SO we try to
;;     be consistent here:
;;
;; (s)BIT: 0     1 .. 7   8     ..  19  20-31
;;       Mark  ~ptr      DISPLACEMENT  FRAME
;;               -TYPE-   ----  ADDRESS  ----

;; note that these create aliases (displaced arrays) so you can set
;; bits in these and they will set the appropriate bits in the
;; underlying word; much simpler (at this level anyway) than doing
;; shifts and trims!

(defparameter *type-field-length* 7)

(defun make-type-field (x)
  (make-array *type-field-length* :element-type 'bit
                                  :displaced-to x
                                  :displaced-index-offset 1))

(defparameter *displacement-field-length* 12)

(defparameter *frame-field-length* 12)

(defparameter *address-field-length* (+ *displacement-field-length* *frame-field-length*))

(defparameter *address-field-mask* #0xffffff)

(defparameter *data-field-length* *address-field-length*)

(defparameter *type-bit-field-end* (- *word-size* *address-field-length*))

(defparameter *displacement-bit-field-end* (- *word-size* *frame-field-length*))

(defun make-data-field (x)
  "Also used for the address field"
  (make-array *data-field-length* :element-type 'bit
                                  :displaced-to x
                                  :displaced-index-offset 8))

;; for those registers that have it
(defun make-displacement-field (x)
  (make-array *displacement-field-length* :element-type 'bit
                                          :displaced-to x
                                          :displaced-index-offset 8))

(defun make-frame-field (x)
  (make-array *frame-field-length* :element-type 'bit
                                   :displaced-to x
                                   :displaced-index-offset 20))

;; for when we don't have fields
(defun get-type-bits (x)
  (subseq x 1 *type-bit-field-end*))

(defun get-displacement-bits (x)
  (subseq x *type-bit-field-end* *displacement-bit-field-end*))

(defun get-frame-bits (x)
  (subseq x *displacement-bit-field-end*))

(defun get-address-bits (x)
  (subseq x *type-bit-field-end*))

;; to help print out 32 bit values as separate fields
(defun break-out-bits-as-integers (value)
  ;; if input is an integer, convert to bit-vector
  (let ((raw-value (cl:if (integerp value)
                     (integer->bit-vector value :result-bits *register-size*)
                     value)))
    (let ((mark (mark-bit raw-value))
          (ptr (pointer-bit raw-value))
          (type (bit-vector->integer (get-type-bits raw-value))) ; include pointer bit
          (disp (bit-vector->integer (get-displacement-bits raw-value)))
          (frame (bit-vector->integer (get-frame-bits raw-value)))
          (addr (bit-vector->integer (get-address-bits raw-value))))
      (values mark ptr type disp frame addr))))

;; list node (a cons) has two pointers (called CAR and CDR). This is
;; the unit of memory allocation!

;; the bus is 32 bits. (A bit of a limit - particularly with only one
;; bus!). For simulation purposes, we will treat the bus as a special
;; kind of register, and other registers must only write to the bus or
;; read from the bus.

;; we should probably get this from the microcode (defreg form) but
;; for now we define these here so we can refer to the registers in
;; the hardware instructions before the microcode is read in. Still we
;; will make the syntax like defreg and attempt consolodation later.

;; define these in common, and define on each register symbol the ones
;; that are valid for that register. Again, note these are (mostly)
;; from the microcode.mcr file, but that gets read too late in the
;; process at this point; I'm guessing on further inspection that
;; those are strictly for the lisp simulator and not part of the chip
;; building process... so we can be free to configure these for our
;; FPGA process here (or in a new FPGA project-specific file... TBD)

;; 10/26/21 write up the names of the sense and control wires
;; separately so we can refer to them in other parts of the code (and
;; move toward having only one place to establish a new control or
;; sense)

;; 10/20/21 added from-type so we can move things from the type field
;; of the bus using nanocode

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *control-wires*
    '(to to-type to-displacement to-frame to-address from
      from-decremented from-incremented from-decremented-frame
      from-decremented-displacement
      from-type mark! unmark! pointer! type!))

  (defparameter *sense-wires*
    '(address=bus type=bus =bus mark-bit not-mark-bit type-not-pointer
      type=pointer type=type frame=0 displacement=0 address=0))

  (eval `(defflags register ,@*control-wires* ,@*sense-wires*))

  ;; note that many of these control wires define covering sets for
  ;; others.  so if we want to write TO a register, but it only has
  ;; TO-TYPE and TO-ADDRESS controls, we can use the latter to
  ;; substitute for the former. Declare those relationships here.
  (reset-covering-set-alist)
  (declare-covering-set 'to 'to-type 'to-address) ; ignore mark-bit - handled separately regardless
  
  (declare-covering-set 'to-address 'to-displacement 'to-frame)
  ;; NB: we don't cover FROM because every registers from which we can legit
  ;; copy the whole register already has FROM declared. And from-type takes
  ;; advantage of that to copy bits directly to the low order bits on the bus!
  ;; (TBD: in the chip these bits were handled separately from the bus with
  ;; their own MUX into the Micro-PC register. The current version of the
  ;; emulator doesn't do that to take advantage of the existing infrastructure
  ;; but we should add that to a future release, effectively setting up a
  ;; parallel bus (and we'll want to handle multiple buses in the future
  ;; anyway!))
  )

;; if we're recompiling this file, we need to reset the counter for nanocontrol bits, lest they get out of hand
(eval-when (:compile-toplevel)
  (setq *nanocontrol-wire-next* *nanocontrol-wire-next-initial-value*)
  (reset-sense-wire-encoding))

(defchip-reg *bus*)
;; added control wires for setting/unsetting special bits on the bus
;; also add not-mark-bit (presumably we'd have an inverter in the fpga
;; so wouldn't need another sense wire, but this is closer than using
;; logic)
(defureg *bus*
    (mark! unmark! type! pointer!)
  (mark-bit not-mark-bit type=pointer type=type frame=0 displacement=0 address=0))

;; bus doesn't really have proper declarations
(defvar *bus-frame* (make-frame-field *bus*))

(defvar *bus-displacement* (make-displacement-field *bus*))

(defvar *bus-data* (make-data-field *bus*))

(defvar *bus-type* (make-type-field *bus*))

(defvar *bus-address* *bus-data*) ;alias

(eval-when (:load-toplevel :execute)
  (mapc #'export-ulisp-symbol '(*bus-frame* *bus-displacement* *bus-data* *bus-address* *bus-type*)))

;; there are 10 registers, which have specialized
;; characteristics. Registers and operators are all on the same bus

;; MEMTOP - the top of the allocated memory
(defchip-reg *memtop*)
(defureg *memtop* (to from) ())

;; NEWCELL - a pointer to the beginning of free storage
(defchip-reg *newcell*)

(defchip-reg *scan-up* *newcell*)

(defureg *newcell* (to-type to-address from from-incremented) (address=bus))

;; EXP the current expression (*scan-down* is an alias)
(defchip-reg *exp*)
(defchip-reg *scan-down* *exp*)

;; BWM add from-decremented-frame and from-decremented-displacement 1/24/22
;; BWM add from-type 2/18/22
(defureg *exp* (to-type to-displacement to-frame
                from from-decremented from-decremented-frame from-decremented-displacement from-type) ())

;; VAL the value of the expression when determined
(defchip-reg *val*)
(defchip-reg *rel-tem-2* *val*)
(defchip-reg *stack-top* *val*)

;; added from-displacement and from-frame to implement &val-frame-to-exp-frame, etc. 1/24/22 BWM
(defureg *val* (to-type to-address from from-displacement from-frame) (type=bus address=bus =bus))

;; RETPC-COUNT-MARK "used in increment operations to store
;; intermediate values because our registers are not dual rank. It is
;; also used for storing microcode return addresses for use in
;; microcode subroutines"
(defchip-reg *retpc-count-mark*)
(defureg *retpc-count-mark* (to-type to-address from
                                     from-type) ()) ;; added 2/23/22

;; ARGS when args for a procedure call are being evaluated at each
;; step the result in VAL is added to the list of already evaluaged
;; arguments
(defchip-reg *args*)
(defureg *args* (to from) ())

(defchip-reg *leader* *args*)
(defchip-reg *rel-tem-1* *args*)

;; when the args are all evaluated, the procedure is invoked. This
;; requires that the formal parameters be bound to the actuals. The
;; occurs by DISPLAY gets the contents of the ARGS register prefices
;; to the environment pointer of the closed procedure being applied

;; when evaluating the args to a proc, the evaluator may have to
;; recurse to obtain the value of a subexpression. The evaluator state
;; will have to be restored when the subexpression returns with a
;; value, requiring the state be saved before recursion. The evaluator
;; maintains a pointer to the stack of pending returns and associated
;; state in STACK
(defchip-reg *stack*)
;; 10/21/2021: add from-type to handle dispatch-on-stack
(defureg *stack* (to-type to-address from from-type) ())

;; DISPLAY - used by EVAL
(defchip-reg *display*)
(defureg *display* (to from) ())

(defchip-reg *node-pointer* *display*)

;; INTERMEDIATE-ARGUMENT used by microcode compiler for storing anonymous temporaries
(defchip-reg *intermediate-argument*)
(defureg *intermediate-argument* (to from) () t) ; force type field for debugging (will present in console)

;; NIL a pointer to nil - it's a dummy register and can't be set (in the hardware, anyway).
(defchip-reg *nil*)
(defureg *nil* (from) ())

;; also set up some pseudo-registers used to facilitate dealing with the external pads (see the AIM)

;; "The external world is conceptualized as a set of registers with
;; special capabilities."

;; "The external ADDRESS register is used for accessing memory and can
;; be set from the bus."

;; [looks like a register but it's not?!]
(defchip-reg *address*) ;; really only 24 bits, but we can't specify that 
(defureg *address* (to from) ()) 

;; "The pseudo-register MEMORY can be read onto the bus or written
;; from the bus. The actual access is preformed to the list cell
;; addressed by the ADDRESS register. The CDR bit controls which half
;; of the cell is being accessed."

(defchip-reg *memory*)
;; add to-type and to-address for the console
(defureg *memory* (to to-type to-address from) ())

;; "One more external register, INTERRUPT, which can be read onto the
;; bus, contains the address of a global symbol whose value (its CAR)
;; is an appropriate interrupt handler."

(defchip-reg *interrupt*)
(defureg *interrupt* (from) ())

;; internal to the chip we have a pointer to the current nano-op as
;; well as to the current u-op, effectively our internal state when
;; combined with the contents of the registers

(defparameter *nano-pc-size* 10
  "bits in the nanopc")

(defparameter *nano-pc-max-address*
  (expt 2 *nano-pc-size*)) ; allows us to check in nano-assembler we
                           ; have large enough

;; update this to a special register
(defchip-special-reg *nano-pc* *nano-pc-size*) ;;program counter for the nano-operation

;; 10/20/21 - make the micro-pc a register for conformance to hardware implementation
;;            and to support loading it from the nanocode (e.g. dispatch) to get the
;;            timing right
;;  2/11/22 - increase from 10 to 11 bits to support test-2.
(defparameter *micro-pc-size* 11
  "bits in the micropc")

(defparameter *micro-pc-max-address*
  (expt 2 *micro-pc-size*)) ; allows us to check in micro-assembler we
                            ; have large enough pc

(defchip-special-reg *micro-pc* *micro-pc-size*) ;;program counter for the micro-operation
(defureg *micro-pc* (to) ()) ; so we can move a field TO the PC

;; these will be defined by microcode file
(declaim (special **pointer-types** **non-pointer-types** **pointer**))


;; lookup functions into *microcontrol-array* using *microcontrol-symtab* or symbol based properties
(defun get-return-ucode (symbol)
  (tbd symbol))

(defun get-type-ucode (symbol)
  (tbd symbol))

(defun get-pc-ucode (symbol)
  (tbd symbol))

(defun get-microcode (symbol)
  "given the symbolic name of a type or return, this gets the
associated offset into the code/controller table. Note if you know
what kind of symbol it is (pc, type, or return), the more specific fn
is more efficient."
  ;; if this were a real machine, we'd not worry about anything
  ;; symbolic and drive this off of addresses in the microcode PROM
  ;; array.
  (or (get-type-ucode symbol)
      (get-return-ucode symbol)
      (get-pc-ucode symbol)))

(defun get-nanocode (symbol)
  "given the symbolic opcode for a bit of naocode, this retreives the
associated offset into the code/controller table"
  ;; similar to get-microcode, this is not exactly how the real
  ;; hardware would work. We'll get to that level of emulation soon I
  ;; hope, but this has the advantage of being able to present the
  ;; actual source code and then the machine version of that
  ;; (essentially setting control wires to be emulated that moves data
  ;; around the registers and pads)
  (cdr (assoc symbol *nanocontrol-symtab*)))
