(in-package :scheme-mach)

(scheme-79:scheme-79-version-reporter "Scheme Machine Sim Int Ops" 0 4 1
                                      "Time-stamp: <2022-04-07 11:55:40 gorbag>"
                                      "repatriating")

;; 0.4.1 4/ 7/22 repatriating defufns, etc. that are used to build the PLA from
;;                  code used for the simulator or define the machine itself.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.4.0   3/18/22 snapping a line: 0.4 release of scheme-79 supports test-0 thru test-3. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.3.10  3/14/22 frame=0? and displacement=0? should get *exp* into *bus* before checking.

;; 0.3.9   3/ 8/22 ok we have enough info to start guessing what eval-exp-popj-to
;;                    should look like. Set up to dispatch on the type of exp
;;                    after stuffing the tag onto the stack for the dispatch to
;;                    continue to. Note at this point we have a symbol-pointer in
;;                    exp (based on our current test-2.lisp) which might really need
;;                    to be the closure and this will help us determine.

;; 0.3.8   2/ 9/22 way too many things (fns, variables) with "line" in their name
;;                    and it's ambiguous.  Splitting so "line" refers to,
;;                    e.g. an output (log) line, "expression" refers to a
;;                    'line' of code (single expression in nano or microcode
;;                    land typically, and because we used (READ) it wasn't
;;                    confined to a single input line anyway) and "wire" to
;;                    refer to, e.g., a control or sense 'line' on a register.
;;                  since &global-value translates into &car, it should be 
;;                     :args-last as &car is.

;; 0.3.7   1/28/22 assign now returns the actual assembly code rather
;;                     than just setting up the to-register in order
;;                     to simplify how we can handle complex second
;;                     arguments

;; 0.3.6   1/27/22 have &global-value call &car to get it's defn, rather
;;                     than invoking a subcompilation.

;; 0.3.5   1/26/22 add dispatch, &global-value, micro-call, micro-return

;; 0.3.4   1/25/22 add new &eq-val?, &frame=0?, and &displacement=0?
;;                     predicates

;; 0.3.3   1/24/22 add mask-interrupts pseudo-pad

;; 0.3.2   1/18/22 cleanup obsolete code: removing special treatment of
;;                    registers which required multiple control wires
;;                    for TO as new covering set computation deals
;;                    with it.

;; 0.3.1   1/13/22 change references from internal-freeze to run-nano
;;                    for consistancy with AIM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.14  1/ 7/22 moved defufn of tag here (rather than ucode-compiler) since it
;;                     "belongs" with go-to.

;; 0.1.13  1/ 5/22 fix reference to scheme-79-mcr-i as fn is now exported from
;;                     microlisp-int and inherited
;;                 add explicit packages for microlisp symbols and intern calls

;; 0.1.12 12/14/21 use new special-register-p fn

;; 0.1.11 10/11/21 Add increment-necell - same as increment-scan-up
;;                     but intent is clearer (and in case we ever
;;                     separate the registers!)

;; 0.1.10  9/20/21 Define some inverse sense bits

;; 0.1.9   9/ 4/21 &set-type handle from-register (not only constants!)

;; 0.1.8   9/ 3/21 &set-type must convert the symbolic type constant to numeric

;; 0.1.7   8/31/21 &pointer? should decode to type=pointer
;;                    (see hint in paper where type=pointer-bus is used in a figure)

;; 0.1.6   8/30/21 add &set-type case to fetch

;; 0.1.5   8/27/21 dispatch-on-stack

;; 0.1.4   8/26/21 &decrement-scan-down

;; 0.1.3   8/24/21 add &address=? for &cons rewrite

;; 0.1.2   8/22/21 cond

;; 0.1.1   8/21/21 &set-type

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.11    8-18-21   compile-paramater -> compile-embedded-expression

;; 0.0.10    7- 7-21   add *internal-freeze* psudo pad to distinguish from
;;                       externally driven freeze (i.e. we can clear
;;                       it safely) [1/13/22 now run-nano]

;; 0.0.9     3- 1-21   restore should generate specialized instructions for 
;;                       *special-registers*

;; 0.0.8     2-27-21   check for alias on to register when doing move

;; 0.0.7     2-23-21   create a pseudo-pad to deal with conditionals 
;;                       (not external to chip)

;; 0.0.6     2-19-21   define our own ufun version of progn

;; 0.0.5     2-15-21   save ufun shoudl be :args-last so it can grab 
;;                       the register and pass to &cons

;; 0.0.4     2-14-21   &increment-scan-up implemented

;; 0.0.3     2-13-21   go-to implemented

;; 0.0.2     1-18-21   comment out prior simulation; will reimplement in 
;;                        terms of new nanocode operations!

;; 0.0.1    12-23-20   &set-type (retracted 1-18)

;; Closer to a hardware simulator corresponding with AIM 559
;; based on the published microcode and description

;; here we implement the various described mechanisms on the chip
;; which we are simulating.

;; Only the microcontroller has the ability to have conditional
;; statements (the nanocontroller does not).  "When a branch occurs,
;; the condition being tested is developed as a boolean bit value
;; which is then merged into the low bit of the next state value. This
;; means that the two targets of the brnach must be allocated in an
;; even/odd pair of states."

;; instructions NOT directly related to storage management:

;; pseudo pad to deal with conditionals - really switches MUX on least
;; order bit of micro-pc to OR of relevant conditional wires

;nano runs on :ph2-rising, and we keep high until we update PC on :ph1-falling
(defchip-pad *conditional* :pseudo *run-nanocontroller-p1* *update-sense-wires* 7)

;; pseudo pad for the internal version of freeze. So we can distinguish between an internally
;; driven freeze (nanocode) and external (generally memory cycle delay)

;; Renamed to run-nano to be consistent with the AIM (1/13/22 BWM)
(defchip-pad *run-nano* :latched-io *run-nanocontroller-p1* :any 8) ; full clock cycle as we may reassert

;; instruction DISPATCH-ON-EXP-ALLOWING-INTERRUPTS implies there is some mask of the interrupt-request
;; pad, so we create this pseudo-pad to suppress interrupts.
(defchip-pad *mask-interrupts* :latched-io :any :any 3) ; should match up with *interrupt-request*

