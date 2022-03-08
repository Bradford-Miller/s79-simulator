;; AIM-514 separates out the storage manager
(in-package :scheme-mach)

(scheme-79:scheme-79-version-reporter "Scheme Storage Manager" 0 3 5
                                      "Time-stamp: <2022-02-09 12:37:55 gorbag>"
                                      "line disambiguation")

;; 0.3.5   2/ 9/22 way too many things (fns, variables) with "line" in their name
;;                    and it's ambiguous.  Splitting so "line" refers to,
;;                    e.g. an output (log) line, "expression" refers to a
;;                    'line' of code (single expression in nano or microcode
;;                    land typically, and because we used (READ) it wasn't
;;                    confined to a single input line anyway) and "wire" to
;;                    refer to, e.g., a control or sense 'line' on a register.

;; 0.3.4   2/ 2/22 updates for &car and &cdr and various &rplacd (via macro)

;; 0.3.3   1/31/22 &car and &cdr now check for constituent-fn

;; 0.3.2   1/28/22 &cons now checks for constitutent-highlevel code to generate
;;                    code to copy result.

;; 0.3.1   1/18/22 cleanup obsolete code: removing special treatment of
;;                    registers which required multiple control wires for TO as
;;                    new covering set computation deals with it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.10  1/ 5/22 fix package usage

;; 0.1.9  12/ 3/21 update scheme-79-mcr -> microlisp

;; 0.1.8  11/17/21 use new defrplacop macro to simplify defufn rplac*
;;                    forms

;; 0.1.7  10/11/21 So apparently the heap is NOT a linked list already
;;                    (unlike most lisps!), it is just a chunk of
;;                    memory ending at *memtop*. So when we cons we
;;                    just increment newcell instead of following the
;;                    cdr. More work for GC though because it has to
;;                    consolodate free space instead of just adding a
;;                    spaghetti of free CONS cells to the freelist!
;;                    But CONS can avoid a memory read to get the
;;                    address of the next CONS cell, so not sure it's
;;                    a loss.

;;                ALSO we note that newcell points to the last
;;                    allocated CONS, not the next one, which means we
;;                    don't need to use the IA register for CONS! Win,
;;                    win!

;; 0.1.6   9/17/21 fix typos

;; 0.1.5   9/ 6/21 annotate expansions for mark-* so validator gets register usage right

;; 0.1.4   9/ 3/21 it's not (assign (fetch *foo*)) but (assign *foo*) ;-). Fix 0.1.1 patch

;; 0.1.3   8/26/21 &scan-down=0, mark-car-being-traced, etc.

;; 0.1.2   8/24/21 simplify &cons by using something closer to what's in the existing microcode

;; 0.1.1   8/23/21 various mark bit support defufn-s

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.6 8-18-21 compile-parameter -> compile-embedded-expression

;; 0.0.5 3- 1-21 don't forget to strip register name when generating
;;                  specialized instructions

;; 0.0.4 2-27-21 specialized instructions for &rplaca and &rplacd for
;;                  registers with TO-ADDRESS wires

;; 0.0.3 2-24-21 &cons expansion statistics

;; 0.0.2 1-20-21 protect &cons expansion by making sure we don't reuse
;;                  *intermediate-argument*

;; 0.0.1 1-14-21 Update commments

;; 0.0.0 12-17-20 New

;; separate "processor" in the Scheme-79 chip, responsible for CONS,
;; CAR, CDR, etc. i.e. operations on memory.  limitations in the
;; project kept this simple and GC was not implemented (in the
;; original prototype), though the design should support it.  Memory
;; addresses were also limited to 8 bits due to size limitations on
;; the die (it was a shared project with other students), and words
;; were 11 bits (3 bits of tag) with a total of 256 memory locations.
;; The original SIMPLE design called for 10 bit addresses but this was
;; cut for die size reasons.

;; The Scheme-79 chip of AIM 559 use 32 bit pointers (addresses): 24
;; bits data, 7 bit type and 1 bit used by the storage allocator.

;; (See: AI Memo # 514 & AI Memo # 559)

;; note: see machine-defs.lisp for some accessor functions to registers.

;; Here we will define &cons, &car, &cdr and gc-related
;; functionality. General register support is in machine-defs.lisp

;; note that memory functionality (i.e. reading the external
;; address/data pads, simulating an attached RAM) is in
;; external-support.lisp and the actual driving of the pads is
;; performed by nanocode (see s79-nanocode.lisp)

;; &cons
(defufn &cons (car-expression cdr-expression
                              :args-last t
                              ;; generally both are fetches of registers
                              :expansion ((:to *newcell*)
                                          (:from *memtop* *newcell*))
                              :constituent t)
  ;; do args-last as otherwise both will try to set the
  ;; *from-register* as they are fetches "... *newcell* contains the
  ;; pointer to the beginning of free storage. it is changed by
  ;; CONSing or saving something on the stack. Whenever *newcell* is
  ;; changed, it is compared with *memtop* which contains the (user
  ;; set) memory limit. When these are equal an interrupt is
  ;; signalled, setting the one bit register *gc-needed* indicating
  ;; need for garbage-collection."

  ;; ok, so we assume *newcell* is valid, and is the address of our
  ;; cons cell. We will replace the contents with the specified car
  ;; and cdr, but also update *newcell* to be (increment *newcell*)
  ;; and then compare it to *memtop*.

  ;; stick the cdr of newcell into temporary storage so we can update
  ;; it later (next free cell)
  (let ()
    ;; it should be possible to avoid using IA: assign the to-register
    ;; to the address of the cons, then update newcell, then update
    ;; the cons via the to-register (my original code updated the cons
    ;; via IA).
    
    ;; OK turns out the original code was right, the TO register might
    ;; itself be the stack!

    ;; Rereading the comments on the microcode, I saw this: "Remember,
    ;; *scan-up* is the newcell register for cons. Thus, because mark
    ;; does not disturb it, initially *scan-up* points at the last
    ;; successfully complete cons". So it points to the last completed
    ;; cons, not the next free cell (which is strange since it's called
    ;; "newcell" but oh well). So I'm changing this to increment newcell
    ;; on entry as of 10/11/21.

    (compile-embedded-expression
     `(microlisp:progn   ;; our version NOT CL's
        (&increment-newcell) ; point to next free cons cell
        (&rplaca (fetch *newcell*) ,car-expression) ; update the cons cell
        (&rplacd (fetch *newcell*) ,cdr-expression) ; both halves
        ;; update the TO register to point to the new cons cell address
        ,(assign-constituent-highlevel '(fetch *newcell*)) ; to support more complex assignments
        ;(assign ,*to-register* (fetch *newcell*))
        ;; newcell has the address of the free cell. If that's
        ;; equal to *memtop* it's a problem, because we won't have
        ;; anymore!
        (microlisp:if (&address=? (fetch *newcell*) (fetch *memtop*))
                          (microlisp-shared::gc-needed!))))))
  
;; &car
(defufn &car (address-expression :constituent t :args-last t) ; generally address-expression is a fetch from a register
  ;; *from-register* should be set up by the address-expression
  ;; if we're inside of an assign, we can use that as the to-register, unless we have an enclosing opcode
  ;(declare (ignore address-expression)) ; don't need it yet...

  (cond
   ((eql *enclosing-opcode* 'assign)
    ;; get the from-register
    (compile-parameter nil address-expression nil)
    (write-generated-code *upla-stream* nil
                          `(((from ,*from-register*) (to ,*to-register*) microlisp-shared::do-car))
                          "&car")
    nil) ; code written so return nil
   (*constituent-assignment-fn*
    ;; expand and then do the assignment
    (let ((rewritten-fn (assign-constituent-highlevel `(&car ,address-expression)))
          (*constituent-assignment-fn* nil))
      (compile-embedded-expression rewritten-fn)))
   (t
    (error "&car not handled"))))

;; &cdr
(defufn &cdr (address-expression :constituent t :args-last t) ; like &car, but separate nanocode
  ;(declare (ignore address-expression)) ; don't need it yet...

  (cond
   ((eql *enclosing-opcode* 'assign)
    (compile-parameter nil address-expression nil) ; get the to-register
    (write-generated-code *upla-stream* nil
                          `(((from ,*from-register*) (to ,*to-register*) microlisp-shared::do-cdr))
                          "&cdr")
    nil) ; code written so return nil
   (*constituent-assignment-fn*
    ;; expand and then do the assignment
    (let ((rewritten-fn (assign-constituent-highlevel `(&cdr ,address-expression)))
          (*constituent-assignment-fn* nil))
      (compile-embedded-expression rewritten-fn)))
   (t
    (error "&cdr not handled"))))

;; &rplaca
(defrplacop &rplaca (microlisp-shared::write-car))

;; &rplacd
(defrplacop &rplacd (microlisp-shared::write-cdr))

;; &rplaca-and-mark!
(defrplacop &rplaca-and-mark! (microlisp-shared::write-and-mark-car))

(defrplacop &rplaca-and-unmark! (microlisp-shared::write-and-unmark-car))

(defrplacop &rplacd-and-mark-car-being-traced! (microlisp-shared::write-and-mark-cdr))

(defrplacop &rplacd-and-mark-car-trace-over! (microlisp-shared::write-and-unmark-cdr))

;; &mark-car-being-traced! ;; mark bit stored in cdr
(defufn &mark-car-being-traced! (from-register :expansion ((:to *intermediate-argument*)
                                                           (:from *intermediate-argument*)))
  (with-intermediate-argument
      (compile-embedded-expression
       `(microlisp:progn
          (assign *intermediate-argument* (&cdr (fetch ,from-register)))
          (&rplacd-and-mark-car-being-traced! (fetch ,from-register) (fetch *intermediate-argument*))))))

;; &mark-car-trace-over!
(defufn &mark-car-trace-over! (from-register :expansion ((:to *intermediate-argument*)
                                                         (:from *intermediate-argument*)))
  (with-intermediate-argument
    (compile-embedded-expression
     `(microlisp:progn
        (assign *intermediate-argument* (&cdr (fetch ,from-register)))
        (&rplacd-and-mark-car-trace-over! (fetch ,from-register) (fetch *intermediate-argument*))))))

;; &mark-in-use! ;; mark bit stored in car
(defufn &mark-in-use! (from-register)
  (with-intermediate-argument
    (compile-embedded-expression
     `(microlisp:progn
        (assign *intermediate-argument* (&car (fetch ,from-register)))
        (&rplaca-and-mark! (fetch ,from-register) (fetch *intermediate-argument*))))))

;; &unmark!
(defufn &unmark! (from-register :expansion ((:to *intermediate-argument*)
                                            (:from *intermediate-argument*)))
  (with-intermediate-argument
    (compile-embedded-expression
     `(microlisp:progn
        (assign *intermediate-argument* (&car (fetch ,from-register)))
        (&rplaca-and-unmark! (fetch ,from-register) (fetch *intermediate-argument*))))))

;; gc-needed!
(defufn gc-needed! ()
  `((gc-needed!))) ; direct translation into nanocode (ha!)

;; &clear-gc-needed
(defufn &clear-gc-needed ()
  `((microlisp-shared::clear-gc!)))

;; &scan-up=scan-down?
(defupred &scan-up=scan-down? (address=bus nil (*scan-up* *scan-down*))
  ;; first register is the one we should use for comparison
  ;; body of function of two args: fail-tag and success-tag
  `(((from *scan-down*)
     (branch address=bus-newcell ,fpga-pla-build-tools::fail-tag ,fpga-pla-build-tools::success-tag))))

;; &scan-down=0?
(defupred &scan-down=0? (address=0 nil (*bus* *scan-down*))
  ;; will have to fetch first, but check that in simple-branch-pred
  `(((from *scan-down*)
     (branch address=0-bus ,fpga-pla-build-tools::fail-tag ,fpga-pla-build-tools::success-tag))))
