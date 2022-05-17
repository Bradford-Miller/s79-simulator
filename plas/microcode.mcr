;; (in-package :scheme-79-mcr) ; this should be in the reader - this is not standard common lisp!

;; also never read or printed, here for documentation purposes only
;; (thought it might make sense to have some kind of version number
;; that can be accessed from the machine!)

;;(scheme-79:scheme-79-version-reporter "S79 Microcode" 0 4 0
;;                                      "Time-stamp: <2022-05-02 16:44:51 gorbag>"
;;                                      "*stack* from-type")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.4.0   3/18/22 snapping a line: 0.4 release of scheme-79 supports test-0 thru test-3. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.3.1 1/17/22 added from-type for type dispatch (new code paying
;;                   more attention to declarations!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.1.1  9/20/21  update comments on defreg bus
;; 0.1.0  8/20/21  "official" 0.2 release: test-0 passed!
;;        2/22/21  fix typo in a comment
;; 0.0.7  2/ 6/21  notes on the debug-routine
;; 0.0.6  1/12/21  notes on funny 9 bit types (no code changes)
;; 0.0.5 12/ 8/20  typos (more getting caught by validator!)
;; 0.0.4 12/ 6/20  more typos and bad character (opt-8?)
;; 0.0.3 12/ 5/20  another typo (rEplacd?)
;; 0.0.2 12/ 2/20  more typos (even my primitive validator caught them)
;; 0.0.1 12/ 1/20  fix typeo (spurious #\,)

;; (in-readtable ucode-syntax) ; to deal with the commas - this should be in the reader.
;; MIT called this "micro lisp" so some minor variations with standard lisp.

;; What follows is a more or less faithful transcript of the microcode from
;; AIM 559: this was for the actual SCHEME-79 chip instead of the SIMPLE
;; prototype produced during Steele's class (which apparently had an error
;; in fabrication preventing it from running). 

;; Some notes, cribbed shamelessly from AIM 559 (many are direct quotes!)

;; data paths consist of a set of special purpose registers (q.v.)  with
;; built-in operators, interconnected with a single 32 bit bus

;; there are 10 registers, shared by the interpreter and GC to save
;; space. (The interpreter cannot run while a GC is taking place, but note
;; cites to fix that). Registers and operators are all on the same bus. (See
;; Figure 3)

;; the first bit in most registers is the MARK bit and in many registers this
;; is followed by a 7 bit type field. The remainder of the 32 bits (24 bits)
;; is an address or in the case of EXP the address is split into 2 12 bit
;; fields, the displacement and frame.  This allows some microcode to enable
;; TO-DISPLAEMENT-EXP and FROM-RETPC-COUNT-MARK which transfers the
;; DISPLACEMENT part of the data field of the the RETPC-COUNT-MARK register
;; or the DISPLACEMENT part of the EXP register.

;; The data part of the EXP register can be decremented and placed on the
;; bus; the DISPLACEMENT and FRAME subfields of the bus can be separately
;; tested for zero - this is used to implement lookup of variables by
;; lexical address. Decrementing the data field of EXP is used as a 'scan
;; down' pointer in teh sweep phase of GC. NEWCELL, which cointains the free
;; storage pointer can be read on the bus directly or the incremented value
;; can be placed on the bus. This is also used to scan up in memory during
;; GC sweep. NEWCELL also continually tests if its data field is equal to
;; the data field on the bus, and the comparison is used for checking during
;; allocation to see if the available memory limit has been exceeded.

;; The bus is exteded off chip through pads. The external world is
;; conceptualized as a set of registers with special capabilities. The
;; external ADDRESS register is used to access memory and set from the
;; bus. The pseudo-register MEMORY can be read on to the bus or written from
;; the bus. The actual access is performed to the list cell addressed by the
;; ADDRESS register, with the CDR bit controlling which half of the cell is
;; being acessed. INTERRUPT is an external register that can be read onto
;; the bus and contains the address of a global symbol whose value (its CAR)
;; is an appropriate interrupt handler.

;; Code compactness leveraged a nano control map to limit the number of
;; combinations of register controls in any one micro instruction.  since a
;; single micro instruction may represent several nano instructions, the
;; micro sequencer is frozen while the nano-sequencer is running, used to
;; allow for memory wait states as well.

;; the nano engine was a full stater machine (see Figure 6), but could not
;; perform conditional branch operations. Subsequences in the microcode
;; could be subroutinized by the conventional use of a free register to hold
;; the return micro address in its type field.

;; micro instructions have the form source register (from ...), destination
;; register (to ...), a set of operations and controls and possible branch
;; conditions.

;; the nano PLA does 3 separate decoding functions: the from, to and
;; operation fields of the microinstruction. (Each compiled separately: the
;; assignment of numbers to each register/constant is therefore unrelated).

;; the operation field of the micro-instruction selects the
;; nano-instruction; the nano instruction can then selectively enable
;; decoding of the from and to fields. For single-word nano-routines, both
;; are automatically enabled, multiple-word nano-routines are predefined and
;; specify when to enable the decodings.

;; microcode branches are devloped where the condition beting tested is a
;; boolean bit merged into the low bit of the next state value, therefore
;; the two targets of the brnch must be allocated in an even/odd pair of
;; states. If a given micro-word is the target of more than one branch, then
;; several copies may have to be made (e.g, one in an odd location, the
;; other in an even). Additionally, addignment of states has to be
;; compatible with the assignment of type numbers (the S-code operations)
;; used by the micro state dispatcher.

;; the following microcode is a direct transcription from the appendix
;; to the AIM (original OCR then fixed manually, some of the line
;; breaks and whitespace were not preserved exactly, but I did
;; the best I could and it should have no effect on the code itself.


;; Appendix - The SCHEME-79 Microcode

;; In this appendix we present a complete listing of the microcode for the
;; SCHEME-79 chip. in this listing, symbols surrounded by single asterisks
;; (e.g. *nil*) are names or aliases of machine registers. Symbols begin-
;; ning with ampersand (&) are the names of hardware operations. The S-code
;; (macro) opcodes are (some of) the data types. These are given specific
;; numerical values. When a DEFFYPE is used to define the microcode that is
;; used when that type is executed, I usually place a comment showing how
;; that data type is expected to be used in a piece of S-code. Some data
;; types are pointer types (meaning that the garbage collector must mark the
;; thing pointed at by the address part) and others are immediate data
;; (hence terminal to the garbage collector). The folowing are the data
;; types with preassigned numerical values:

;;; [I'm guessing these numerical constants are octal! BWM]

(defschip **pointer-types**
    '((self-evaluating-pointer 0) 
      (symbol 1)
      (global 2)
      (set-global 3)
      (conditional 4)
      (procedure 5)
      (first-argument 6)
      (next-argument 7)
      (last-argument 10)
      (apply-no-args 11)
      (apply-1-arg 12)
      (primitive-apply-1 13)
      (primitive-apply-2 14)
      (sequence 15)
      (spread-argument 16)
      (closure 17)
      (get-control-point 20)
      (control-point 21)
      (interrupt-point 22)
      (self-evaluating-pointer-1 23) 
      (self-evaluating-pointer-2 24) 
      (self-evaluating-pointer-3 25) 
      (self-evaluating-pointer-4 26)
      ))

(defschip **non-pointer-types** 
   '((self-evaluating-immediate 100)
     (local 101)
     (tail-local 102)
     (set-local 103)
     (set-tail-local 104) 
     (set-only-tail-local 105)
     (primitive-car 106) ;Built-in operators
     (primitive-cdr 107) 
     (primitive-cons 110) 
     (primitive-rplaca 111) 
     (primitive-rplacd 112) 
     (primitive-eq 113)
     (primitive-type? 114) 
     (primitive-type! 115) 
     (gc-special-type 116) ;Never appears except during gc
     (self-evaluating-immediate-1 117)
     (self-evaluating-immediate-2 120) 
     (self-evaluating-immediate-3 121) 
     (self-evaluating-immediate-4 122) 
     (mark 123)
     (done 124)
     (primitive-add1 125)
     (primitive-sub1 126)
     (primitive-zerop 127)
     (primitive-displacement-add1 130) 
     (primitive-not-atom 131)
     (boot-load 776)                     ;micro-address forced by RESET
     (process-interrupt 777)             ;micro-address forced by EXT INT RQ
     ))

;; [[ So a note on the above, given the type field is 6 bits, the 7th
;;    bit is the (non) pointer bit, and the 8th bit is the mark bit,
;;    777 is 9 bits - whaaat? So I'm thinking these are not only the
;;    values of the type array plus pointer bit, but also offsets into
;;    a dispatch table; the funny addresses might only be generated by
;;    the hardware under the condition of those pins being activated
;;    and never from actual register contents.]]

;; Pointer data is indicated by the 100 bit being off in the data type.

(defschip **pointer** 100)                    ;100 bit means non-pointer.

;; Next we find the definitions of the registers. The following registers
;; are used by EVAL: *val*, *exp* , *args*, *display* , *stack*, in
;; addition, *newcell* contains the pointer to the beginning of free
;; storage. it is changed by CONSing or saving something on the
;; stack. Whenever *newcell* is changed, it is compared with *memtop* which
;; contains the (user set) memory limit. When these are equal an interrupt
;; is signalled, setting the one bit register *gc-needed* indicating need for
;; garbage-collection.  *Nil* is a dummy register, it cannot be set, and its
;; value is nil. The garbage collector uses the following registers:
;; *stack-top*, *node-pointer*, *1oader* are used by the GC mark phase
;; only. *Scan-up* *scan-down* and *memtop* are used by the GC sweep
;; phase. *Rel-tem-1* *rel-tem-2* are temporaries used in GC relocate phase
;; only. *Intermediate-argument* is used by microcode compiler for storing
;; anonymous temporaries and *retpc-count-mark* is used in increment
;; operations to store intermediate values because our registers are not
;; dual rank. It is also used for storing microcode return addresses for use
;; in microcode subroutines. There is overlap in the use of the registers,
;; for example, *scan-up* is the *newcell* pointer when in EVAL. Thus the
;; above names are really aliases for the real underlying physical
;; registers. The mapping is made below:

(defschip **machine-registers** 
    '((*nil*)
      (*memtop*)
      (*newcell*)
      (*scan-up* *newcell*) 
      (*exp*)
      (*scan-down* *exp*) 
      (*val*)
      (*rel-tem-2* *val*) 
      (*stack-top* *val*) 
      (*args*)
      (*leader* *args*) 
      (*rel-tem-1* *args*) 
      (*display*)
      (*node-pointer* *display*)
      (*stack*)
      (*retpc-count-mark*) 
      (*intermediate-argument*)))

;; Each physical register has certain capabilities which are controlled by
;; particular control wires which go into it. Thus the to-displacment wire
;; on the *exp* register will, if raised, allow the displacement field of
;; the register to be set from the corresponding field of the bus. The
;; registers also develop certain conditions which are reflected by the
;; states of sense wires. So, for example, the type=bus wire coming out of
;; the *val* register indicates if the type field of the register is equal
;; to the corresponding field on the bus. The following expressions define
;; the control lines and sense wires on the registers.

(defreg *exp* (to-type to-displacement to-frame from from-decremented
               from-type from-decremented-frame from-decremented-displacement) ;; additions BWM 1/24/22, 2/18/22
  ())

(defreg *newcell* (to-type to-address from from-incremented) (address=bus))

(defreg *val*
    (to-type to-address from from-displacement from-frame) ; BWM added from-displacement and from-frame 1/24/22
    (type=bus address=bus =bus))               ;=bus is AND of type, address=bus

(defreg *retpc-count-mark* (to-type to-address from) ())

(defreg *stack* (to-type to-address from from-type) ()) ; BWM added from-type for type dispatch

(defreg *memtop* (to from) ())

(defreg *args* (to from) ())

(defreg *display* (to from) ())

(defreg *intermediate-argument* (to from) ())

(defreg *nil* (from) ())

;;  Additionally, the bus is sensitive to several conditions:

(defreg bus
   () (mark-bit type-not-pointer frame=0 displacement=0 address=0))
   
;; [I'm redefining this defreg for the bus slightly - adding mark-bit,
;;  and type-not-pointer as controls as well which allows us to force
;;  them on or off on the bus prior to writing them to memory. This
;;  version appears in machine-defs.lisp and I'm showing it here for
;;  completeness]. 

;(defreg bus
;    (mark! unmark! type! pointer!) (mark-bit type-not-pointer frame=0 displacement=0 address=0))

;; A register has two basic operations which can be done to it. Its contents
;; can be fetched, and its contents can be assigned from some source. In
;; addition, for the LISP simulator we define two additional operations
;; which are used for stacks:

(defmacro save (quantity)
  (assign *stack* (&cons ,quantity (fetch *stack*))))

(defmacro restore (register)
  (progn (assign ,register (&car (fetch *stack*))) 
         (assign *stack* (&cdr (fetch *stack*)))))

;;  At this point we begin to look at the microcode proper. Boot-load is the
;; place where the chip is initialized to run. The first thing it does is
;; initialize the memory limit register and then it picks up (as an
;; interrupt address) a pointer to the expression to begin executing. It
;; stashes this away in the stack and goes off to MARK to get memory
;; organized and set up a reasonable value for *scan-up* (*newce11*). The
;; (micro) return address is stored as the type of the stack pointer. Thus
;; all micro return addresses must be pointer types - something the compiler
;; must know about!

(deftype boot-load
  (assign *scan-up* (fetch *nil*))
  (&increment-scan-up)                               ; to location 1
  (assign *memtop* (&car (fetch *scan-up*)))
  (assign *scan-up* (fetch *memtop*))
  (assign *stack* (&get-interrupt-routine-pointer)) ;from pads
  (&set-type *stack* boot-load-return)
  (go-to mark))

(defreturn boot-load-return
   (assign *exp* (&car (fetch *stack*))) 
   (assign *stack* (fetch *nil*))
   (&set-type *stack* done)
   (dispatch-on-exp-allowing-interrupts))

   
;;   When there is nothing more to do the machine halts.

(deftype done
    (go-to done))

;; The next section of the microcode is the SCHEME-79 chip storage allocator
;; and garbage collector. Mark is the garbage-collector entry point. It is a
;; user function with no arguments which returns Nil, when it is done.  We
;; use the Deutseh-Schorr-Waite mark algorithm. There are 3 registers
;; containing pointer data: *stack-top*, *node-pointer*, *leader*. A datum
;; may be a pointer or a terminal datum; this may be tested by &pointer?
;; Objects have 2 data parts which can fit a pointer - the CAR and
;; CDR. These are accessed by &car and &cdr functions of the pointer to the
;; object. They are clobbered by &rplaca and &rplacd functions of a pointer
;; to the object and the replacement datum. Objects also have two mark bits,
;; the in-use and car-trace-in-progress bit. The in-use bit is stored in
;; the CAR of the node and the car-trace-in-progress bit is stored in the
;; CDR of the node. They are accessed by &in-use? and &car-being-traced? of
;; a pointer to the object. They are set and cleared by &mark-in-use
;; &mark-car-being-traced!, &mark-car-trace-over! and &unmark! of the
;; pointer to the object. In addition, any &rplaca or &rplacd operation
;; will clear the associated mark bit. This requires the introduction of
;; &rplaca-and-mark! to change the CAR pointer while setting the in-use bit.

(deftype mark                       ;MARK(?)
  (&rplaca (fetch *nil*) (fetch *stack*)) 
  (assign *node-pointer* (fetch *nil*))
  (assign *stack-top* (fetch *nil*)) 
  (&set-type *stack-top* gc-special-type) 
  (go-to mark-node))

(defpc mark-node
  (assign *leader* (&car (fetch *node-pointer*)))
  (cond ((and (&pointer? (fetch *leader*))
              (not (&in-use? (fetch *leader*))))
         (&mark-car-being-traced! (fetch *node-pointer*))
         (&rplaca-and-mark! (fetch *node-pointer*) (fetch *stack-top*)) 
         (go-to down-trace))
        (t
         (&mark-in-use! (fetch *node-pointer*))
         (go-to trace-cdr))))

(defpc down-trace
  (assign *stack-top* (fetch *node-pointer*)) 
  (assign *node-pointer* (fetch *leader*)) 
  (go-to mark-node))

(defpc trace-cdr
  (assign *leader* (&cdr (fetch *node-pointer*)))
  (cond ((and (&pointer? (fetch *leader*)) (not (&in-use? (fetch *leader*))))
         (&rplacd (fetch *node-pointer*) (fetch *stack-top*))
         (go-to down-trace))
        (t (go-to up-trace))))

(defpc up-trace
  (cond ((&=type? (fetch *stack-top*) gc-special-type) 
         (go-to sweep))
        (t (assign *leader* (fetch *stack-top*))
           (cond ((&car-being-traced? (fetch *leader*)) 
                  (&mark-car-trace-over! (fetch *leader*)) 
                  (assign *stack-top* (&car (fetch *leader*)))
                  (&rplaca-and-mark! (fetch *leader*)
                                     (fetch *node-pointer*))
                  (assign *node-pointer* (fetch *leader*))
                  (go-to trace-cdr))
                 (t (assign *stack-top* (&cdr (fetch *leader*))) 
                    (&rplacd (fetch *leader*) (fetch *node-pointer*)) 
                    (assign *node-pointer* (fetch *leader*))
                    (go-to up-trace))))))

;; The sweep algorithm for this garbage collector is the simple two finger
;;  compacting method. The two "fingers" are: *scan-up* and
;;  *scan-down*. Remember, *scan-up* is the newcell register for cons.
;;  Thus, because mark does not disturb it, initially, *scan-up* points at
;;  the last successfully completed cons.

(defpc sweep
   (&increment-scan-up)
   (assign *scan-down* (fetch *scan-up*))  ;initialization
   (assign *scan-up* (fetch *nil*))        ;make address = 0
   (&set-type *scan-up* gc-special-type)
   (&clear-gc-needed)
   (go-to scan-down-for-thing))

(defpc scan-down-for-thing
  (&decrement-scan-down)
  (cond ((&scan-up=scan-down?) (go-to relocate-pointers)) 
        ((&in-use? (fetch *scan-down*)) (go-to scan-up-for-hole)) 
        (t (go-to scan-down-for-thing))))

(defpc scan-up-for-hole
  (cond ((&in-use? (fetch *scan-up*))
         (&increment-scan-up)
         (cond ((&scan-up=scan-down?) (go-to relocate-pointers)) 
               (t (go-to scan-up-for-hole))))
        (t (go-to swap-thing-and-hole))))

;;   The following code is rather tricky. The last rplaca operation performs
;; several important operations at once. Since the type of *scan-up* is
;; gc-special-type, the cell pointed at by *scan-down* (which is above the
;; eventual *scan-up* and thus will be free storage) is marked as a "broken
;; heart" pointing at where its contents has gone. This will be looked at
;; later by the relocation phase. This free-cell-to-be is also unmarked by
;; this operation.

(defpc swap-thing-and-hole
  (&rplaca-and-mark! (fetch *scan-up*) (&car (fetch *scan-down*))) 
  (&rplacd (fetch *scan-up*) (&cdr (fetch *scan-down*)))
  (&rplaca (fetch *scan-down*) (fetch *scan-up*))
  (go-to scan-down-for-thing))

;;   The relocation phase now adjusts all live pointers which point at
;; object which have been moved, leaving behind broken hearts. At the entry
;; to relocate-pointers, *scan-up* = *scan-down* and they point at the
;; highest occupied location in memory. *Scan-up* is left there to become
;; the future *newcell* and *scan-down* is used to count down until we get
;; to the bottom of memrory.

(defpc relocate-pointers
  (assign *rel-tem-1* (&car (fetch *scan-down*)))
  (cond ((&pointer? (fetch *rel-tem-1*))
         (assign *rel-tem-2* (&car (fetch *rel-tem-1*)))
         (cond ((&=type? (fetch *rel-tem-2*) gc-special-type) 
                (&set-type *rel-tem-2* (fetch *rel-tem-1*))
                (&rplaca (fetch *scan-down*) (fetch *rel-tem-2*))))))
  (assign *rel-tem-1* (&cdr (fetch *scan-down*)))
  (cond ((&pointer? (fetch *rel-tem-1*))
         (assign *rel-tem-2* (&car (fetch *rel-tem-1*)))
         (cond ((&=type? (fetch *rel-tem-2*) gc-special-type) 
                (&set-type *rel-tem-2* (fetch *rel-tem-1*))
                (&rplacd (fetch *scan-down*) (fetch *rel-tem-2*))))))
  (&unmark! (fetch *scan-down*))
  (cond ((&scan-down=0?)
         (&set-type *scan-up* self-evaluating-pointer)
         (assign *stack* (&car (fetch *nil*))) ;might have been relocated
         (&rplaca (fetch *nil*) (fetch *nil*))
         (assign *val* (fetch *nil*))
         (dispatch-on-stack))
        (t (&decrement-scan-down)
           (go-to relocate-pointers))))

;;   Congratulations, you have just survived the garbage collector! We now
;; proceed to examine the evaluator proper. The first part of the evaluator
;; is the stuff for dealing with variable references. The opcodes which take
;; a lexical-address as their data field decode that field into a frame
;; number and a displacement number in the *exp* register. Lexical access of
;; local variables uses lookup-exp to get a locative to the value. The CAR
;; of the locative is the value cell for that variable. Micro-call is a
;; microcode macro operation which stashes the (micro code) return address
;; specified by its second argument in the type field of *retpc-count-mark*
;; and then goes to the micro-code address specified by its first
;; argument. Micro-return is used to dispatch on this saved type field.

(deftype local              ;LOCAL(lexical-address)
    (micro-call lookup-exp local-return))

(defpc local-return
  (assign *val* (&car (fetch *display*)))
  (dispatch-on-stack))

;;   Tail local variables give SCHEME an LSUBR option. That is, a procedure
;; may be passed the list of evaluated arguments as the value of a variable
;; rather than having an explicit local variable for each value passed. For
;; example: in ((lambda x (foo x)) 1 2 3), x is a tail variable which is
;; bound to the list (1 2 3). additionally, this is extended to give the
;; power of rest variables as follows: in (lamba (x y . z) ---) x and y are
;; bound to the first two arguments while z is a tail variable which is
;; bound to the remaining arguments.

(deftype tail-local          ;TAIL-LOCAL(lexical-address)
    (micro-call lookup-exp tail-local-return))

(defpc tail-local-return
  (assign *val* (fetch *display*))
  (dispatch-on-stack))

;; Global variables are stored in the value cell of a symbol. The value cell
;; is assumed to be in the CAR of the symbol. The CDR may be used for other
;; purposes (such as property lists). Thus the global type may be thought of
;; as an alias for CAR.

(deftype global              ;GLOBAL(symbol)
  (assign *val*              ;global-value=&car
   (&global-value (fetch *exp*))) 
  (dispatch-on-stack))

;;   The following stuff is for assignment to variables. These types are to
;; be used as part of a sequence whose previous entry develops a value in
;; *val* which will be the value stuffed into the variable's value locative.

(deftype set-local           ;SET-LOCAL(lexical-address)
    (micro-call lookup-exp set-local-return))

(defpc set-local-return
  (&rplaca (fetch *display*) (fetch *val*)) 
  (dispatch-on-stack))

(deftype set-tail-local      ;SET-TAIL-LOCAL(lexical-address)
    (micro-call lookup-exp set-tail-local-return))

(defpc set-tail-local-return
  (&rplacd (fetch *display*) (fetch *val*)) 
  (dispatch-on-stack))

;;   The following is a tricky little critter. It is needed because if we
;; have a tail only variable (e.g. (lambda x ---)) we need to be able to get
;; at the header of the sublist of the display referred to by the tail
;; variable.

(deftype set-only-tail-local ;SET-ONLY-TAIL-LOCAL(lexical-address)
  (if (&frame=0?)
      (progn (&rplaca (fetch *display*) (fetch *val*)) 
             (dispatch-on-stack))
      (progn (assign *display* (&cdr (fetch *display*))) 
             (&decrement-frame)
             (go-to set-only-tail-local))))

;&set-global-value = &rplaca
(deftype set-global           ;SET-GLOBAL(symbol)
  (&set-global-value (fetch *exp*) (fetch *val*)) 
  (dispatch-on-stack))

(defpc lookup-exp
    (if (&frame=0?)
        (progn (assign *display* (&car (fetch *display*))) 
               (go-to count-displacement))
        (progn (&decrement-frame)
               (assign *display* (&cdr (fetch *display*))) 
               (go-to lookup-exp))))

(defpc count-displacement
    (if (&displacement=0?)
        (micro-return)
        (progn (&decrement-displacement)
               (assign *display* (&cdr (fetch *display*))) 
               (go-to count-displacement))))

;;    Next come all of the various types of self-evaluating data. There are
;; two different classes -- pointer data and immediate data. A symbol is
;; pointer data. We provide several unspecified varieties of such
;; self-evaluating data for the user to assign to things like fixed numbers
;; and floating numbers.

(deftype self-evaluating-immediate   ;SELF-EVALUATING-IMMEDIATE(frob)
  (assign *val* (fetch *exp*))
  (dispatch-on-stack))

(deftype self-evaluating-immediate-1 ;SELF-EVALUATING-IMMEDIATE-1(frob) 
  (assign *val* (fetch *exp*)) 
  (dispatch-on-stack))

(deftype self-evaluating-immediate-2 ;SELF-EVALUATING-IMMEDIATE-2(frob) 
  (assign *val* (fetch *exp*)) 
  (dispatch-on-stack))

(deftype self-evaluating-immediate-3 ;SELF-EVALUATING-IMMEDIATE-3(frob) 
  (assign *val* (fetch *exp*)) 
  (dispatch-on-stack))

(deftype self-evaluating-immediate-4 ;SELF-EVALUATING-IMMEDIATE-4(frob)
  (assign *val* (fetch *exp*)) 
  (dispatch-on-stack))

(deftype symbol                      ;SYMBOL(frob)
  (assign *val* (fetch *exp*))
  (dispatch-on-stack))

(deftype self-evaluating-pointer     ;SELF-EVALUATING-POINTER(frob)
  (assign *val* (fetch *exp*))
  (dispatch-on-stack))

(deftype self-evaluating-pointer-1   ;SELF-EVALUATING-POINTER-1(frob)
  (assign *val* (fetch *exp*))
  (dispatch-on-stack))

(deftype self-evaluating-pointer-2   ;SELF-EVALUATING-POINTER-2(frob) 
  (assign *val* (fetch *exp*))
  (dispatch-on-stack))

(deftype self-evaluating-pointer-3   ;SELF-EVALUATING-POINTER-3(frob) 
  (assign *val* (fetch *exp*))
  (dispatch-on-stack))

(deftype self-evaluating-pointer-4   ;SELF-EVALUATING-POINTER-4(frob)
  (assign *val* (fetch *exp*)) 
  (dispatch-on-stack))

;;     A lambda expression in the original SCHEME code turns into a
;; procedure in the S-code. When executed, a procedure constructs and
;; returns a closure. Procedures may be documented with a description of the
;; original variable names and the context they were compiled in (perhaps
;; even a direct pointer to the source code) thus providing for debugging
;; tools.

(deftype procedure                   ;PROCEDURE((script . documentation))
  (assign *val* (&cons (fetch *exp*) (fetch *display*))) 
  (&set-type *val* closure)
  (dispatch-on-stack))

;;    An if expression in the SCHEME source code turns into a sequence which
;; evaluates the predicate part of the if and then falls into a conditional
;; to choose between the consequent and alternative expressions on the basis
;; or the value of the *val* register.

(deftype conditional                 ;CONDITIONAL((consequent . alternative))
  (if (&eq-val (fetch *nil*))
      (assign *exp* (&cdr (fetch *exp*)))
      (assign *exp* (&car (fetch *exp*)))) 
  (dispatch-on-exp-allowing-interrupts))

;;   The following macro definition defines a common sequence in the rest of
;; the microcode. This sequence will be the standard way to attack a
;; compound expression. The (micro) return address is stashed in *retpc-
;; count-mark* so that it can be used as the type of a stack cell. The top
;; of the stack had better be standard-return which knows how to undo this
;; mess.

(defmicromacro save-cdr-and-eval-car (return-tag)
  (progn (&set-type *retpc-count-mark* ,return-tag) 
         (go-to standard-eval)))

(defpc standard-eval
  (save (fetch *display*))
  (&set-type *stack* (fetch *retpc-count-mark*)) 
  (save (&cdr (fetch *exp*)))
  (&set-type *stack* standard-return)
  (assign *exp* (&car (fetch *exp*)))
  (dispatch-on-exp-allowing-interrupts))

(defreturn standard-return
  (restore *exp*)
  (assign *retpc-count-mark* (fetch *stack*)) 
  (restore *display*)
  (dispatch (fetch *retpc-count-mark*)))

;;   The sequence construct is very important in the S-code language. Not
;;  only is it used to implement PROGN but also, it is used to develop
;;  values in the *val* register to be used by later parts of the sequence
;;  such as conditionals or variable assigners.

(deftype sequence                   ;SEQUENCE((expression . rest))
  (assign *val* (fetch *nil*))      ;for gc
  (save-cdr-and-eval-car sequence-return))

(defreturn sequence-return
    (dispatch-on-exp-allowing-interrupts))

;;  Control points are used to implement the general "catch tags" used in
;;  constructing non-standard control structures. It is useful for error
;;  exits, and multiprocess sorts of work. It is only to be used with
;;  extreme caution since it is easy to screw oneself with constructs such
;;  as this which violate the expression structure of the language.

(deftype get-control-point          ;GET-CONTROL-POINT((variable-setter . rest)).
  (&set-type *val* control-point)
  (save-cdr-and-eval-car sequence-return))

;;  To evaluate a form with more than one arguinent one starts with a
;;  pointer of type first-argument which is used to initilize the *args*
;;  register which will be used to accumulate the arguments. The evaluation
;;  of the first argument is to be continued with an evaluation of each
;;  successive next argument until the last argument is encountered which
;;  should fall into the execution of the body of the procodure being
;;  called.

(deftype first-argument             ;FIRST-ARGUMENT((arg1 . rest))
    (save-cdr-and-eval-car first-argument-return))

(defreturn first-argument-return
  (assign *args* (&cons (fetch *val*) (fetch *nil*)))
  (save (fetch *args*)) 
  (dispatch-on-exp-allowing-interrupts))

;;  Next argument just accumulates the value of an argument and continues
;;  the evaluation of the form.

(deftype next-argument              ;NEXT-ARGUMENT((arg . rest))
  (save (fetch *args*))
  (save-cdr-and-eval-car next-argument-return))

(defreturn next-argument-return
  (restore *args*)
  (&rplacd (fetch *args*) (&cons (fetch *val*) (fetch *nil*))) 
  (assign *args* (&cdr (fetch *args*)))
  (dispatch-on-exp-allowing-interrupts))

;;  Finally we get to the evaluation of the last argument. At this time the
;; continuation is an expression which should evaluate to a closure which is
;; to be applied.

(deftype last-argument              ;LAST-ARGUMENT((arg . fun))
  (save (fetch *args*))
  (save-cdr-and-eval-car last-argument-return))

(defreturn last-argument-return
  (restore *args*)
  (&rplacd (fetch *args*)
            (&cons (fetch *val*) (fetch *nil*)))
  (eval-exp-popj-to internal-apply)) ;Amazing! Where did retpc go?

;;  Procedures with zero or one argument are handled specially
;;  For efficiency reasons.

(deftype apply-1-arg                 ;APPLY-1-ARG((arg . fn))
    (save-cdr-and-eval-car apply-1-arg-return))

(defreturn apply-1-arg-return
  (assign *args* (&cons (fetch *val*) (fetch *nil*))) 
  (save (fetch *args*))
  (eval-exp-popj-to internal-apply))

(deftype apply-no-args              ;APPLY-NO-ARGS((fn . ?))
  (assign *exp* (&car (fetch *exp*)))
  (save (fetch *nil*))              ;ugh! need a place for retpc.
  (eval-exp-popj-to internal-apply))

;;  Spread argument is apply. It evaluates an argument, takes it as the set
;; of arguments to be passed to the procedure specified by the continuation.

(deftype spread-argument            ;SPREAD-ARGUMENT((arg . fun))
    (save-cdr-and-eval-car spread-argument-return))

(defreturn spread-argument-return
  (save (fetch *val*))
  (eval-exp-popj-to internal-apply))

(defreturn internal-apply            ;function is in *val*.
  (restore *args*)
  (assign *exp* (fetch *val*))
  (dispatch-on-exp-allowing-interrupts))

;;  Every user procedure is a closure . The closures are produced by
;;  evaluating procedures. A closure has a script which is the body of the
;;  procedure to be executed and a display which is the environment which
;;  the closure was manufactured in. Notice that there two CAR operations
;;  required to get the actual body of the procedure. This is necessary to
;;  bypass the documentation packaged in the procedure definition.

(deftype closure                     ;CLOSURE((script . display))
  (assign *display*
   (&cons (fetch *args*) (&cdr (fetch *exp*))))
  (assign *exp* (&car (&car (fetch *exp*)))) 
  (dispatch-on-exp-allowing-interrupts))

;;  When a control point (non-standard continuation) is executed it is
;;  interpreted as a procedure with one argument which returns that argument
;;  to the constructor of the control point.

(deftype control-point               ;CONTROL-POINT(state)
  (assign *val* (&car (fetch *args*)))
  (assign *stack* (&car (fetch *exp*)))
  (dispatch-on-stack))

;;  An interrupt point is similar to a control point except that the
;;  *display* and *va1* registers must be restored.

(deftype interrupt-point             ;INTERRUPT-POINT(state)
  (assign *stack* (fetch *exp*))
  (restore *val*)
  (restore *display*)
  (go-to restore-exp-args-dispatch))

(deftype primitive-apply-1           ;PRIMITIVE-APPLY-1((arg . primop))
  (save (&cdr (fetch *exp*)))
  (assign *exp* (&car (fetch *exp*)))
  (eval-exp-popj-to primitive-apply-1-return))

(defreturn primitive-apply-1-return
  (restore *exp*)
  (dispatch-on-exp-allowing-interrupts))

;;  The primitive operators included on the SCHEME-79 chip are implemented
;;  in the following microcode.

(deftype primitive-car              ;PRIMITIVE-CAR(?)
  (assign *val* (&car (fetch *val*)))
  (dispatch-on-stack))

(deftype primitive-cdr              ;PRIMITIVE-CDR(?)
  (assign *val* (&cdr (fetch *val*)))
  (dispatch-on-stack))

(deftype primitive-type?            ;PRIMITIVE-TYPE?(?)
  (assign *exp* (fetch *val*)) 
  (assign *val* (fetch *nil*)) 
  (&set-type *val* (fetch *exp*))   ;build prototype.
  (dispatch-on-stack))

                                    ;PRIMITIVE-NOT-ATOM(?)
(deftype primitive-not-atom      
  (if (&pointer? (fetch *val*))
      (progn (assign *val* (fetch *nil*))
             (&set-type *val* self-evaluating-immediate)) ;T
      (assign *val* (fetch *nil*)))
  (dispatch-on-stack))

(deftype primitive-zerop            ;PRIMITIVE-ZEROP(?)
  (if (&val=0?)
      (progn (assign *val* (fetch *nil*))
             (&set-type *val* self-evaluating-immediate))  ;T
      (assign *val* (fetch *nil*)))
  (dispatch-on-stack))

(deftype primitive-sub1             ;PRIMITIVE-SUB1(?)
  (assign *scan-down* (fetch *val*))
  (&decrement-scan-down-to-val)
  (dispatch-on-stack))

(deftype primitive-add1             ;PRIMTIIVE-ADD1(?)
  (assign *exp* (fetch *scan-up*)) 
  (assign *scan-up* (fetch *val*)) 
  (&increment-scan-up-to-val) 
  (assign *scan-up* (fetch *exp*))
  (dispatch-on-stack))

(deftype primitive-displacement-add1 ;PRIMITIVE-DISPLACEMNT-ADD1(?)
  (assign *exp* (fetch *nil*))
  (&decrement-frame)          ;make -1 in frame part
  (&val-displacement-to-exp-displacement)
  (assign *args* (fetch *scan-up*)) 
  (assign *scan-up* (fetch *exp*)) 
  (&increment-scan-up)
  (assign *exp* (fetch *scan-up*)) 
  (&val-frame-to-exp-frame)
  (assign *val* (fetch *exp*)) 
  (dispatch-on-stack))

;;  Thus cons = list*.

(deftype primitive-apply-2          ;PRIMITIVE-APPLY-2((arg primop))
  (save (fetch *args*))
  (save (&cdr (fetch *exp*)))
  (assign *exp* (&car (fetch *exp*)))
  (eval-exp-popj-to restore-exp-args-dispatch))

(defreturn restore-exp-args-dispatch
  (restore *exp*)
  (restore *args*)
  (dispatch-on-exp-allowing-interrupts))

(deftype primitive-cons             ;PRIMITIVE-CONS(?)
  (&rplacd (fetch *args*) (fetch *val*))
  (restore *val*)
  (dispatch-on-stack))

(deftype primitive-eq               ;PRIMITIVE-EQ(?)
  (restore *args*)
  (assign *args* (&car (fetch *args*)))
  (if (&eq-val (fetch *args*))
      (progn (assign *val* (fetch *nil*))
             (&set-type *val* self-evaluating-immediate)) 
      (assign *val* (fetch *nil*)))
  (dispatch-on-stack))

(deftype primitive-rplaca           ;PRIMITIVE-RPLACA(?)
  (restore *args*)
  (assign *val* (&rplaca (&car (fetch *args*)) (fetch *val*))) 
  (dispatch-on-stack))

(deftype primitive-rplacd           ;PRIMITIVE-RPLACD(?)
  (restore *args*)
  (assign *val* (&rplacd (&car (fetch *args*)) (fetch *val*))) 
  (dispatch-on-stack))

(deftype primitive-type!            ;PRIMITIVE-TYPE!(?)
  (restore *args*)
  (assign *exp* *val*)
  (assign *val* (&car (fetch *args*)))
  (&set-type *val* (fetch *exp*))
  (dispatch-on-stack))

;;  When an interrupt is requested and the machine is allowing interrupts,
;;  the microcode continues from the following place:

(deftype process-interrupt 
  (save (fetch *args*)) 
  (save (fetch *exp*)) 
  (save (fetch *display*)) 
  (save (fetch *val*))
  (&set-type *stack* interrupt-point)
  (assign *args* (fetch *stack*))
  (assign *exp* (&car (&get-interrupt-routine-pointer))) ;from pads
  (dispatch-on-exp-allowing-interrupts))

;;  The following routines are used to let the user get at the internal
;;  storage allocator registers (GOD help him!). 

(defpc get-memtop
  (assign *val* (fetch *memtop*)) 
  (dispatch-on-stack))

(defpc set-memtop
  (assign *memtop* (fetch *val*)) 
  (dispatch-on-stack))

(defpc get-scan-up
  (assign *val* (fetch *scan-up*)) 
  (dispatch-on-stack))

(defpc set-scan-up
  (assign *scan-up* (fetch *val*))
  (&set-type *scan-up* self-evaluating-pointer) 
  (dispatch-on-stack))

;;  The following code is put in for debugging purposes. By setting the
;;  state of the machine to the appropriate value we can read out or set any
;;  of the internal machine registers.

;; [since this appears to be an infinite loop, I'm guessing the intent
;;  was to use an interrupt to enter or escape -- BWM]

(defpc debug-routine
  (&write-to-pads (fetch *exp*)) 
  (&write-to-pads (fetch *val*)) 
  (&write-to-pads (fetch *args*)) 
  (&write-to-pads (fetch *display*)) 
  (&write-to-pads (fetch *stack*)) 
  (&write-to-pads (fetch *newcell*)) 
  (&write-to-pads (fetch *memtop*)) 
  (&write-to-pads (fetch *retpc-count-mark*))
  (&write-to-pads (fetch *intermediate-argument*))
  (&read-from-pads *exp*)
  (&read-from-pads *val*)
  (&read-from-pads *args*)
  (&read-from-pads *display*)
  (&read-from-pads *stack*)
  (&read-From-pads *newcell*) 
  (&read-from-pads *memtop*)
  (&read-from-pads *retpc-count-mark*) 
  (&read-from-pads *intermediate-argument*) 
  (go-to debug-routine))

