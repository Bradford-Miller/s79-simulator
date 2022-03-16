;; Time-stamp: <2022-03-15 14:30:24 gorbag>

;; Now that we can boot, we want to run a simple S-Code function.
;; The following is a hand-compiled version of figure 2 in AIM-559
;; (the APPEND function)

;; We will invoke it with two lists and expect to get a result that is
;; 2nd list appended to the first.

(cl:in-package :scheme-mach)

;; The code that is represented as S-Code is:

#||
(defun append (list1 list2)
  (cond ((eq list1 '()) list2)
        (t (cons (car list1)
                 (append (cdr list1) list2)))))
||#

;; From the AIM: (quoting)

;; The S-code representation differs form the original Lisp expression
;; in several ways. It distinguishes between two kinds of variables in
;; the original Lisp expression. Global variable references translate
;; into a special kind of pointer which points at the global value of
;; the symbol. A local variable reference is transformed into an
;; instruction containing a lexical address of its value in the
;; environment structure. Constants are transformed into instructions
;; which move the appropriate constant into the accumulated
;; value. Certain primitive procedures (for example, CAR, CDR, CONS,
;; EQ, etc.) are recognized and are realized directly as machine
;; op-codes. Procedure calls are converted from prefix to postfix
;; notation. Conditionals and control sequences (PROGN) are peformed
;; somewhat differently than they are in the source langauge.

;; the (ASCII ;-) graphic representation of the S-Code (AIM 559 Figure 2):

;;  ----------
;;  |        |
;;  |        V
;;  |    (closure   |  ...   )   ; This is the global value cell of APPEND
;;  |    (   *      |        )
;;  |        |
;;  |        V
;;  |    (procedure |  NIL   )   ; The environment is NIL (top level)
;;  |    (   *      |        )
;;  |        |
;;  |        V
;;  |    (sequence  |  *-----)-> ; CDR points at documentation of APPEND
;;  |    (   *      |        )
;;  |        |
;;  |        V
;;  |    (1st-arg   | cond   ) 
;;  |    (   *      |   *    )
;;  |        |          |
;;  |        |          --------
;;  |        V                 |
;;  |    (local     | apply2 ) | 
;;  |    (  0,0     |   *    ) |
;;  |                   |      |    
;;  |        ------------      |     ---------
;;  |        |                 |     |       |
;;  |        V                 |     |       V
;;  |    ( NIL      |   EQ   ) |     |   (1st-arg   |  CONS  )
;;  |    (          |   ...  ) |     |   (   *      |   ...  )
;;  |                          |     |       |
;;  |        -------------------     |       |
;;  |        |                       |       |
;;  |        V                       |       V
;;  |    (local     |1st-arg )       |   (apply1    |last-arg)
;;  |    (  0,1     |   *    )       |   (   *      |   *    )
;;  |                   |            |       |          |
;;  |        ------------            |       |          --------
;;  |        |                       |       |                 |
;;  |        V                       |       V                 |
;;  |    (apply1    | apply2 )       |   (local     |  CDR   ) |
;;  |    (   *      |   *    )       |   (  0,0     |  ...   ) |
;;  |        |          |            |                         |
;;  |        |          --------------       -------------------
;;  |        |                               |
;;  |        V                               V
;;  |    (local     |  CAR   )           (local     | global )
;;  |    (  0,0     |  ...   )           (  0,1     |    *   )
;;  |                                                    |
;;  ------------------------------------------------------

;; if I read this correctly, we want our function application to be in
;; the first address we will execute. That should be the application
;; of the APPEND function to a couple of lists.

;; the "..."s in the above are what appear in TR 559 (Figure 2). But I suspect
;; this represents more code which I'll have to figure out to make the example
;; work! 

;; in the microcode, we start with arguments and the continuation
;; after last-argument is the function to call
;;
;;        (sequence        | NIL    ) ;; single element sequence
;;        (      *         |        )
;;               |
;;               V
;;        (1st-arg         | NIL    ) ;; ?? done ??
;;        (      *         |        )
;;               |                       
;;               V                       
;;        (SE-Pointer      | last-arg )     (SE-Pointer      | global  ) 
;;        (      *         |   *------)---> (      *         |    *----)----->     [Global Value Cell of APPEND]
;;               |                                 |
;;               V                                 V              
;;        (SE-immediate    | SE-ptr )       (SE-immediate    | SE-ptr ) 
;;        (      1         |   *    )       (      3         |   *    )
;;                             |                                 |
;;               ---------------                   ---------------
;;               |                                 |
;;               V                                 V
;;        (SE-immediate    | NIL    )       (SE-immediate    | NIL    )
;;        (      2         |        )       (      4         |        )

;;    Now the symbol cell of APPEND looks like (Value cell | Plist)
;; which I believe is what was reported in Figure 2. So now we just
;; have to expand figure 2 to cover

;; Note this has some differences with AIM 514, the append example
;; there is with symbols (we will temporarily avoid these as we don't
;; have an OBTAB yet, yet alone a REPL, intern, etc. which will have
;; to be defined in S-Code). And there appears to be an explicit
;; "number" type which doesn't appear in the Scheme-79 microcode, but
;; could be used as an alias for, say, self-evaluating-immediate-1 but
;; as the actual code for all these is identical in the microcode, I
;; don't yet see the need to differentiate them. (Presumably this is
;; to allow for ease of coprocessing since the Scheme-79
;; implementation doesn't have an ALU!)


;; so a first cut at that.
;;
;; let {foo} be a memory location tag we can refer to, with {foo}+1,
;;    {foo}+2, etc.) being offsets from that tag. (We'll fill those in
;;    by hand for now when we fit this into our memory). We'll use our
;;    lookup fns to grab representations from the loaded microcode too.

;; some constants to make this more compact. MCR should have been loaded
;; first so these can be resolved.
(defconstant +sep+ (pointer-type-name->int 'microlisp:self-evaluating-pointer))
(defconstant +symbol+ (pointer-type-name->int 'microlisp:symbol))
(defconstant +sei+ (non-pointer-type-name->int 'microlisp:self-evaluating-immediate))

(defconstant +nil+ (make-word +sep+ #o0)) ;; might really need to be +symbol+?
;; alternatively may want to create my own type for now. The original microcode
;; is not sacrosanct, we should get it working as far as it is, but we can
;; allow our own types and encoding for simplicity. Remember our goal here is
;; to generate tools for generating FPGA processors, not the Scheme-79 chip
;; itself; just document it in the README as a variation if it continues into
;; the general microcode.

(defconstant +clos+ (pointer-type-name->int 'microlisp:closure))
(defconstant +proc+ (pointer-type-name->int 'microlisp:procedure))
(defconstant +seq+ (pointer-type-name->int 'microlisp:sequence))
(defconstant +glo+ (pointer-type-name->int 'microlisp:global))
(defconstant +loc+ (non-pointer-type-name->int 'microlisp:local))

(defconstant +sa+ (pointer-type-name->int 'microlisp:spread-argument)) ; generic apply
(defconstant +app1+ (pointer-type-name->int 'microlisp:primitive-apply-1)) ; could be apply-1-arg?
(defconstant +app2+ (pointer-type-name->int 'microlisp:primitive-apply-2))
(defconstant +arg1+ (pointer-type-name->int 'microlisp:first-argument))
(defconstant +argL+ (pointer-type-name->int 'microlisp:last-argument))

(defconstant +cond+ (pointer-type-name->int 'microlisp:conditional))
(defconstant +eq+ (non-pointer-type-name->int 'microlisp:primitive-eq))
(defconstant +car+ (non-pointer-type-name->int 'microlisp:primitive-car))
(defconstant +cdr+ (non-pointer-type-name->int 'microlisp:primitive-cdr))
(defconstant +cons+ (non-pointer-type-name->int 'microlisp:primitive-cons))

(eval-when (:load-toplevel :execute)
  ;; set some breakpoints to help with debugging

  (when *microcode-compiled* ; doesn't make sense if we only validated the code
    (flet ((break (sym) (set-breakpoint (microcontrol-symbol-value sym t) :permanent))
           (tbreak (sym) (set-breakpoint (microcontrol-symbol-value sym t)))) ; temporary break
      ;; break before any call to eval-exp-popj-to:
      ;; I'm not yet clear on the semantics of eval-exp-popj-to so these breaks
      ;; deal with tags near those calls.
      ;(break 'microlisp:last-argument-return)
      ;(break 'microlisp:apply-1-arg-return)
      ;(break 'microlisp:spread-argument-return)
      ;(break 'microlisp:primitive-apply-1)
      ;(break 'microlisp:primitive-apply-2)

      ;; we know the boot code works from test-1, so break on that return so we can start tracing
      ;; the interpretation of our apply fn (and advance this breakpoint as we determine what works)

      ;; Single stepping is painful but has to be done once... delete these breaks once we confirm the
      ;; memory and registers are correct
      ;(tbreak 'microlisp:boot-load-return)
      ;(tbreak 'microlisp:sequence)
      ;; (tbreak 'microlisp:first-argument) ; we get here fine as of 2/23/22. (almost to first-argument-return!)
      ;; (tbreak 'microlisp:last-argument) ; we get here fine as of 3/8/22. *args* seems to be good (first arg is there)

      ;; second arg seems to get set up correctly so we're good at the point we
      ;; call eval-exp-popj-to in last-argument-return! (see break above on last-argument-return)

      ;; if we reach here, we should check that *args* is now correct for invoking the closure
      ;; (tbreak 'microlisp:local) ; this passed 3/15/22
      ;; (tbreak 'microlisp:primitive-eq) ; this passed 3/15/22
      ;; next is setting up the cond
      (tbreak 'microlisp:conditional)

      ))
  
  (setq {args} 3 ;; addresses we fill in after our hand compilation (in decimal)
        {arg1} 5
        {arg2} 8
        {append} 10
        {argnxt} 7
        {app-glo} 11
        {app-fn} 12
        {app-seq} 13
        {cond-c1} 17
        {cond-c2} 20
        {cond-c3} 23)
  (setq *cold-boot-memory-array*
        ;; short so we'll use list format

        ;; start with address 2 - where we will begin executing
        ;; code. CAR should be a pointer to the first argument to the function
        ;; and CDR should be the function pointer

        ;; stack initialized to boot-load-return|2 in boot-load
        ;; boot-load-return sets
        ;; initial EXP as the CAR of stack (address 2), and we will dispatch on that
        ;; STACK is Done|NIL

        ;; CAR                        CDR
        `(
          ;; start off with a sequence, pointing to our first expression and NIL rest
          ,(make-word +seq+ {args}) ,+nil+ ; address 2
          ;; this is the function application we will evaluate
          ,(make-word +arg1+ (1+ {args})) ,+nil+ ; address 3 ({args})
          ;; I had used self-evaluating-poitner for the cdr here but that led to an infinite loop.
          ;; primitive-cons might be the right choice, but I'm concerned it's a non-pointer type. If that
          ;; fails, try sequence? Or may need to make up my own pointer for now.
          ,(make-word +sep+ {arg1})  ,(make-word +argL+ {argnxt}) ; address 4
          ,(make-word +sei+ #o1)    ,(make-word +sep+ (1+ {arg1})) ; address 5 ({arg1})
          ,(make-word +sei+ #o2)    ,+nil+                ; address 6
          ;; append goes into the continuation, but possibly this should be the
          ;; closure, (the value of the append symbol) at this point, i.e., we
          ;; may expect the compiler to have removed the symbol reference?
          ,(make-word +sep+ {arg2}) ,(make-word +glo+ {app-glo})               ; address 7 {argnxt}
          ,(make-word +sei+ #o3)    ,(make-word +sep+ (1+ {arg2})) ; address 8 ({arg2})
          ,(make-word +sei+ #o4)    ,+nil+                ; address 9
          ;; and here we define the APPEND symbol (note that as nothing refers to it, it will likely be gc'd away)
          ,(make-word +glo+ {app-glo}) ,+nil+             ; address 10 {append}; symbol; no plist
          ,(make-word +clos+ {app-fn}) ,+nil+             ; address 11 {app-glo}; global value cell
          ;; leading into the APPEND function
          ,(make-word +proc+ {app-seq}) ,+nil+            ; address 12 {app-fn}; no environment
          ,(make-word +seq+ (1+ {app-seq})) ,+nil+        ; address 13 {app-seq}; no documentation
          ,(make-word +arg1+ (+ 2 {app-seq})) ,(make-word +cond+ {cond-c1}) ; address 14
          ,(make-word +loc+ #o0)    ,(make-word +app2+ (+ 3 {app-seq})) ; address 15
          ,+nil+                    ,(make-word +eq+ #o0) ; address 16
          ,(make-word +loc+ #o1)    ,(make-word +arg1+ (1+ {cond-c1})) ; address 17 {cond-c1}
          ,(make-word +app1+ (+ 2 {cond-c1})) ,(make-word +app2+ {cond-c2}) ; address 18
          ,(make-word +loc+ #o0)    ,(make-word +car+ #o0); address 19
          ,(make-word +arg1+ (+ 1 {cond-c2})) ,(make-word +cons+ #o0) ; address 20 {cond-c2}
          ,(make-word +app1+ (+ 2 {cond-c2})) ,(make-word +argL+ {cond-c3}) ; address 21
          ,(make-word +loc+ #o0)    ,(make-word +cdr+ #o0) ; address 22
          ,(make-word +loc+ #o1)    ,(make-word +glo+ {app-glo}) ; address 23 {cond-c3}
          ))

  ;; when appropriate set interrupt routine pointer to 2,
  ;; note that the microcode will ask for it to assign to *stack*.
  (setq external-chips:*interrupt-address* 2)

  ;; we want enough memory to set up a freelist, but not so much we have a lot to check. Let's make it
  ;; 64 words since that's reasonable to manually look through. We'll increase it later once we
  ;; automate testing. (TBD)
  (setq *initial-memtop* (max 64 ; give it some room
                              (/ (+ 2 (length *cold-boot-memory-array*)) 2))) ; not shorter than our array though

  ;; gc-special-type always generates a warning since it's not a dispatchable type
  (format *error-output* ";;~%;; NB: warning about declared but undefined types are to be expected with this test 
;;     (in particular gc-special-type).~%;;~%")

  ;; these are shine-through from test-1, need to update for test-2 (TBD). Until then, expect the test to always fail.
  
  (setq *goal-memory-array*
        ;; CAR                      CDR
        `(
          #o0                      #o0  ;A0 null and OBLIST, shouldn't change
         ,(make-word #o100 #o3777) #o0  ;A1, was address 7, got relocated here at first open location
                                        ;(memtop not needed)
         ,(make-word #o0 #o3)      #o0  ;A2 still our pointer to our evaluable expression
         ,(make-word #o100 #o1777) ,(make-word #o0 #o4) ; A3 doesn't change
         ,(make-word #o100 #o2777) ,(make-word #o0 #o1) ; A4 updated CDR to point to A1 (was A7)
         ;; everything else is garbage (check *goal-newcell*)
          ))

  (setq *goal-memory-offset* 0)

  (setq *goal-stack* (bit-vector->integer (make-word #o124 #o0))) ; DONE and points to NIL

  (setq *goal-memtop* *initial-memtop*) ; better be the same

  (setq *goal-newcell* #o4) ; After compaction 5 should be our first free address, and newcell is the last valid CONS

  (setq *goal-exp* #o3) ; the start of our expression to be evaluated
  )

;; set up a test for successful completion
(defun check-test-2 ()
  (note "Checking test-2's result. 
This test checked that the hand-compiled APPEND code worked correctly")
  (cond
    ((and
      (external-chips:compare-memory *goal-memory-array*
                                     *goal-memory-offset*
                                     (/ (length *goal-memory-array*) 2)
                                     :warn)
      (= (bit-vector->integer microlisp-shared:*newcell*) *goal-newcell*)
      (= (bit-vector->integer microlisp-shared:*memtop*) *goal-memtop*)
      (= (bit-vector->integer microlisp-shared:*stack*) *goal-stack*)
      (= (bit-vector->integer microlisp-shared:*exp*) *goal-exp*))
     
     t)
    (t
     nil)))
