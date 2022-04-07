(in-package :scheme-mach)

(scheme-79:scheme-79-version-reporter "Scheme Machine ufunctions" 0 4 0
                                      "Time-stamp: <2022-04-07 14:24:55 gorbag>"
                                      "repartriated code")

;; 0.4.0   4/ 7/22 repatriating code from sim-machine-external,
;;                    to segregate code used for constructing the PLA from that used
;;                    to construct the machine/simulator itself

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.4.0   3/18/22 snapping a line: 0.4 release of scheme-79 supports test-0 thru test-3. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; &get-interrupt-routine-pointer
(defufn &get-interrupt-routine-pointer (:constituent t)

  (ecase *enclosing-opcode*
    (assign ; better have a *to-register*...
     (write-generated-code *upla-stream* nil
                           `(((to ,*to-register*) microlisp-shared::do-simple-get-interrupt-pointer))
                           "&get-interrupt-routine-pointer"))))

;; &read-from-pads

;; &write-to-pads


;; bus conditions mark-bit type-not-pointer frame=0 displacement=0 address=0

;; I added this to implement &cons:
(defupred &address=? (:calculated nil) 
  ;; may want to provide the calculation here, actually
  )

;; mark bit - these instructions fetch onto the bus and check the bit

;; &in-use? ;; from the CAR of the passed pointer which was just fetched (and thus on the bus)

;; commenting out earlier implementations in favor of those that will be implemented in terms of nanocode
(defupred &in-use? (mark-bit :car *bus*))

(defupred &not-in-use? (not-mark-bit :car *bus*))

(defupred-inverse &in-use? &not-in-use?)

;; &car-being-traced?
(defupred &car-being-traced? (mark-bit :cdr *bus*))
;  "similar to &in-use but checks the mark bit of the cdr"
  
;; &frame=0? ;; bus operation
(defupred &frame=0? (frame=0 nil *bus*))

;; &displacement=0? ;; bus operation
(defupred &displacement=0? (displacement=0 nil *bus*))

;;
;; not sure what instruction is related to (bus) address=0 but will
;; temporarily define an instruction here this could be checking for a
;; pointer to nil?

;; &address=0?

;(defufn &address=0? ()
;  (zerop-field *bus-data*))


;; exp register should support to-type, to-address, from, to-displacement, to-frame, from, from-decremented

;; &set-type (reg to-type)

;; so not sure how they dealt with constants that are part of the
;; instruction; I'm going to use the from field since it tends to be
;; larger than the to field (there are more from registers than to
;; register control wires, so it's less of a loss of extra bits).
(defufn &set-type (reg type-constant-or-register)
  (let ((real-register (translate-alias reg))
        (source-register (cl:if (register-p type-constant-or-register)
                                (translate-alias type-constant-or-register))))
    ;; register better support a type!
    (validate-register-control real-register 'to-type)
    (let ((nanocode (intern (format nil "DO-SET-TYPEC-~A" (strip-register-name real-register))
                            *ulang-shared-pkg*))
          (nanocode-from-register (intern (format nil "DO-SET-TYPE-~A" (strip-register-name real-register))
                                          *ulang-shared-pkg*))
          (numeric-type-constant (unless source-register
                                   (or (non-pointer-type-name->int type-constant-or-register)
                                       (pointer-type-name->int type-constant-or-register)))))
      (cl:if source-register ; if it named a register
             `(((from ,source-register) ,nanocode-from-register))
             `(((from-type-const ,numeric-type-constant) ,nanocode)))))) ; so should have been a constant

;; &val-displacement-to-exp-displacement 

;; &val-frame-to-exp-frame 

;; &decrement-scan-down-to-val (the from-decremented, remember scan-down is an alias for exp)

;; "the opcodes which take a lexical-address as their data field
;; decode that field into a frame number and a displacement number in
;; the *exp* register" so that requires, I suppose, to-address,
;; from/to-displacement and from/to-frame ?



;; val register should support to-type to-address from type=bus address=bus =bus

;; to-type because it can be the target of &set-type (q.v.)

;; &eq-val
(defupred &eq-val (=bus :arg *val*))

;; &val=0?

;; &decrement-scan-down
(defufn &decrement-scan-down ()
  `((microlisp-shared::do-decrement-scan-down)))

;; &decrement-scan-down-to-val

;; &increment-scan-up
(defufn &increment-scan-up ()
  `((microlisp-shared::do-increment-scan-up)))

;; alias - newcell is currently the same as scan-up
(defufn &increment-newcell ()
  `((microlisp-shared::do-increment-newcell)))

;; &increment-scan-up-to-val




;; &set-global-values

;; &global-value
(defufn &global-value (reg-ref :constituent t :args-last t) ; args-last because &car is
  ;; register reference should be to address of a symbol, and the &car
  ;; of that symbol is the global-value cell
  (&car reg-ref)) ; redirect

;; &decrement-displacement
(defufn &decrement-displacement () ; decrements the displacement field of *exp*
  `((microlisp-shared::do-decrement-displacement)))

;; &decrement-frame
(defufn &decrement-frame () ; decrements the frame field of *exp*
  `((microlisp-shared::do-decrement-frame)))

;; pointer bit - this instruction fetches onto the bus and checks the bit
;; &pointer?
(defupred &pointer? (type=pointer nil *bus*)) ; only bus can check this

;; not in the original machine, but for completeness (inverse)
(defupred &type? (type=type nil *bus*))

(defupred-inverse &pointer? &type?)

;; &=type?  
(defupred &=type? (type=bus nil))

;; assign
(defufn assign (to-register from :args-last t) ; args-last functions get
                                               ; special handling in
                                               ; compile-expression &
                                               ; compile-parameter
  (let ((*enclosing-opcode* 'assign)
        (*to-register* to-register)
        (*constituent-assignment-fn* `(assign ,to-register)))
    ;; if our current expression is the same as the assignment fn, clear it to
    ;; prevent loops
    (cond-binding-predicate-to foo 
      ((from-direct-register-p from 'assign)
       (let ((foo (write-generated-code *upla-stream* *current-expression*
                                        `(((:to ,to-register) (:from ,foo) microlisp-shared::mover))
                                        "assign defufn")))
         (if *upla-stream*
           nil ; we generated the code so don't return it
           foo)))

      (t
       (compile-parameter *upla-stream* from nil))))) ; let *constituent-assignment-fn* do it's job

;; fetch
(defufn fetch (from-register :constituent t)
  ;; set up the from field. Should be inside of some other microcode.
  (setq *from-register* from-register)
  (ecase *enclosing-opcode*
    ((assign
      save) ; save set up the to register as the stack
     `(((from ,from-register) (to ,*to-register*) microlisp-shared::mover)))
    ((&rplaca &rplaca-and-mark! &rplaca-and-unmark!
      &mark-in-use! &unmark!

      &rplacd &rplacd-and-mark-car-being-traced! &rplacd-and-mark-car-trace-over! 
      &mark-car-being-traced! &mark-car-trace-over! 

      &set-type

      dispatch eval-exp-popj-to) ; really dispatch
     from-register) ; just send back the register
    ))

;; restore
(defufn restore (register)
  (let ((*to-register* register))
    `(((to ,register) microlisp-shared::do-restore))))

;; save
(defufn save (thingo :args-last t :expansion ((:from *stack*) (:to *stack*))) ; thingo should have already been evaluated
  (let ((*to-register* '*stack*) ; ignore that this is a hidden to register since it has to be handled special anyway
        (*constituent-assignment-fn* `(assign *stack*)))
    ;; inefficient & temporary? should generate an address
    
    (compile-embedded-expression `(&cons ,thingo (fetch *stack*))))) ; &cons is :args-last but that's ok

;; general ucode operations 

;; tag
(defufn tag (tagname)
  (list tagname)) ; well that seems to be how MIT did it... relying on the assembler... 

;; go-to
;; for the most part, go-to can be put on the prior
;; microinstruction. But for now we'll make life easier and give it
;; it's own instruction line (maybe optimize/compress it away when we
;; have the context?)
(defufn go-to (tag)
  ;; emit a no-op with the tag that we can backpatch later when all
  ;; the tags have known offsets don't check this here - assembler
  ;; will do it as we generate tags internally too
  
  ;;(assert (microcode-symbol tag) (tag) "Not a type or tag we can goto: ~s" tag)
  `(((go-to ,tag))))
  ;(generate-ucode 'no-op nil nil tag))

;; dispatch-on-stack as far as I can tell this takes the TYPE field
;; from the stack pointer and converts that to our uPC via the tags
;; declared in **non-pointer-types** (and maybe **pointer-types**). So
;; what we have to do is get the type field which is straightforward
;; enough, and look up the uPC equivalent which we can get in our jump
;; table. While if this were any other machine, I would expect the
;; stack to be POP'd, I don't see any immediate rationale for doing
;; that here (yet), and didn't see any coroborating comments in the
;; TRs
(defufn dispatch-on-stack ()
  `(((from *stack*) microlisp-shared::type-dispatch)))

;; dispatch-on-exp-allowing-interrupts
(defufn dispatch-on-exp-allowing-interrupts ()
  `(((from *exp*) (clear-mask-interrupts) microlisp-shared::type-dispatch)))

;; dispatch
;; general form of above?
(defufn dispatch (reg-ref)
  (declare (ignore reg-ref)) ; should already be in the *from-register*
  `(((from ,*from-register*) microlisp-shared::type-dispatch)))

;; "There is a register for the current expression, EXP. When the
;; value of an expression is determined it is put into the VAL
;; register. When the arguments for a procedure call are being
;; evaluated at each step the result in VAL is added to the list of
;; already evaluated arguments kept in ARGS. When the arguments are
;; all evaluated, the procedure is invoked. This requires that the
;; formal parameters of the procedure be bound to the actual
;; pareameters computed. This binding occurs by the DISPLAY register
;; getting the contents of the ARGS register prefixed to the
;; environment pointer of the closed procedure being applied. When
;; evaluating the arguments to a procedure, the evaluator may have to
;; recurse to obtain the value of a sub-expresson. The state of the
;; evaluator will have to be restored when the subexpression returns
;; with a value. This requires that the state be saved before
;; recursion. The evaluator maintains a pointer to the stack of
;; pending returns and associated state in the register called STACK."

;; eval-exp-popj-to

;; in AIM 514, popj-return is defined as:
;;
;; (setq exp (car clink))
;; (setq clink (cdr clink))
;; (funcall exp)

;; in other words, the object to be evaluated is popped off the stack and then
;; called.

;; here we're presented with a tag to go to (rather than a return) and
;; usually that tag is "internal-apply" which presumably is doing the
;; work of the evaluation of the next exp from val. So what is this
;; function doing?

;; The internal-apply (tag) sets up exp and dispatches as well as sets
;; up args. Is there anything left for this fn to do other than the
;; goto? Is there something we should be getting from the stack?

;; so looking at the current encoding, (where we put the GLOBAL pointer on the
;; stack for the continuation function, may want to do something else in the
;; future which could require this function to be modified!) at the time we
;; call this, exp has the pointer to the global cell; val is not correct; stack
;; is a SEP to (*args* . std-rtn stack-next) which is what internal-apply seems
;; to expect (other than something useful in val). So we need to set dispatch
;; on the exp (maybe without allowing interrupts?) which will set val up to the
;; global. BUT that also means we need to set up a retpc on the stack register
;; so the exp evaluation has a continuation.

(defufn eval-exp-popj-to (tag)
  ;; `(((go-to ,tag))) ; at least goto the tag?
  (compile-embedded-expression
   `(microlisp:progn
      (&set-type *stack* ,tag)
      (dispatch (fetch *exp*)))))
      

;; "Micro-call is a microcode macro operation which stashes the (micro
;; code) return address specified by its second argument in the type
;; field of *retpc-count-mark* and then goes to the micro-code address
;; specified by its first argument. Micro-return is used to dispatch
;; on this saved type field."

;; micro-call
(defufn micro-call (target-tag return-address-tag)
  (compile-embedded-expression
   `(microlisp:progn
      (&set-type *retpc-count-mark* ,return-address-tag)
      (go-to ,target-tag))))

;; micro-return
(defufn micro-return ()
  `(((from *retpc-count-mark*) microlisp-shared::type-dispatch)))

