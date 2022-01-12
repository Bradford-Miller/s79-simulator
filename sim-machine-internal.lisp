(in-package :scheme-mach)

(scheme-79:scheme-79-version-reporter "Scheme Machine Sim Int Ops" 0 3 0
                                      "Time-stamp: <2022-01-11 15:23:49 gorbag>"
                                      "0.3 release!")

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
;;                       it safely)

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
;; order bit of micro-pc to OR of relevant conditional lines

;nano runs on :ph2-rising, and we keep high until we update PC on :ph1-falling
(defchip-pad *conditional* :pseudo *run-nanocontroller-p1* *update-sense-lines* 7)

;; pseudo pad for the internal version of freeze. So we can distinguish between an internally
;; driven freeze (nanocode) and external (generally memory cycle delay)
(defchip-pad *internal-freeze* :latched-io *run-nanocontroller-p1* :any 8) ; full clock cycle as we may reassert

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

;(defufn &frame=0? ()
;  (zerop-field *bus-frame*))

;; &displacement=0? ;; bus operation

;(defufn &displacement=0? ()
;  (zerop-field *bus-displacement*))

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
;; register control lines, so it's less of a loss of extra bits).
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

;; &decrement-displacement

;; &decrement-frame

;; pointer bit - this instruction fetches onto the bus and checks the bit
;; &pointer?
(defupred &pointer? (type=pointer nil *bus*)) ; only bus can check this

;; not in the original machine, but for completeness (inverse)
(defupred &type? (type=type nil *bus*))

(defupred-inverse &pointer? &type?)

;; &=type?  
(defupred &=type? (type=bus nil))

;; assign
(defufn assign (to-register from :args-last t) ; args-last functions get special handling in compile-line & compile-parameter
  ;; set up the to field, then we'll evaluate the rest of the expression inside of compile-line
  (declare (ignore from))
  (setq *to-register* to-register)) ; there can be more than one, but lets only allow one for now

;; fetch
(defufn fetch (from-register :constituent t)
  ;; set up the from field. Should be inside of some other microcode.
  (setq *from-register* from-register)
  (ecase *enclosing-opcode*
    ((assign
      save) ; save set up the to register as the stack

     (cl:if (special-register-p (translate-alias *to-register*))
         ;; can't just use mover since that needs a TO control
         `(((from ,from-register) (to ,*to-register*)
            ,(intern (format nil "MOVE-TO-~A" (strip-register-name (translate-alias *to-register*)))
                     *ulang-shared-pkg*)))
         `(((from ,from-register) (to ,*to-register*) microlisp-shared::mover))))
    ((&rplaca &rplaca-and-mark! &rplaca-and-unmark!
      &mark-in-use! &unmark!

      &rplacd &rplacd-and-mark-car-being-traced! &rplacd-and-mark-car-trace-over! 
      &mark-car-being-traced! &mark-car-trace-over! 

      &set-type)
     from-register) ; just send back the register
    ))

;; restore
(defufn restore (register)
  (let ((*to-register* register))
    (cl:if (special-register-p (translate-alias register))
           `(((to ,register) ,(intern (format nil "DO-RESTORE-~A" (strip-register-name (translate-alias register)))
                                      *ulang-shared-pkg*)))
           `(((to ,register) microlisp-shared::do-restore)))))

;; save
(defufn save (thingo :args-last t :expansion ((:from *stack*))) ; thingo should have already been evaluated
  (let ((*to-register* '*stack*)) ; ignore that this is a hidden to register since it has to be handled special anyway
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

;; dispatch-on-exp-allowing-interrupts

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

;; dispatch

;; eval-exp-popj-to

;; micro-call

;; micro-return

;; if




