;; Time-stamp: <2022-02-03 12:29:10 gorbag>

;; This test builds on the basic stuff (stack manipulation) we did in test-0. Now
;; we will play with the type fields of the pointers and implement the chip's GC 
;; function. This will allow us to set up a freelist. Once we've done that we halt 
;; so we can check if the freelist is valid. If so, we'll have successfully tested 
;; using type fields on pointers, dispatching off the stack, and a fairly complex
;; bit of code that will exercise a number of conditions.


(defschip **pointer-types**
    '((self-evaluating-pointer 0) 
;      (symbol 1)
;      (global 2)
;      (set-global 3)
;      (conditional 4)
;      (procedure 5)
;      (first-argument 6)
;      (next-argument 7)
;      (last-argument 10)
;      (apply-no-args 11)
;      (apply-1-arg 12)
;      (primitive-apply-1 13)
;      (primitive-apply-2 14)
;      (sequence 15)
;      (spread-argument 16)
;      (closure 17)
;      (get-control-point 20)
;      (control-point 21)
;      (interrupt-point 22)
;      (self-evaluating-pointer-1 23) 
;      (self-evaluating-pointer-2 24) 
;      (self-evaluating-pointer-3 25) 
;      (self-evaluating-pointer-4 26)
      ))

(defschip **non-pointer-types** 
   '((self-evaluating-immediate 100)
;     (local 101)
;     (tail-local 102)
;     (set-local 103)
;     (set-tail-local 104) 
;     (set-only-tail-local 105)
;     (primitive-car 106) ;Built-in operators
;     (primitive-cdr 107) 
;     (primitive-cons 110) 
;     (primitive-rplaca 111) 
;     (primitive-rplacd 112) 
;     (primitive-eq 113)
;     (primitive-type? 114) 
;     (primitive-type! 115) 
     (gc-special-type 116) ;Never appears except during gc
;     (self-evaluating-immediate-1 117)
;     (self-evaluating-immediate-2 120) 
;     (self-evaluating-immediate-3 121) 
;     (self-evaluating-immediate-4 122) 
     (mark 123)
     (done 124)
;     (primitive-add1 125)
;     (primitive-sub1 126)
;     (primitive-zerop 127)
;     (primitive-displacement-add1 130) 
;     (primitive-not-atom 131)
     (boot-load 776)                     ;micro-address forced by RESET
;     (process-interrupt 777)             ;micro-address forced by EXT INT RQ
     ))

(defschip **pointer** 100)                    ;100 bit means non-pointer.

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

(defreg *exp* (to-type to-displacement to-frame from from-decremented) ())

(defreg *newcell* (to-type to-address from from-incremented) (address=bus))

(defreg *val*
    (to-type to-address from)
    (type=bus address=bus =bus))               ;=bus is AND of type, address=bus

(defreg *retpc-count-mark* (to-type to-address from) ())

(defreg *stack* (to-type to-address from from-type) ()) ;; BWM added from-type for type-dispatch

(defreg *memtop* (to from) ())

(defreg *args* (to from) ())

(defreg *display* (to from) ())

(defreg *intermediate-argument* (to from) ())

(defreg *nil* (from) ())

;; we use a more complete version of this in machine-defs.lisp
(defreg bus
    () (mark-bit type-not-pointer frame=0 displacement=0 address=0))

(deftype boot-load
  (assign *scan-up* (fetch *nil*))
  (&increment-scan-up)                               ; to location 1
  (assign *memtop* (&car (fetch *scan-up*)))
  (assign *scan-up* (fetch *memtop*))
  (assign *stack* (&get-interrupt-routine-pointer)) ;from pads
  ;; the following was NOT in test-0 - we set up a type on the stack such that
  ;; we can execute a return in the microcode similar to a subroutine call
  (&set-type *stack* boot-load-return)
  (go-to mark))

;; ok here's where we return to after we've done with the initial gc
;; compacting memory and setting the pointer(s)

(defreturn boot-load-return
   (assign *exp* (&car (fetch *stack*))) 
   (assign *stack* (fetch *nil*))
   (&set-type *stack* done)
;; we'll stop the test here and see if our freelist is correctly set up
   (go-to done)
;   (dispatch-on-exp-allowing-interrupts)
    )

   
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
;; the in- use and car-trace-in-progress bit. The in-use bit is stored in
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
         (dispatch-on-stack)) ; [should be to boot-load-return in test-1! BWM]
        (t (&decrement-scan-down)
           (go-to relocate-pointers))))

;;   Congratulations, you have just survived the garbage collector! 
