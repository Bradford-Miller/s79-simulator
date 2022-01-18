;; Time-stamp: <2021-03-03 17:46:46 gorbag>"

;; This is intended to be the starting point for test microcode,
;; consisting of the declarations without any actual code and drawn
;; from the actual microcode, delete or simplify what isn't needed.

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

(defreg *stack* (to-type to-address from) ())

(defreg *memtop* (to from) ())

(defreg *args* (to from) ())

(defreg *display* (to from) ())

(defreg *intermediate-argument* (to from) ())

(defreg *nil* (from) ())

(defreg bus
    () (mark-bit type-not-pointer frame=0 displacement=0 address=0))
