;; Time-stamp: <2021-11-23 17:42:31 gorbag>

;; in our first test we will create a simple boot function that will read and write to memory.
;; we copy our definitional stuff from the microcode.mcr file

(defschip **pointer-types**
    '((self-evaluating-pointer 0)))      ; not used but need at least one; will generate a warning!

(defschip **non-pointer-types** 
   '((self-evaluating-immediate 100)     ; 10/12/21 add this to generate correct type for stack objects
     (done 124)
     (boot-load 776)                     ;micro-address forced by RESET
;     (process-interrupt 777)             ;micro-address forced by EXT INT RQ (not used yet)
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


;; ok here's the test
(deftype boot-load
  ;; from the original boot-load code... check assign, fetch (mover nanocode)
  (assign *scan-up* (fetch *nil*)) ; should get 0

  ;; check increment
  (&increment-scan-up) ; pointing to 1

  ;; now accessing external memory
  (assign *memtop* (&car (fetch *scan-up*))) ; see test-0.lisp

  ;; now a hack to initilize the freelist
  (&increment-scan-up) ; 2
  (&increment-scan-up) ; 3
  (&increment-scan-up) ; 4 ; last entry of stack and therefore should be where newcell points to (i.e. 5 will be next cons)

  ;; note that scan-up is the same register as newcell, so what is at address 5 becomes the next CONS cell
  ;; reading pads - needs assist from peripheral and set up in code in test-0.lisp
  (assign *stack* (&get-interrupt-routine-pointer)) ; should get a 2

  ;; ok now lets exercise things a bit - *stack* should point to an actual stack we can restore from or save to.
  
  (restore *display*) ; pop should be #o1777, *stack* should now point to addess 3.
  
  (restore *val*) ; pop should be #o2777, *stack* should now point to address 4.
  
  (save (fetch *display*)) ; *stack* should now point to address 5: (#o1777, #o4); Newcell should now point to 5
  
  (save (fetch *val*)) ; *stack* should now point to address 6: (#o2777, #o5); Newcell should now point to 6
  
  ;; we should have reversed the two items on the stack, but note it will be in a new position!
  (go-to done))

(deftype done ; loop when done
  (go-to done))

