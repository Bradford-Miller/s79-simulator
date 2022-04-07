;; Time-stamp: <2022-04-07 12:44:16 gorbag>

;; implement a test function that checks GC correctly sets up a free list

;; like test-0, we create a simple stack but it will be more or less invariant during the test.
;; unlike test-0, we make the stack have holes so GC will have to do some consolodation!

(cl:in-package :scheme-mach)

;; init-memory-for-cold-boot will set address 0 to (0 . 0) and 1 to
;; (*initial-memtop* . ?) so starting with address 2,
;; *cold-boot-memory-array* will be copies.

;; #o0 is self-evaluating0pointer, so address 2 (which should point to
;; #our evaluable expression) is address 3 (which will turn out to be
;; #a constant list.

;; #o100 is self-evaluating-immediate

;; so what we expect after this runs is for the evaluable expression to have been compressed and the rest is garbage, with *exp* being a pointer to the evaluable expression.

(eval-when (:load-toplevel :execute)
  (setq *cold-boot-memory-array*
        ;; short so we'll use list format

        ;; start with address 2 - where we will begin executing
        ;; code. CAR should be a pointer to the code though for test-1
        ;; we won't actually execute it; it gets stuffed into *exp* in
        ;; boot-load-return and stack gets nulled out.

        ;; we point to a constant list just so we have some
        ;; non-garbage to maintain by the GC.

        ;; CAR                        CDR
        `(
          ,(make-word #o0 #o3)      ,(make-word #o0 #o0) ; address 2 (see above)
          ,(make-word #o100 #o1777) ,(make-word #o0 #o4) ; address 3 - first word of what we 'execute' (->4->7)
          ,(make-word #o100 #o2777) ,(make-word #o0 #o7) ; address 4
          ;; note the next CONS points to another address on the
          ;; freelist just to confuse the GC (well hopefully not!)
          ;; This CONS shouldn't get MARKed and thus after compacting
          ;; only addresses 2, 3 and 4 should be live
          ,(make-word #o100 #o4777) ,(make-word #o0 #o10); address 5 (garbage) 
          #o0        #o0      ; address 6
          ,(make-word #o100 #o3777) ,(make-word #o0 #o0) ; address 7 - last word of initial expression
          #o0        #o0      ; address 8 etc. (freelist)
          ))

  ;; when appropriate set interrupt routine pointer to 2,
  ;; note that the microcode will ask for it to assign to *stack*.
  (setq external-chips:*interrupt-address* 2)

  ;; we want enough memory to set up a freelist, but not so much we have a lot to check. Let's make it
  ;; 32 words since that's reasonable to manually look through. We'll increase it later once we
  ;; automate testing.
  (setq *initial-memtop* (max 32
                              (/ (+ 2 (length *cold-boot-memory-array*)) 2))) ; not shorter than our array though

  ;; since we don't put in the deftype for self-evaluating-pointer we'll get a warning.
  (format *error-output* ";;~%;; NB: warning about declared but undefined types are to be expected with this test 
;;     (in particular gc-special-type, self-evaluating-pointer, and self-evaluating-immediate).~%;;~%")
  
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
(defun check-test-1 ()
  (note "Checking test-1's result. 
This test checked that the GC code is working correctly by consolodating 
free space and setting up pointers to the initial free CONS")
  (cond
    ((and
      (s79-console:compare-memory *goal-memory-array*
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
