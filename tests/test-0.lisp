;; Time-stamp: <2022-02-04 17:58:33 gorbag>

;; implement a test function that checks memory and some registers for correct values
;; set up *cold-boot-memory-array* so reset will setup external memory correctly for the test

(in-package :scheme-mach)

;; init-memory-for-cold-boot will set address 0 to (0 . 0) and 1 to
;; (*initial-memtop* . ?) so starting with address 2,
;; *cold-boot-memory-array* will be copied.

;; note that in this first test we don't have GC yet, so types don't
;; really matter but we set it anyway.

;; OK 10/11/21: finally realized this chip doesn't have a freelist!
;; Instead it is just the top N of memory, which the GC consolodates
;; when it runs. SO I've changed the test in terms of eliminating the
;; addresses within the freelist, and noted which words in the memory
;; didn't change but are now technically "garbage".

;; NB: type #o100 is self-evaluating-immediate, and #o0 is
;; self-evaluating-pointer

(eval-when (:load-toplevel :execute)
  (setq *cold-boot-memory-array*
        ;; short so we'll use list format
        ;; CAR                       CDR
        `(,(make-word #o100 #o1777) ,(make-word #o0 #o3) ; address 2 - start of stack; cdr is next stack address
          ,(make-word #o100 #o2777) ,(make-word #o0 #o4) ; address 3
          ,(make-word #o100 #o3777) ,(make-word #o0 #o0) ; address 4 - last entry of stack and shouldn't be touched)))
          #o0        #o0      ; address 5 (start of freelist) 
          #o0        #o0      ; address 6
          #o0        #o0      ; address 7
          #o0        #o0      ; address 8
          #o0        #o0      ; address 9 - end of our freelist and memory (for this test)
          ))

  ;; when appropriate set interrupt routine pointer to 2, note that
  ;; the microcode will ask for it to assign to *stack*.
  (setq external-chips:*interrupt-address* 2)

  ;; add four half-words for init-memory-for-cold-boot usage, then
  ;; subtract 2 for zero offset (that is, 0 is first address)
  (setq *initial-memtop* (/ (+ 2 (length *cold-boot-memory-array*)) 2)) 

  (format *error-output*
          ";;~%;; NB: warning about declared but undefined types are to be expected with this test
;;     (in particular self-evaluating-pointer and self-evaluating-immediate).~%;;~%");~%")
  
  (setq *goal-memory-array* ; use defparameter so we can redefine in other test-n.lisp files
        ;; CAR                            CDR
        `(,(make-word #o100 #o1777)       #o3  ;2 <- garbage
          ,(make-word #o100 #o2777)       #o4  ;3 <- garbage
          ,(make-word #o100 #o3777)       #o0  ;4 <- last entry of stack (unchanged!)
          ,(make-word #o100 #o1777)       #o4  ;5 <- middle of stack (new)
          ,(make-word #o100 #o2777)       #o5  ;6 <-*stack*; head of stack <-*newcell*
          #o0                             #o0  ;7 <- (still) empty
          #o0                             #o0  ;8 <- end of freelist
          #o0                             #o0  ;9 <- memtop    
          ))

  (setq *goal-stack* #o6)

  (setq *goal-memtop* *initial-memtop*)

  (setq *goal-newcell* *goal-stack*) ; last allocated cons was for the stack

  (setq *goal-memory-offset* 2)
  )

;; set up a test for successful completion
(defun check-test-0 ()
  ;; check memory is correctly arranged and registers have what we
  ;; expect we can do this in two ways - the logical one would look at
  ;; the stack register and then at the memory linked to that.  BUT
  ;; since the machine is derterministic, we can just look at the
  ;; final memory layout and see if it's exactly what we expect.

  (note "Checking test-0's result. 
This test checked that basic operations needed to boot and stack save/restore 
operations are functioning correctly. If this works, test-1 should be run.")
  (cl:cond
    ((and (external-chips:compare-memory *goal-memory-array*
                                         *goal-memory-offset*
                                         (/ (length *goal-memory-array*) 2)
                                         :warn))
     ;; these are the registers that "matter" for this test
     (= (bit-vector->integer microlisp-shared:*newcell*) *goal-newcell*)
     (= (bit-vector->integer microlisp-shared:*memtop*) *goal-memtop*)
     (= (bit-vector->integer microlisp-shared:*stack*) *goal-stack*)
     t)
    (t
     nil)))
