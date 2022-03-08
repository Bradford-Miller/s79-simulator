(in-package :scheme-mach)

(scheme-79:scheme-79-version-reporter "Scheme Mech Test Support" 0 3 3
                                      "Time-stamp: <2022-02-25 17:27:00 gorbag>"
                                      "test: clear-memory")

;; 0.3.3   2/25/22 add clear-memory call to test so we can more easily see what changes
;;                     (particularly when we call test multiple times)

;; 0.3.2   1/26/22 support *microcode-compiled*

;; 0.3.1   1/12/22 load microcode before the .lisp file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.1.5  1/24/22 test: load microcode first as test-n.lisp may need it

;; 0.1.4  1/ 6/22 update packages due to migration

;; 0.1.3 12/ 6/21 moved test fn here

;; 0.1.2 12/ 2/21 add defparameter forms for goals of other registers
;;                     (e.g., exp, val, etc.)

;; 0.1.1 10/15/21 move defparamter forms here, SET them in test-n.lisp
;;                     rather than redefine them (as we may run
;;                     multiple tests in a row!)

;; 0.1.0  10/12/21 New - support functions for the various test-n.lisp
;;                     files (may move to FPGA Dev support at some point)

(defparameter *goal-memory-array* nil
  "This is what we expect memory to look like after we've run")

(defparameter *goal-memory-offset* 2
  "This is the start address for *goal-memory-array*")

(defparameter *goal-memtop* #o10)

(defparameter *goal-exp* #o0)

(defparameter *goal-val* #o0)

(defparameter *goal-retpc-count-mark* #o0)

(defparameter *goal-args* #o0)

(defparameter *goal-stack* #o0)

(defparameter *goal-display* #o0)

(defparameter *goal-newcell* *goal-stack*)

(defvar *current-test* nil
  "for debugging/diagnostics")

(defvar *microcode-compiled* nil
  "so when we laod test-<n>.lisp we can tell if we did validate-only-p")

;; we use this function to allow us to cons up constants for tests in
;; a way that's clear to the reader what the value of the cons is
;; (specifically the type).
(defun make-word (type data)
  (let* ((temp (make-register)) ; final bit-vector of appropriate size
         (type-field (make-type-field temp))
         (data-field (make-data-field temp)))
    (declare (dynamic-extent temp))

    ;; we can expect mismatches in the length as source is integer, so
    ;; turn off verbosity
    (copy-field (integer->bit-vector type) type-field :verbose-p nil) 
    (copy-field (integer->bit-vector data) data-field :verbose-p nil)
    temp))

;; to make testing easier - we will redefine this from time to time!
(defun test (&key (n nil number-p) (validate-only-p nil))
  (declare (ignorable number) ; because we edit this from time to time
           (special microlisp-shared:*micro-pc* microlisp-shared:*nano-pc* diagnostics:*test-suite*)) ; load order
  ;; increasing numbers tests more microcode/nanocode compilation sets. Note as a test,
  ;; it should also set up some external memory so we can begin executing without loading
  ;; individual cells from the front panel.
  ;;
  ;; since we're testing, clear the memory (easier to tell what changes)
  (clear-memory)
  
  (let ((*microcode-compiled* nil))
    (cond
     (number-p
      (let ((test-setup-file (format nil "src:scheme-79;s79-simulator;tests;test-~d.lisp" n))
            (test-ucode-file (format nil "src:scheme-79;s79-simulator;tests;test-~d.mcr" n)))
        (cond
         (validate-only-p
          (read-microcode-for-interpreter test-ucode-file))
         (t 
          (compile-microcode test-ucode-file)
          (setq *microcode-compiled* t)))
        (load test-setup-file)) ; do we need to do this if we are only validating?

      ;; declare the test and set up the diagnostics console (if available)
      (setq diagnostics:*test-suite*
            (diagnostics:declare-diagnostic-suite ; sets up associations based on what's been loaded
             (setq *current-test* (format nil "test-~d" n))))) 
     (validate-only-p 
      ;; validates the entire (original) microcode. Note that this happens
      ;; if compile-microcode is called as well since it sets up some tables 
      ;; the compiler needs
      (read-microcode-for-interpreter *default-microcode*))
     (t
      ;; validates and compiles the microcode into a binary format
      ;; suitable for storing on-chip (as an array).
      (compile-microcode *default-microcode*)))
    ;; if the diagnostics panel is already running... update it
    (when (diagnostics-running-p)
      (s79-console:update-diagnostics (diagnostics-running-p) t))

    (unless validate-only-p ;; don't fiddle with the machine if we're just trying to validate the microcode
      (reset) ; flip the reset pad

      ;; clear out PCs
      (scheme-mach:set-micro-pc 0)
      (copy-field (integer->bit-vector 0) microlisp-shared:*nano-pc*))))
