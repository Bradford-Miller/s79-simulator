(defpackage :s79-compiler-tests
  (:use :scheme-mach :fiveam :cl-lib common-lisp))

(in-package :s79-compiler-tests)

(scheme-79:scheme-79-version-reporter "Scheme Mech Test Support" 0 3 0
                                      "Time-stamp: <2022-02-03 17:17:10 gorbag>"
                                      "new")

;; 0.0.0   2/ 3/22 New package to do regression tests on the compiler (where most
;;                     of the bugs show up!)

(def-suite s79-test-suite
  :description "general regression tests for scheme-79")

(def-suite compiler-test-suite
  :description "regression tests for the scheme-79 microcode compiler"
  :in s79-test-suite)

(in-suite compiler-test-suite)

;; we will check the compiler output by statement into upla assembler language.
;; this is less likely to change (as it is symbolic) between iterations of
;; ucode definitions we will NOT check the actual (type) constants, etc. as
;; those can easily change (particularly those that are assigned during
;; compilation such as defreturn).

;; As we find/fix bugs in the compiler we will add tests here to ensure against
;; regression.

;; Since compilation normally writes to a file, our test function will write to
;; a string stream and then READ it in to see if it is equivalent to our
;; expected result

(defun test-expression (exp expected-result)
  ;; similar to debug-compile-line but we capture the stream and use it to
  ;; generate the result to compare to
  (let* ((test-stream (make-string-output-stream))
         (*upla-stream* test-stream))
    (compile-line exp)
    (let ((result (read-from-string (get-output-stream-string test-stream) :eof-error-p nil :eof-value :eof)))
      (= expected-result result))))

(defun test-multi-expression (exp expected-results)
  ;; multiple results are generated
  (let* ((test-stream (make-string-output-stream))
         (*upla-stream* test-stream))
    (compile-line exp)
    (dolist (expectation expected-results)
      (let ((result (read-from-string (get-output-stream-string test-stream) :eof-error-p nil :eof-value :eof)))
        (= expectation result)))))

(test simple-assign
      "test simple assignment"
      (test-expression '(assign *stack* (fetch *nil*))
                       '((from *nil*) (to *stack*) mover))
      )

(test complex-assign
      "assignment using car/cdr"
      (test-expression '(assign *stack* (&car (fetch *exp*)))
                       '((from *exp*) (to *stack*) do-car))
      (test-expression '(assign *rel-tem-1* (&cdr (fetch *rel-tem-2*)))
                       '((from *rel-tem-2*) (to *rel-tem-1*) do-cdr))
      )

(test simple-rplac
      "simple version of rplaca/replacd"
      (test-expression '(&rplaca (fetch *scan-down*) (fetch *scan-up*))
                       '((from *scan-up*) (to *scan-down*) write-car))
      )

(test complex-rplac
      "rplac* using car/cdr which requires intermediate argument"
      (test-multi-expression '(&rplaca-and-mark! (fetch *scan-up*) (&car (fetch *scan-down*)))
                             '(((from *scan-down*) (to *intermediate-argument*) do-car)
                               ((from *intermediate-argument*) (to *scan-up*) write-and-mark-car)))
      )

;; (much) more to come I'm sure...

#||

(run! 'compiler-test-suite)

||#
        
    
