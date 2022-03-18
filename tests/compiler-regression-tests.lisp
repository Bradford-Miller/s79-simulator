(defpackage :s79-regression-tests
  (:use :scheme-mach :microlisp-int :microlisp-shared :fpga-pla-build-tools
  :fpga-project-defs :common :scheme-79 :scheme-shared :fiveam :cl-lib
  common-lisp)
  (:shadowing-import-from :fiveam #:run)
  (:export #:run-compiler-test-suite #:run-s79-test-suite))

(in-package :s79-regression-tests)

(scheme-79:scheme-79-version-reporter "S79 Regression Tests" 0 4 0
                                      "Time-stamp: <2022-02-03 17:17:10 gorbag>"
                                      "line disambiguation")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.4.0   3/18/22 snapping a line: 0.4 release of scheme-79 supports test-0 thru test-3. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.3.1   2/ 9/22 way too many things (fns, variables) with "line" in their name
;;                    and it's ambiguous.  Splitting so "line" refers to,
;;                    e.g. an output (log) line, "expression" refers to a
;;                    'line' of code (single expression in nano or microcode
;;                    land typically, and because we used (READ) it wasn't
;;                    confined to a single input line anyway) and "wire" to
;;                    refer to, e.g., a control or sense 'line' on a register.

;; 0.3.0   2/ 3/22 New package to do regression tests on the compiler (where most
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

;; assembly code can have the first two arguments (from and to elements) reversed so check for that
;;

(defun assy-equalp (assy1 assy2)
  "given two expressions of upla assembly code, check if they are equivalent"
  (let ((assy1-from (assoc 'from assy1))
        (assy2-from (assoc 'from assy2))
        (assy1-to (assoc 'to assy1))
        (assy2-to (assoc 'to assy2))
        (assy1-rem (remove-if #'(lambda (x) (and (consp x) (member (car x) '(from to)))) assy1))
        (assy2-rem (remove-if #'(lambda (x) (and (consp x) (member (car x) '(from to)))) assy2)))
    (and (equalp assy1-from assy2-from)
         (equalp assy1-to assy2-to)
         (equalp assy1-rem assy2-rem))))

(defun test-expression (exp expected-result)
  ;; similar to debug-compile-expression but we capture the stream and use it to
  ;; generate the result to compare to
  (let* ((test-stream (make-string-output-stream))
         (*upla-stream* test-stream))
    (compile-expression exp)
    (let ((result (read-from-string (get-output-stream-string test-stream) nil :eof)))
      (is (assy-equalp expected-result result)))))

(defun test-multi-expression (exp expected-results)
  ;; multiple results are generated
  (let* ((test-stream (make-string-output-stream))
         (*upla-stream* test-stream))
    (compile-expression exp)
    (let ((output-string (get-output-stream-string test-stream))
          (next-char 0)
          (result nil))
      (dolist (expectation expected-results)
        (msetq (result next-char) (read-from-string output-string nil :eof :start next-char))
          (is (assy-equalp expectation result))))))

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

(test simple-unmark
  "simple &unmark!"
  (test-multi-expression '(&unmark! (fetch *scan-down*))
                         '(((from *scan-down*) (to *intermediate-argument*) do-car)
                           ((from *intermediate-argument*) (to *scan-down*) write-and-unmark-car)))
  )

;;somewhat problematic because tags (generated by the cond macro)
;;are gensyms, so need to have a better way to compare them... will have to 
;; use a pattern matcher?

#||
(test multi-arm-cond
  "cond with multiple clauses"
  (test-multi-expression '(microlisp:cond 
                           ((&scan-up=scan-down?) (go-to relocate-pointers))
                           ((&in-use? (fetch *scan-down*)) (go-to scan-up-for-hole))
                           (t (go-to scan-down-for-thing)))
                         '()))
||#

;; (much) more to come I'm sure...

(defun run-compiler-test-suite ()
  (run! 'compiler-test-suite))

(defun run-s79-test-suite ()
  (run! 's79-test-suite))
