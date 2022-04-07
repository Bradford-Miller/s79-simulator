(in-package :diagnostics)

(scheme-79:scheme-79-version-reporter "S79 Diagnostics Support" 0 4 0
                                      "Time-stamp: <2022-03-18 15:27:02 gorbag>"
                                      "0.4 release!")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.4.0   3/18/22 snapping a line: 0.4 release of scheme-79 supports test-0 thru test-3. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.7   1/ 6/22 update more pacakge refs

;; 0.1.6  12/ 3/21 update scheme-79-mcr-i -> "" (if from microlisp-int or fpga-pla-build-tools)

;; 0.1.5  11/ 4/21 call specialized update-diagnostics, not just redraw-diagnostics
;;                    when we change the tests.

;; 0.1.4  10/15/21 build microcode tests around ALL microfunctions, not
;;                    just those 'in use'

;; 0.1.3  10/13/21 *last-conditional-result*
;;                    fix checking for existing test to check for
;;                    success-predicate name existing already

;; 0.1.2  10/12/21 use :junk-allowed t on parse-integer when trying to
;;                    determine while files belong to a test and just
;;                    ignore those that aren't.

;; 0.1.1   9/ 8/21 move the more generic functions and definitions to
;;                    support/fpga-diagnostics

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.5 08/19/21 increment the number of successes and fails by the number of incremental runs

;; 0.0.4 08/18/21 move *test-suite* defvar here

;; 0.0.3 08/14/21 add conditional-p to tests so we can check both branches of suitable microcode

;; 0.0.2 08/13/21 add test number to test suite objects (to make it
;;                    easier to run them automatically via (test :n n))

;; 0.0.1 08/10/21 Sort test objects to make them easier to find in the
;;                    diagnostics display

;; 0.0.0 08/09/21 New

(defvar *test-suite* nil
  "Used when running a test suite (diagnostic-test-group)")

(defvar *nanotest-objects* nil)

(defvar *last-conditional-result* nil
  "record result of last conditional evaluation in set-micro-pc for later update to diagnostic record")

;; note this will add to existing nanotests that may have already been run
(defun create-nanotest-objects (&optional associated-groups)
  #+capi (declare (special s79-console:*diagnostics-interface*)) ; hasn't been defined yet
  (mapc #'(lambda (nano-entry)
            (let ((nano-name (car nano-entry)))
              (unless (simple-find-test nano-name *nanotest-objects*)
                (push (make-instance 'diagnostic-test
                                     :name nano-name
                                     :associated-groups associated-groups)
                      *nanotest-objects*))))
          *nanocontrol-symtab*)

    ;;sort
    (setq *nanotest-objects* (sort *nanotest-objects* #'string<= :key #'(lambda (x) (string (test-name x)))))
  
    ;; if we have a panel, we update the tests in the panel
    #+capi (when s79-console:*diagnostics-interface*
             (s79-console:update-nanotests s79-console:*diagnostics-interface*)))

(defvar *microtest-objects* nil)

;; predicates can fail or succeed and we should really make sure both arms get tested.

(defun generate-fail-mt-name (micro-name)
  (intern (format nil "~A-FAIL" micro-name) *scheme-mach*))

(defun generate-success-mt-name (micro-name)
  (intern (format nil "~A-SUCCESS" micro-name) *scheme-mach*))

;; note this will add to existing microtests that may have already been run
(defun create-microtest-objects (&optional associated-groups)
  #+capi (declare (special s79-console:*diagnostics-interface*)) ; hasn't been defined yet
  (mapc #'(lambda (micro-name)
            (unless (simple-find-test micro-name *microtest-objects*)
              (let ((declarations (cdr (microcode-declarations micro-name))))
                (cond
                 ((and declarations (member :conditional declarations))
                  (let ((success-sym (generate-success-mt-name micro-name))
                        (fail-sym (generate-fail-mt-name micro-name)))
                    (push (make-instance 'diagnostic-test
                                         :name success-sym
                                         :conditional-p :succeed
                                         :associated-groups associated-groups)
                          *microtest-objects*)
                    (push (make-instance 'diagnostic-test
                                         :name fail-sym
                                         :conditional-p :fail
                                         :associated-groups associated-groups)
                          *microtest-objects*)))
                 (t
                  (push (make-instance 'diagnostic-test
                                       :name micro-name
                                       :conditional-p nil
                                       :associated-groups associated-groups)
                        *microtest-objects*))))))
        (all-microfunctions)) ; use all the defined ones, not just the
                              ; ones "in use" since we're trying to
                              ; determine coverage!

  ;;sort
  (setq *microtest-objects* (sort *microtest-objects* #'string<= :key #'(lambda (x) (string (test-name x)))))
  
  ;; if we have a panel, we update the tests in the panel
  #+capi (when s79-console:*diagnostics-interface*
           (s79-console:update-microtests s79-console:*diagnostics-interface*)
           ))

(defvar *predicate-test-objects* nil)

(defun create-predicate-test-objects (&optional associated-groups)
  #+capi (declare (special s79-console:*diagnostics-interface*)) ; hasn't been defined yet
  (mapc #'(lambda (pred-name)
            (unless (not (null (simple-find-test (generate-success-mt-name pred-name) *predicate-test-objects*))) ; if success arm is there, then so is fail
              (let ((success-sym (generate-success-mt-name pred-name))
                    (fail-sym (generate-fail-mt-name pred-name)))

                (push (make-instance 'diagnostic-test
                                     :name success-sym
                                     :conditional-p :succeed
                                     :associated-groups associated-groups)
                      *predicate-test-objects*)
                (push (make-instance 'diagnostic-test
                                     :name fail-sym
                                     :conditional-p :fail
                                     :associated-groups associated-groups)
                      *predicate-test-objects*))))
        (predicates-used)) ; populated once we load microcode

  ;;sort
  (setq *predicate-test-objects* (sort *predicate-test-objects* #'string<= :key #'(lambda (x) (string (test-name x)))))
  
  ;; if we have a panel, we update the tests in the panel
  #+capi (when s79-console:*diagnostics-interface*
           (s79-console:update-predicates s79-console:*diagnostics-interface*)))

(defvar *test-suite-objects* nil)

;; note this will REPLACE test suites with whatever is in the tests subdirectory
(defun create-test-suite-objects (&optional associated-tests)
  #+capi (declare (special s79-console:*diagnostics-interface*)) ; hasn't been defined yet
  (setq *test-suite-objects*
        (mapcan #'(lambda (directory-entry)
                    (let ((name (pathname-name directory-entry))
                          (type (pathname-type directory-entry)))
                      ;; pull out the ones of the form test-n.lisp
                      (when (and (equalp type "lisp")
                                 (equalp (subseq name 0 5) "test-"))
                        (let*-non-null ((test-number (parse-integer (subseq name 5) :junk-allowed t)))
                          (list (make-instance
                                 'diagnostic-test-group
                                 :name name
                                 :number test-number
                                 ;; default test is named check-test-n, and defined in test-n.lisp
                                 :associated-tests associated-tests
                                 :result-interpreter (intern (format nil "CHECK-TEST-~d" test-number) *scheme-mach*)))))))
                (directory "tests/")))
  
  ;; if we have a panel, we update the tests in the panel
  #+capi (when s79-console:*diagnostics-interface*
           (s79-console:update-test-suites s79-console:*diagnostics-interface*)))

;; when the test is loaded, we can call this function to populate the associated tests AND the nano and microtest objects for that test.
(defun declare-diagnostic-suite (name)
  ;; we should already have a test-suite object that matches name, we
  ;; just need to update it and create the other objects based on what
  ;; the validator found.
  (let ((suite (simple-find-test name *test-suite-objects*))
        (associated-tests (append (create-microtest-objects)
                                  (create-predicate-test-objects)
                                  (create-nanotest-objects))))
    (setf (group-associated-tests suite) associated-tests)
    (mapc #'(lambda (test)
              (pushnew name (test-associated-groups test)
                       :test #'equalp))
          associated-tests)
    suite))

(defun clear-metrics ()
  (flet ((cm (test-list)
           (mapc #'(lambda (test)
                     (setf (test-num-runs test) 0)
                     (setf (test-num-successful test) 0)
                     (setf (test-num-failed test) 0)
                     (setf (test-run-p test) nil)
                     (slot-makunbound test 'result))
                 test-list)))
    (cm *microtest-objects*)
    (cm *predicate-test-objects*)
    (cm *nanotest-objects*)))
  
;; generally, markup of the tests is done via the functions in the diagnostic-panel
;; (since the variables are kept in the interface). However, generalized functions here
;; can help with defining diagnostics (e.g., how to run the test or determine success
;; automatically).

