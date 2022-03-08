;; writing this as standalone so may want own-package at some point.
(in-package :s79-console)

(scheme-79:scheme-79-version-reporter "Scheme Machine Diag. Panel" 0 3 0
                                      "Time-stamp: <2022-01-31 17:57:41 gorbag>"
                                      "0.3 release!")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.2  10/12/21 add fourth widget for supporting predicates; adjust height so
;;                    first and fourth panels can coexist

;; 0.1.1   8/20/21 remove check-box function - should be set by testing functions

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.3 8/20/21 Aesthetics

;; 0.0.2 8/ 7/21 Add report of statistics for each test (runs, succeeded, failed)

;; 0.0.1 8/ 6/21 Add support for marking all tests that have run as
;;                 successful or failure, the idea being that a common
;;                 use case will be to run sets of tests and noting
;;                 the success only at the end.
;;
;;                 We can still manually override those tests marked
;;                 as, e.g. failure as actually successful while
;;                 debugging happens (e.g., run tests A, B, the result
;;                 is fine, so A&B marked successful.  then run A, C,
;;                 D and have a failure so mark C & D as failed (A
;;                 already succeeded). As we debug we discover C was
;;                 OK so we manually mark that successful leaving D
;;                 (as our agenda for further debugging). Note that we
;;                 can programmatically adjust these so as we get
;;                 smarter and more granular tests we can have the
;;                 test suite itself mark these changes.
;;
;;               Add third panel of tests

;; 0.0.0 8/ 5/21 New

(defparameter *diagnostics-grid-font*
  (gp:make-font-description :family "courier" :weight :medium :slant :roman :size 12))

(gp:register-image-translation
 'check-image
 #.(gp:read-external-image (current-pathname "images/16x16/check.bmp")
                           :transparent-color-index 0))

(gp:register-image-translation
 'cross-image
 #.(gp:read-external-image (current-pathname "images/16x16/cross.bmp")
                           :transparent-color-index 0))

(gp:register-image-translation
 'minus-image
 #.(gp:read-external-image (current-pathname "images/16x16/minus box.bmp")
                           :transparent-color-index 0))

(gp:register-image-translation
 'ok-image
 #.(gp:read-external-image (current-pathname "images/16x16/OK.bmp")
                           :transparent-color-index 0))

(gp:register-image-translation
 'plus-image
 #.(gp:read-external-image (current-pathname "images/16x16/plus box.bmp")
                           :transparent-color-index 0))

(gp:register-image-translation
 'question-image
 #.(gp:read-external-image (current-pathname "images/16x16/Question.bmp")
                           :transparent-color-index 0))

(gp:register-image-translation
 'warning-image
 #.(gp:read-external-image (current-pathname "images/16x16/warning.bmp")
                           :transparent-color-index 0))

;; each of the items should have an indicator indicating if it was run, and the result of an automated(?) test which
;; can be overridden by the diagnostician.

;; we have set up some possible images for this and here we associate
;; the logical meaning with the image

(defparameter *not-run* 'warning-image) ; warning-image ; check-image

(defparameter *run* 'check-image)

(defparameter *run-unknown-result* 'question-image) ; question-image

(defparameter *run-good-result* 'ok-image) ;plus-image ;check-image

(defparameter *run-bad-result* 'cross-image) ;minus-image

(defparameter *numtests* 20
  "Maximum number of diagnostics we want to display at a time")

(defparameter *test-name-column-width* 250)

(defparameter *test-state-column-width* 65)

(defparameter *test-state-total-column-width* (* *test-state-column-width* 5))

(defparameter *diagnostics-height* 30)

(defparameter *diagnostics-total-height* (* *numtests* (1+ *diagnostics-height*)))

(defun clear-diagnostic-tests (interface)
  (setf (dw1-tests interface) nil)
  (setf (dw2-tests interface) nil)
  (setf (dw3-tests interface) nil)
  (setf (dw4-tests interface) nil))

(defun test-run-p-check-box (dt &optional (value nil valp))
  (if valp
      (setf (test-run-p dt) value)
      (test-run-p dt)))

(defun test-result-check-box (dt &optional (value nil valp))
  (let ((run-p (test-run-p-check-box dt)))
    (if run-p
        (if valp
            (setf (test-result dt) value)
            (if (slot-boundp dt 'fpga-diagnostics::result)
                (test-result dt)
                :unknown))
        :disabled))) ; don't allow it to be changed until run

(defun run-p-drawing-function (row column pane width height) 
  (declare (ignore width height)) ;for now
  (let ((run-p (test-run-p-check-box (cell-data-object row column))))
    (gp:draw-image pane 
                   (gp:load-image pane (if run-p *run* *not-run*)) 
                   10 10))) ; I set this by eye, it seems to work well with the 16x16 images

(defun result-drawing-function (row column pane width height) 
  (declare (ignore width height))
  (let ((result (test-result-check-box (cell-data-object row column))))
    (gp:draw-image pane 
                   (gp:load-image pane (cond
                                        ((eql result :disabled)
                                          *not-run*)
                                         ((eql result :unknown)
                                          *run-unknown-result*)
                                         ((null result)
                                          *run-bad-result*)
                                         (t
                                          *run-good-result*)))
                   10 10)))

(defun diagnostics-rows (tests focus-function)
  (mapcar #'(lambda (test)
              (make-instance 'grid-data-row
                             :data-object test
                             :focus-function focus-function
                             :size *diagnostics-height*))
          tests))

(defun create-diagnostics-grid (name tests &optional focus-function)
  (make-instance 
   'grid-widget
   :name :diagnostics
   :title name
   :font *diagnostics-grid-font*
   :column-sections
   (list
    ;; the first column section is the row header, displaying the name
    ;; of the test
    (make-instance 'grid-column-section
                   :name :test
                   :scrollable-p t
                   :size *test-name-column-width*
                   :bands
                   (list (make-instance 'grid-display-column
                                        :header-title "Test Name"
                                        :name :name
                                        :data-reader 'test-name
                                        :band-dragging t
                                        :proportional-p t
                                        :right-border-spec 1
                                        :size *test-name-column-width*)))
    ;; the second column is a button grid showing if the test has been
    ;; run yet and if so what the result was
    (make-instance 'grid-column-section
                   :name :state
                   :scrollable-p nil
                   :size *test-state-total-column-width*
                   :right-border-spec nil
                   :bands
                   (list
                    (make-instance 'grid-display-column
                                   :name :run-p
                                   :header-title "Run?"
                                   :data-reader 'test-run-p-check-box
                                   :data-writer 'test-run-p-check-box
                                   ;:check-box-function 'test-run-p-check-box
                                   :drawing-function 'run-p-drawing-function
                                   :print-function nil ; suppress printing as we will use the drawing function to show an icon
                                   :size *test-state-column-width*)
                    (make-instance 'grid-display-column
                                   :name :result
                                   :header-title "Result"
                                   :data-reader 'test-result-check-box
                                   :data-writer 'test-result-check-box
                                   ;:check-box-function 'test-result-check-box
                                   :drawing-function 'result-drawing-function
                                   :print-function nil ; ditto
                                   :size *test-state-column-width*)
                    (make-instance 'grid-display-column
                                   :name :total-run
                                   :header-title "Runs"
                                   :data-reader 'test-num-runs
                                   :size *test-state-column-width*)
                    (make-instance 'grid-display-column
                                   :name :total-successes
                                   :header-Title "Success"
                                   :data-reader 'test-num-successful
                                   :size *test-state-column-width*)
                    (make-instance 'grid-display-column
                                   :name :total-failures
                                   :header-title "Failures"
                                   :data-reader 'test-num-failed
                                   :size *test-state-column-width*))))
   :row-sections
   (list
    ;; column headers
    (make-instance 'grid-special-row
                   :name :header
                   :data-getter :other-band
                   :data-reader 'header-title
                   :band-dragging t
                   :size *diagnostics-height*
                   :bottom-border-spec 4) ; because why not
    ;; now the actual test rows.
    (make-instance 'grid-row-section
                   :name :body
                   :scrollable-p t
                   :size (* (min (length tests) *numtests*) (1+ *diagnostics-height*))
                   :resizable-p nil
                   :bands (diagnostics-rows tests focus-function)))))

(capi:define-interface diagnostic-panel ()
  ((dw1-name :initform "Machine Diagnostics 1"
             :initarg :dw1-name
             :reader dw1-name)
   (dw1-tests :initform nil
              :initarg :dw1-tests
              :accessor dw1-tests)
   (dw2-name :initform "Machine Diagnostics 2"
             :initarg :dw2-name
             :reader dw2-name)
   (dw2-tests :initform nil
              :initarg :dw2-tests
              :accessor dw2-tests)
   (dw3-name :initform "Machine Diagnostics 3"
             :initarg :dw3-name
             :reader dw3-name)
   (dw3-tests :initform nil
              :initarg :dw3-tests
              :accessor dw3-tests)
   (dw4-name :initform "Machine Diagnostics 4"
             :initarg :dw4-name
             :reader dw4-name)
   (dw4-tests :initform nil
              :initarg :dw4-tests
              :accessor dw4-tests)
   (diagnostics-widget1 :initarg :dw-widget1 
                        :accessor diagnostics-grid1)
   (diagnostics-widget2 :initarg :dw-widget2
                        :accessor diagnostics-grid2)
   (diagnostics-widget3 :initarg :dw-widget3
                        :accessor diagnostics-grid3)
   (diagnostics-widget4 :initarg :dw-widget4
                        :accessor diagnostics-grid4))
  
  (:panes
   (control                             
    capi:push-button-panel
    :items '(:all-fail :all-succeed :clear-metrics :redraw :run-suite-test)
    :print-function 'string-capitalize
    :callback-type :data-interface
    :selection-callback 'enable-button-selection-callback)
   ;; for testing
   #||(op 
    capi:collector-pane
    :initial-constraints '(:visible-min-width (character 25)))||#
   )
  (:layouts
   (column1
    capi:column-layout
    '(diagnostics-widget1 diagnostics-widget4))
   (row1
    capi:row-layout
    '(column1 diagnostics-widget2 diagnostics-widget3))
   (default-layout
    capi:column-layout
    '(row1 control #||op||#)))
  (:default-initargs
   :layout 'default-layout
   :title "Machine Diagnostics"))

;; programmatic manipulation

(defvar *diagnostics-interface* nil)

(defun find-test (test-name &optional test-list (interface *diagnostics-interface*))
  ;; we can be smarter about test-list

  (cond
    ((and interface
          (null test-list))
     (or (simple-find-test test-name (dw1-tests interface))
         (simple-find-test test-name (dw2-tests interface))
         (simple-find-test test-name (dw3-tests interface))
         (simple-find-test test-name (dw4-tests interface))))
    ((and (not (null test-list))
          (symbolp test-list)) ; should name a function
     (funcall test-list interface))
    (t
     (simple-find-test test-name test-list))))

(defun find-all-tests (test-feature &optional test-list (interface *diagnostics-interface*))
  "Find all the tests in the list (explicit or implicit) with the
feature. Features supported so far are: :run and :not-run"
  ;; again, be smart about test-list
  (cond
    ((and interface
          (null test-list))
     (append (find-all-tests test-feature (dw1-tests interface) nil)
             (find-all-tests test-feature (dw2-tests interface) nil)
             (find-all-tests test-feature (dw3-tests interface) nil)
             (find-all-tests test-feature (dw4-tests interface) nil)))
    ((consp test-list)
     (mapcan #'(lambda (test)
                 (when (case test-feature
                         (:run
                          (test-run-p test))
                         (:not-run
                          (not (test-run-p test))))
                   (list test)))
             test-list))
    ((and (not (null test-list))
          (symbolp test-list)) ; should name a function
     (find-all-tests test-feature (funcall test-list interface)))
    (t
     nil)))

(defun mark-test-run (test &optional (interface *diagnostics-interface*))
  (prog1 (setf (test-run-p test) t)
    (when (diagnostics-running-p)
      (redraw-diagnostics interface)))) ; overkill but good for now

(defun retract-test-run (test &optional (interface *diagnostics-interface*))
  (setf (test-run-p test) nil)
  (mark-test-unknown test interface)
  (when (and interface (diagnostics-running-p))
    (redraw-diagnostics interface))
  :not-run)

(defun mark-test-success (test &optional (interface *diagnostics-interface*))
  (when (test-run-p test)
    (prog1 (setf (test-result test) t) ; this should increment the success count
      (when (and interface (diagnostics-running-p))
        (redraw-diagnostics interface)))))

(defun mark-test-failure (test &optional (interface *diagnostics-interface*))
  (when (test-run-p test)
    (setf (test-result test) nil) ; this should increment the failure count
    (when (and interface (diagnostics-running-p))
      (redraw-diagnostics interface))
    :failure))

(defun mark-test-unknown (test &optional (interface *diagnostics-interface*))
  (when (slot-boundp test 'result)
    (slot-makunbound test 'result)
    (when (and interface (diagnostics-running-p))
      (redraw-diagnostics interface))
    :unknown))

(defun mark-all-tests-successful (interface)
  ;; mark all the tests that have run
  (mapc #'(lambda (x) (mark-test-success x interface))
        (find-all-tests :run)))

(defun mark-all-tests-failure (interface)
  ;; mark all the tests that have run but are not yet marked as
  ;; failure. this seems over dramatic, but if tests are incremental
  ;; then presumably only a few things are newly changed, and we won't
  ;; change things that are already marked successful.
  (mapc #'(lambda (x) (unless (and (slot-boundp x 'result)
                                   (eql (test-result x) t)) ; don't mark something we know was successful as a failure
                        (mark-test-failure x interface)))
        (find-all-tests :run)))

(defun redraw-diagnostics (interface)
  (execute-with-interface interface 'gp:invalidate-rectangle
                          (diagnostics-grid1 interface))
  (execute-with-interface interface 'gp:invalidate-rectangle
                          (diagnostics-grid2 interface))
  (execute-with-interface interface 'gp:invalidate-rectangle
                          (diagnostics-grid3 interface))
  (execute-with-interface interface 'gp:invalidate-rectangle
                          (diagnostics-grid4 interface)))

;; temporary: for testing interface
#||
(defun report-button-action (type data interface)
  (format (capi:collector-pane-stream (slot-value interface 'op))
          "~S ~a~%" data type))
||#

(defun enable-button-selection-callback (data interface)
  #||(report-button-action "selected" data interface)||#
  (case data
    (:all-fail
     (mark-all-tests-failure interface))
    (:all-succeed
     (mark-all-tests-successful interface))
    (:clear-metrics
     (clear-metrics)
     (redraw-diagnostics interface))
    ;; shouldn't need this once we have things working correctly, but it helps to check!
    (:redraw
     (redraw-diagnostics interface))
    (:run-suite-test
     ;; should happen automatically when we reach the halt address
     (let*-non-null ((test-group *test-suite*)
                     (result-tester (test-result-interpreter test-group)))
        (if (funcall result-tester)
            ;; sucessful test run!
            (mark-all-tests-successful interface)
            (mark-all-tests-failure interface))))
     ))
