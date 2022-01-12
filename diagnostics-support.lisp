(in-package :s79-console)

(scheme-79:scheme-79-version-reporter "Scheme Machine Diagnostics" 0 3 0
                                      "Time-stamp: <2022-01-11 15:13:17 gorbag>"
                                      "0.3 release!")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.8  12/ 3/21 update scheme-79-mcr-i -> "" (if from microlisp-int or fpga-pla-build-tools)

;; 0.1.7  11/ 4/21 split out subpanel update fns

;; 0.1.6  10/25/21 keep in mind that most of the diagnostics will run
;;                   only on the simulator and not the chip, so local
;;                   copies of the micro-pc can and should just be
;;                   integers.

;; 0.1.5  10/20/21 *micro-pc* is now a bit vector (marching toward hardware)

;; 0.1.4  10/15/21 read annotation fns into *ulang-pkg*

;; 0.1.3  10/13/21 use *last-conditional-result* to set success or fail
;;                     test for a predicate.

;; 0.1.2  10/12/21 add panel for predicates

;; 0.1.1   8/24/21 cond vs cl:cond, if vs cl:if

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.3 08/18/21 When collecting annotations now support multiple
;;                   lists in the string move defvar *test-suite* to
;;                   defs

;; 0.0.2 08/14/21 Mark both conditional branches as run when we run a
;;                   conditional. Later we should check which
;;                   particular branch was run to insure complete
;;                   coverage (TBD)

;; 0.0.1 08/13/21 Add focus-function to test suites to call (test);
;;                   update-diagnostics fn

;; 0.0.0 08/09/21 New

;; generally, markup of the tests is done via the functions in the
;; diagnostic-panel (since the variables are kept in the
;; interface). However, generalized functions here can help with
;; defining diagnostics (e.g., how to run the test or determine
;; success automatically).

;; here we define groups of tests so we can declare them to the panel.

;; there are three panels in the diagnostics panel. These are:
;;
;; 1) the nanocodes. We can get a list of the defined nanocodes from
;;    the *nanocontrol-symtab* 


;; 2) the microcodes. We can get a list of the defined microcodes from
;;    the microcodes-used function in the validator (after it runs of
;;    course) (see diagnostics-defs.lisp)


;; 3) finally the explict test suites in the tests directory.



;; ok when we start up (this file is loaded next to last) we can do a
;; simple population of the test suite objects (but not the associated
;; tests).
(eval-when (:load-toplevel)
  (create-test-suite-objects))

(defun test-focus-fn (row column on)
  "Grid tool supports a focus function - we use it to run tests"
  (when on
    (let* ((test (cell-data-object row column))
           (test-name (test-name test))
           (test-number (group-test-number test))
           (displayer (cell-displayer row column))
           ;;(interface (capi:element-interface (top-level-widget row)))
           )
      ;; make sure we clicked on the test name
      (when (eql (name displayer) :name)
        (with-dialog-results (result okp)
          (prompt-for-confirmation (format nil "Run test ~A?" test-name))
          (when (and okp result)
            (setq *test-suite* test)
            (test :n test-number)))))))

;; when we are ready to set up the diagnostic panel, call this function (e.g. from the button on the main console)

(defun start-diagnostics ()
  (let ((dw1-name "Microcode")
        (dw2-name "Nanocode")
        (dw3-name "Test Suites")
        (dw4-name "Predicates")
        ;; these get set up when the (test) fn is called (e.g. (test :n 0) which loads the mcr and supporting fns for test-0).
        (dw1-tests *microtest-objects*)
        (dw2-tests *nanotest-objects*)
        (dw3-tests *test-suite-objects*)
        (dw4-tests *predicate-test-objects*))
    (capi:display
     (setq *diagnostics-interface*
           (make-instance 'diagnostic-panel
                          :dw1-name dw1-name
                          :dw2-name dw2-name
                          :dw3-name dw3-name
                          :dw4-name dw4-name
                          :dw1-tests dw1-tests
                          :dw2-tests dw2-tests ;
                          :DW3-tests dw3-tests
                          :dw4-tests dw4-tests
                          :dw-widget1 (create-diagnostics-grid dw1-name dw1-tests)
                          :dw-widget4 (create-diagnostics-grid dw4-name dw4-tests)
                          :dw-widget2 (create-diagnostics-grid dw2-name dw2-tests) ;
                          :DW-widget3 (create-diagnostics-grid dw3-name dw3-tests 'test-focus-fn))))))

(defun update-diagnostics (&optional (interface *diagnostics-interface*) update-test-suite-objects-p)
  ;; we've updated the microcode and/or nanocode, so update the diagnostics available as well
  (create-nanotest-objects)
  (create-microtest-objects)
  (create-predicate-test-objects)

  ;(update-microtests)

  ;(update-nanotests)

  ;(update-predicates)

  (when update-test-suite-objects-p
    (update-test-suites interface)))

(defun update-microtests (&optional (interface *diagnostics-interface*))
  (setf (dw1-tests interface) *microtest-objects*)
  (setf (diagnostics-grid1 interface)
        (create-diagnostics-grid (dw1-name interface) *microtest-objects*))
  (redraw-diagnostics interface))

(defun update-nanotests (&optional (interface *diagnostics-interface*))
  (setf (dw2-tests interface) *nanotest-objects*)
  (setf (diagnostics-grid2 interface)
        (create-diagnostics-grid (dw2-name interface) *nanotest-objects*))
  (redraw-diagnostics interface))

(defun update-predicates (&optional (interface *diagnostics-interface*))
  (setf (dw4-tests interface) *predicate-test-objects*)  
  (setf (diagnostics-grid4 interface)
        (create-diagnostics-grid (dw4-name interface) *predicate-test-objects*))
  (redraw-diagnostics interface))

(defun update-test-suites (&optional (interface *diagnostics-interface*))
  (setf (dw3-tests interface) *test-suite-objects*)
  (setf (diagnostics-grid3 interface)
        (create-diagnostics-grid (dw3-name interface) *test-suite-objects* 'test-focus-fn))
  (redraw-diagnostics interface))

(defun diagnostics-running-p ()
  "Nil if not running, and the interface if it is"
  (when (not (null *diagnostics-interface*)) ; might want to check if it's actually running of course (TBD)
    *diagnostics-interface*))

;; metrics

;; every time we update the micro-pc we mark the nanocode that was
;; just executed (from the prior micro-pc) as well as the last micro-pc
;; (as having been done).
;;
;; if the opcode was conditional (e.g. sense-and-branch nanocode) we
;; may mark which branch was taken (to prove we exercised both
;; branches). We might even want to mark what the sense line was to
;; prove we exercised all the sense lines. The current code doesn't
;; yet do that. (TBD)
;;
;; Note that every micro instruction should be associated with a
;; particular nanocode (which may have multiple instructions)
;;
;; example: do-car is a nanocode we want to mark, but there are two
;; associated nanoinstructions we don't mark.

;; note this is just one use case for diagnostics (we check that the
;; opcodes and nanocodes have been exercised and are working). Other
;; use cases are certainly possible, but this is what
;; start-diagnostics, above, will expect; other versions could be
;; synthesized to test, e.g. microcode types, return points, etc.
;; (effectively "basic blocks" in the microcode for a particular
;;  machine)

;; NB, micro-pc, prior-micro-pc are integers here, as they are NOT run
;; on the chip but only on the simulator to collect metrics. On-chip
;; diagnostics would probably have to be done differently e.g. using
;; microcode, so we don't bog down the chip doing non-productive work!

(let ((prior-micro-pc 0))
  (defun reset-uinstruction-metrics ()
    (setq prior-micro-pc 0))
  
  (defun update-microinstruction-metrics (micro-pc)
    ;; micro-pc has changed, so record metrics for the prior-micro-pc
    (when (plusp prior-micro-pc)
      (let* ((microop-symbol (microop-symbol prior-micro-pc))
             (microop (elt *microcontrol-array* prior-micro-pc))
             (nanoop-pc (second microop))
             (nanoop-symbol (nanoop-symbol nanoop-pc)))

        (note "updating diagnostics metrics for PC: #o~o (~A)" prior-micro-pc (or microop-symbol ""))

        (assert (not (null nanoop-symbol)) (prior-micro-pc) "Could not dertermine the nanoop for the microop!")
        (let ((nanoop-test (simple-find-test nanoop-symbol *nanotest-objects*)))
          (cl:if nanoop-test
                 (setf (test-run-p nanoop-test) t)
                 (note "diagnostic metrics: No test for nanoop ~s" nanoop-symbol)))
        ;; some microops are noops or other fillers so we don't worry about those. Check if there is an
        ;; annotation associated with the pc

        ;; note that conditionals will have to be handled specially since it matters if we took the success or fail
        ;; branch! (TBD)

        (mapc #'(lambda (fn)
                  ;; does it name a predicate?
                  ;(note "updating fn: ~s" fn)
                  (cl:cond
                    ((upred-p fn)
                     ;(note "(a predicate)")
                     (cl:if (not (null *last-conditional-result*))
                            (let*-non-null ((test (simple-find-test
                                                   (string (generate-success-mt-name fn))
                                                   *predicate-test-objects*)))
                                           (setf (test-run-p test) t))
                            (let*-non-null ((test (simple-find-test
                                                   (string (generate-fail-mt-name fn))
                                                   *predicate-test-objects*)))
                                           (setf (test-run-p test) t))))
                    (t
                     (let*-non-null ((test (simple-find-test (string fn) *microtest-objects*)))
                                    (setf (test-run-p test) t)))))
              (collect-annotation-fns prior-micro-pc))))
    ; remember the current microinstruction for next time
    (setq prior-micro-pc micro-pc)
  ))

(defun collect-annotation-fns (pc)
  (let* ((annotation (get-uc-annotation pc))
         (more-p t)
         (start 0)
         current-annotation
         annotation-list
         end-posn
         (*package* *ulang-pkg*))
    ;; parse the annotation for microops and use
    ;; those to mark up annotations will start
    ;; with ";; " so we have to skip that.  Also
    ;; annotation-list may have more than one form
    ;; (8/18/21) as we include embedded functions,
    ;; so we have to collect them all.
    (when annotation
      ;; clean up the annotation
      (setq annotation (remove-if #'(lambda (x) (member x '(#\: #\; #\#))) annotation))
      (while more-p
        (msetq (current-annotation end-posn) (read-from-string annotation nil nil :start start))
        (cl:cond 
         (current-annotation
          (setq start end-posn)
          (cl:if (consp current-annotation) ; ignore symbols
            (push current-annotation annotation-list)))
         (t
          (setq more-p nil))))
      (mapcan #'collect-fns annotation-list))))

(defun collect-fns-i (list)
  (when (consp list)
    (cl:cond
     ((consp (car list))
      (append (collect-fns (car list)) (collect-fns-i (cdr list))))
     (t
      (collect-fns-i (cdr list))))))

(defun collect-fns-cond (cond-list) ;; conds have lists as arguments
  (mapcan #'collect-fns-i cond-list)) ; the first will be a list so collect-fns-i should do the right thing

(defun collect-fns (list)
  (cons (car list)
        (cl:if (eql (car list) 'microlisp:cond)
          (collect-fns-cond (cdr list))
          (collect-fns-i (cdr list)))))
