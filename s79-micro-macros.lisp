(in-package :scheme-mach)

;; for now we put this into :scheme-mach but due to collision with CL we may want a more restricted package in the
;; future (so not all :scheme-mach packages have to reference "cl:cond" for instance)

(scheme-79:scheme-79-version-reporter "Scheme Microcode Macros" 0 3 0
                                      "Time-stamp: <2022-01-11 15:17:21 gorbag>"
                                      "0.3 release!")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.10  1/ 4/22 make generate-cond-test a method rather than a defun. This allows us to invoke
;;                   the generic function inside validation code defined under support!

;; 0.1.9  12/14/21 use defumac for macro defufns.

;; 0.1.8  12/ 3/21 update scheme-79-mcr-i -> "" (if from microlisp-int or fpga-pla-build-tools)
;;                   update scheme-79-mcr -> microlisp

;; 0.1.7 10/ 8/21 fix validation of cond, if

;; 0.1.6  9/27/21 scheme-79-mcr:progn, if, cond

;; 0.1.5 09/20/21 fix call to generate-cond-test in NOT processing

;; 0.1.4 09/18/21 look up inverted predicate for NOT

;; 0.1.3 09/14/21 add (COND): to upla-comment so it shows up on console

;; 0.1.2 08/30/21 use compare-to-type-const (a defufn) to make code generation consistant.

;; 0.1.1 added "if" 

;; 0.1.0 Moved to own file form sim-machine-internal.lisp

;; *special-ucode-operations-alist*
;; cond

(defun generate-cond-test-boolean (test-clause success-tag fail-tag)
  ;; not just a simple test, so we have to break down into multiple tests
  (case (car test-clause)
    (and
     (generate-cond-test-and (cdr test-clause) success-tag fail-tag))
    (or
     (generate-cond-test-or (cdr test-clause) success-tag fail-tag))
    (not
     ;; just invert tags (ok, that won't work because tags are in a special position)
     ;;(generate-cond-test (cadr test-clause) fail-tag success-tag)
     ;; instead we have to look up the inverse predicate 9/18/21 BWM
     (generate-cond-test `(,(invert-predicate (caadr test-clause)) ,(cadadr test-clause)) success-tag fail-tag))))

(defun generate-cond-test-and (and-clauses success-tag fail-tag)
  (let* ((current-expression (car and-clauses))
         (ce-success-tag (gensym "$BOOL-SUCCEED"))
         (ce-fail-tag (gensym "$BOOL-FAIL")))
      (cl:cond
        ((cdr and-clauses) ; will have more clauses
        `(,@(generate-cond-test current-expression ce-success-tag ce-fail-tag)
          (tag ,ce-fail-tag)
          (go-to ,fail-tag) ;done
          (tag ,ce-success-tag)
          ;;next AND test
          ,@(generate-cond-test-and (cdr and-clauses) success-tag fail-tag)))
        (t
         (generate-cond-test current-expression success-tag fail-tag)))))

(defun generate-cond-test-or (or-clauses success-tag fail-tag)
  (let* ((current-expression (car or-clauses))
         (ce-success-tag (gensym "$BOOL-SUCCEED"))
         (ce-fail-tag-1 (gensym "$BOOL-FAIL"))
         (ce-fail-tag-2 (gensym "$BOOL-FAIL2")))
      (cl:cond
        ((cdr or-clauses) ; will have more clauses
        `(,@(generate-cond-test current-expression ce-success-tag ce-fail-tag-1)
          (tag ,ce-fail-tag-1)
          (go-to ,ce-fail-tag-2) ; have to skip over success address
          (tag ,ce-success-tag)
          (go-to ,success-tag) ; done
          (tag ,ce-fail-tag-2)
          ;;next OR test
          ,@(generate-cond-test-or (cdr or-clauses) success-tag fail-tag))) 
        (t
         (generate-cond-test current-expression success-tag fail-tag)))))

;; make this a method so we can define the generic function in support (used by the validator) 1/4/22 BWM
(defmethod generate-cond-test (test-clause success-tag fail-tag)
  ;; return a LIST of microcode statements
  (cl:cond
    ((member (car test-clause) *boolean-terms*) ; complex?
     (generate-cond-test-boolean test-clause success-tag fail-tag))
    ((endp (cdr test-clause)) ; simple test
     `((simple-branch ,(car test-clause) ,fail-tag ,success-tag)))
    ((and (consp (cadr test-clause))
          (endp (cddr test-clause)) ; one argument
          (eql (caadr test-clause) 'fetch)) ; typical case
     `((fetch-and-test-for-success ,(cadadr test-clause) ,(car test-clause) ,fail-tag ,success-tag)))
    ((and (consp (cadr test-clause))
          (eql (caadr test-clause) 'fetch)
          (consp (caddr test-clause))
          (eql (caaddr test-clause) 'fetch) ; some kind of comparison between two registers
          (endp (cdddr test-clause))) ; two arguments
     `((compare-registers ,(car test-clause) ,(cadadr test-clause) ,(cadaddr test-clause) ,fail-tag ,success-tag)))
    ((eql (car test-clause) '&=type?) ; have to handle special for now, presuming next arg is a register then a constant
     `((compare-to-type-const ,(cadadr test-clause) ,(caddr test-clause) ,fail-tag ,success-tag)))
    (t
     (error "generate-cond-test: unhandled case ~s" test-clause))))

;; note that this code may generate some wasted GO-TO instructions and
;; tags (particularly if the COND will never "fall through"), but we
;; can clean that up in an intermediate pass before PASS-5 if needed
;; (do some basic block code analysis)
(defumac microlisp:cond (&rest cond-expressions :args-last t :constituent t)
  (let ((remaining-expressions cond-expressions)
        (proposed-code nil)
        (end-cond-tag (gensym "$COND-DONE")))
    ;; set up a list of tests and progns
    (while remaining-expressions
      (let* ((current-expression (pop remaining-expressions))
             (fail-tag (gensym "$COND-FAIL"))
             (success-tag (gensym "$COND-SUCCEED"))
             (end-clause-tag (gensym "$COND-NEXT-CLAUSE"))
             (test-clause (car current-expression))
             (progn-clause (cadr current-expression)) ; should have the progn inserted by the validator
             (proposed-clause (cl:if (eql test-clause 't)
                                `(,progn-clause
                                  (tag ,(cl:if remaining-expressions
                                          end-clause-tag ; strange construct, probably should warn
                                          end-cond-tag)))
                                `(,@(generate-cond-test test-clause success-tag fail-tag)
                                  (tag ,fail-tag)
                                  (go-to ,(cl:if remaining-expressions
                                            end-clause-tag
                                            end-cond-tag))
                                  (tag ,success-tag)
                                  ,progn-clause
                                  (go-to ,end-cond-tag)
                                  (tag ,(cl:if remaining-expressions
                                          end-clause-tag
                                          end-cond-tag))))))
        (setq proposed-code (append proposed-code proposed-clause))))
    ;; something for the log
    (when *upla-stream*
      (upla-write-comment "(COND):") ; so something shows up on the console
      (upla-write-comment "~s---" `(cond ,@cond-expressions)))

    (compile-embedded-expression `(microlisp:progn ,@proposed-code))))

;; if
(defumac microlisp:if (predicate-clause success-clause &optional failure-clause :args-last t :constituent t)
  (let ((fail-tag (gensym "$IF-FAIL"))
        (fail-clause-tag (gensym "$IF-FAIL-CLAUSE"))
        (success-tag (gensym "$IF-SUCCEED"))
        (done-tag (gensym "$END-IF"))
        (fail-clauses (cl:if failure-clause (list failure-clause)))) ; force list for splice

    (compile-embedded-expression 
     `(microlisp:progn 
        ,@(generate-cond-test predicate-clause success-tag fail-tag)
        (tag ,fail-tag)
        (go-to ,fail-clause-tag)
        (tag ,success-tag)
        ,success-clause
        (go-to ,done-tag)
        (tag ,fail-clause-tag)
        ,@fail-clauses ; have to splice in case it's empty
        (tag ,done-tag)))))

;; progn

(defumac microlisp:progn (&rest ucode-expressions :args-last t :constituent t) ; not really constituent but this will supress printing output
  ;; just collect the results of compiling the expressions
  (mapcan #'(lambda (expression)
              (let ((compiled-expression (compile-embedded-expression expression))) ; supress check
                (copy-list compiled-expression)))
          ucode-expressions)) ; do the copy to prevent munging constants

