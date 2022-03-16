(in-package :scheme-mach)

(scheme-79:scheme-79-version-reporter "S79 ucode support ops" 0 3 4
                                      "Time-stamp: <2022-03-15 12:45:31 gorbag>"
                                      "special-case for frame=0 and displacement=0 preds")

;; 0.3.4   3/11/22 frame=0 and displacement=0 check the fields in the
;;                    *bus* register but should load *exp* first
;;                    This should be somehow declared in the
;;                    upred definition for these, but for now we will
;;                    special case in simple-branch-pred (TBD)

;; 0.3.3   2/ 9/22 way too many things (fns, variables) with "line" in their name
;;                    and it's ambiguous.  Splitting so "line" refers to,
;;                    e.g. an output (log) line, "expression" refers to a
;;                    'line' of code (single expression in nano or microcode
;;                    land typically, and because we used (READ) it wasn't
;;                    confined to a single input line anyway) and "wire" to
;;                    refer to, e.g., a control or sense 'line' on a register.

;; 0.3.2   1/27/22 fix 0.3.1

;; 0.3.1   1/25/22 add :arg option for defupred which just puts the
;;                      argument or from register onto the bus (rather
;;                      than taking the car or cdr of what it points
;;                      to). Here we modify fetch-and-test-pred to
;;                      support that.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.16  1/ 7/22 formerly ucode-compiler.lisp now ucode-support-ops.lisp
;;                    as only the supporting defufn's remain in this file!
;;                 Move 'toplevel' compiler fns either to support hierarchy or
;;                    to compiler-defs (as interface to project-specific compiler)

;; 0.1.15  1/ 6/22 use opcode-fn instead of property, and other
;;                    property accessor fns

;; 0.1.14 12/ 2/21 start to move some fns and variables to
;;                    support/pla-support/

;; 0.1.13 11/24/21 use *note-output* instead of *error-output* for
;;                    notes

;; 0.1.12 10/15/21 use *ulang-pkg*

;; 0.1.11 10/13/21 use input file name for temp files so we can
;;                    distinguish multiple (different) test runs

;; 0.1.10  9/21/21 move version banner in upla to the front of the
;;                    file.

;; 0.1.9.  9/13/21 Supress internal code generation in compile-expression when
;;                   we are calling write-generated-code on an
;;                   application of a compiled-function (which might
;;                   itself have a comile-embedded expression) until
;;                   it returns with the code set

;; 0.1.8   9/ 7/21 use justified output for log message on validation
;;                   results to break up long lines; also used
;;                   *banner-length* for the length of those lines
;;                   (see support/common.lisp)

;; 0.1.7   9/ 2/21 simple-branch will use defupred fn when available

;; 0.1.6   9/ 1/21 realized (while patching it) had duplicated code from
;;                   predefs, now call sense-wire-real-name from there.

;; 0.1.5   8/31/21 write-generated-code no longer asserts if >1 expression is
;;                   to be written (though it does put a note in the log)

;; 0.1.4   8/30/21 compare-to-type-const

;; 0.1.3   8/24/21 compare-registers

;; 0.1.2   8/23/21 simple-branch

;; 0.1.1   8/22/21 differentiate microcode 'cond' from cl:cond

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.12   8-19-21 have compile-parameter pass a stream

;; 0.0.11   8-14-21 mark fetch-and-test as :conditional (for later diagnostics)

;; 0.0.10   6-26-21 Consolodate debug flags to scheme-79-defs

;; 0.0.9    3-11-21 delete remainders of assembler (had been commented out)

;; 0.0.8    3- 2-21 move function-being-compiled to compiler-defs, call finalize-microcontroller-array
;;                     (just a stub right now)

;; 0.0.7    2-20-21 eliminate 'backpatch' and 'post-optimization' as assembler takes care of it

;; 0.0.6    2-19-21 use open-temp-file instead of with-open-file on create-temp-file
;;                     intended to produce better behavior with abnoraml exits...

;; 0.0.5    2-17-21 start breaking out uPLA assembler and refactoring for
;;                     symbolic ouput form defufn
;;          2-16-21 some comments about implementing conditionals from
;;                     the AIM - will require an intermediate
;;                     representation to make it easier to create tags
;;                     for the assembler to use (even to have to have
;;                     an assembler at all, which was something I
;;                     thought would only be neede for the
;;                     nanocontroller!) But needed to deal with the
;;                     problem of maintaining even/odd
;;                     microinstructions as the control result of
;;                     conditionals is to set the low order bit of the
;;                     ucontroller pc!

;; 0.0.4    2-15-21 compiler has to run through types as well as tags
;;                     (all have code objects) also stub in
;;                     backpatching so we can convert symbolic tags to
;;                     offsets into the *microcontrol-array*

;; 0.0.3    2-13-21 stub in post-optimization to get rid of superfluous
;;                     go-tos (at least) still need to backpatch tags
;;                     into array offsets too, might want to do both
;;                     at same time

;; 0.0.2    2- 8-21 set up a set of special variables to pass context
;;                     information to defufn defined code generators

;; 0.0.1    2- 5-21  start to flesh out design, begin implementing (compile-microcode <filename>)

;;          1-20-21 No actual update - just some comment fixes based
;;                     on machine-nano.lisp's implementation

;; 0.0.0    1-14-21  New

;; special functions used for expansion of the defufn's into micro-pla assembler

(defufn fetch-and-test-for-success (from-register sense-wire fail-tag success-tag :declarations '(:conditional))
  ;; check the sense-wire - some require us to fetch the pointer onto the bus
  (cl:cond
    ((upred-p sense-wire)
     (fetch-and-test-pred from-register sense-wire fail-tag success-tag))
    (t
     `(((from ,from-register) (branch ,sense-wire ,fail-tag ,success-tag))))))
  
;; predicate version of the above
(defufn fetch-and-test-pred (from-register pred fail-tag success-tag :declarations '(:conditional))
  (assert (upred-p pred) (pred) "~s was not a declared predicate" pred)
  (mlet (sense-wire pred-type implied-register) (upred-desc pred)
     (let ((real-sense-wire (sense-wire-real-name sense-wire (or implied-register from-register))))
       (cl:cond
         ((null pred-type)
          (fetch-and-test-for-success (or from-register implied-register) real-sense-wire fail-tag success-tag))
         (t
          (ecase pred-type
            (:arg
             `(((from ,from-register) ;; get it onto the bus
                (branch ,real-sense-wire ,fail-tag ,success-tag))))
            (:car
             `(((from ,from-register) do-car)
               ((branch ,real-sense-wire ,fail-tag ,success-tag))))
            (:cdr
             `(((from ,from-register) do-cdr)
               ((branch ,real-sense-wire ,fail-tag ,success-tag))))))))))

(defufn simple-branch (sense-wire fail-tag success-tag :declarations '(:conditional))
  (cl:cond
    ((upred-p sense-wire)
     (simple-branch-pred sense-wire fail-tag success-tag))
    (t
     ;; presumably nothing needs to be loaded(?)
     (cl:if *debug-compiler*
            (break "simple-branch: branch instruction ~s does not put anything on bus (check)" sense-wire)
            (warn "simple-branch: branch instruction ~s does not put anything on bus (check)" sense-wire))
     (simple-branch-noload sense-wire fail-tag success-tag))))

(defufn simple-branch-noload (sense-wire fail-tag success-tag :declarations '(:conditional))
  `(((branch ,sense-wire ,fail-tag ,success-tag))))

(defufn simple-branch-pred (pred fail-tag success-tag  :declarations '(:conditional))
  (assert (upred-p pred) (pred) "~s was not a declared predicate" pred)
  (mlet (sense-wire pred-type implied-register defining-fn-symbol) (upred-desc pred)
    (let ((real-sense-wire (sense-wire-real-name sense-wire implied-register)))
      (cl:cond
        ((not (null defining-fn-symbol)) ; if we declared a defining function
         (funcall defining-fn-symbol fail-tag success-tag)) ; use it
        ((and (null pred-type) implied-register)
         (if (member real-sense-wire '(displacement=0-bus frame=0-bus)) ; hack! Should have something in the upred for this (TBD)
             (fetch-and-test-for-success '*exp* real-sense-wire fail-tag success-tag)
             (fetch-and-test-for-success implied-register real-sense-wire fail-tag success-tag)))
        ((null pred-type) ; does not need to look at memory, only register content
         (simple-branch real-sense-wire fail-tag success-tag))
        (t
         (error "simple-branch-pred: unhandled case ~s" pred))))))

(defufn compare-registers (pred register-1 register-2 fail-tag success-tag :declarations '(:conditional))
  (assert (upred-p pred) (pred) "~s was not a declared predicate" pred)
  (mlet (sense-wire pred-type implied-register) (upred-desc pred)
     (declare (ignorable implied-register)) ; don't need it yet
     (cl:cond
       ;; ok, the one case I'm currently using in &cons - can add more later ;-)
       ((and (null pred-type)
             ;; we have to figure out based on the parameters (probably should have a lookup table, no? TBD)
             (eql sense-wire :calculated) 
             (eql register-1 '*newcell*)
             (eql pred '&address=?))
        (fetch-and-test-for-success register-2 'address=bus-newcell fail-tag success-tag))
       (t
        (error "compare-register: unhandled case: pred: ~s r1: ~s r2: ~s" pred register-1 register-2)))))

(defufn compare-to-type-const (register constant fail-tag success-tag :declarations '(:conditional))
  "Handle the case of comparison between a register and a type constant (typically &type=?)"
  (let ((real-register (translate-alias register)))
    (assert (eql real-register '*val*) (register) "Currently only handle &type=? comparisons to the *val* register")
    ;; load the constant value (second argument) onto the bus and then do the comparison
    `(  ;; val has a type comparison
      ((from-type-const ,(or (non-pointer-type-name->int constant)
                             (pointer-type-name->int constant)))
       (branch-type type=bus-val ,fail-tag ,success-tag)))))
       

