(in-package :microlisp-shared) ; instructions are (now) in :microlisp-shared package

(scheme-79:scheme-79-version-reporter "S79 ucode Defs" 0 3 1
                                      "Time-stamp: <2022-01-27 10:38:44 gorbag>"
                                      "micro-call now a macro so add decl")

;; 0.3.1   1/26/22 defined micro-call macro, so add appropriate declarations

;; xxxxx   1/19/22 remove some TBDs in the comments (they were done already)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.12  1/ 5/22 change package to microlisp-shared so instructions
;;                   get interned correctly
;;                 Add decls for &rplaca-and-unmark!,
;;                   &rplacd-and-mark-car-being-traced!, etc.  so they
;;                   get interned before we compile
;;                   storage-manager.lisp
;;                 Export symbols for predicates appropriately
;;                 Add decls for my "internal" defufns like
;;                   compare-registers

;; 0.1.11 12/14/21 change NIL numargs to T in declarations, both
;;                   effectively mean the same thing: don't
;;                   check. (Used for things that have a variable
;;                   number of legal arguments, like COND)

;; 0.1.10 12/13/21 make sure we update symbols in *ulang-pkg*

;; 0.1.9  12/ 6/21 .. 12/ 8/21 tables were simplified (now just three:
;;                  one for automatically generated from defufn, one
;;                  for normal microcode operators and one for macro
;;                  operators, i.e. those that when called generate
;;                  more microcode operators that have to be compiled.

;; 0.1.8  12/ 3/21 move some variable declarations to
;;                   support/pla-support/ulisp-defs update
;;                   scheme-79-mcr -> microlisp

;; 0.1.7  10/15/21 use *ulang-pkg*

;; 0.1.6  10/13/21 simple-branch for cond expansion

;; 0.1.5  10/ 8/21 new: *internal-ucode-operations-alist*

;; 0.1.4   9/27/21 microlisp:progn, if, cond

;; 0.1.3   9/ 6/21 move mark and umark opcodes with embedded compile to
;;                   the appropriate alist and annotate for the
;;                   validator to get the right instruction count

;; 0.1.2   8/25/21 update if declaration for 2-3 arguments

;; 0.1.1   8/24/21 update &cons declarations for rewrite

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.4  2/19/21 elaborate on internal functions to &cons

;; 0.0.3  2/17/21 use *scheme-mach*

;; 0.0.2  2/15/21 add *embedded-ucode-operations-alist* to deal with
;;                   ucode operations that expand into other ucode
;;                   operations (for analysis and compilation purposes)

;; 0.0.1  2/10/21 add save & restore as opcodes instead of macros (as
;;                   original chip had)

;; 0.0.0 12/ 8/20 moved these from validator

;; first a list of the instructions and some declarations about them
;; form is (<instruction-symbol> <numargs> <options>*)
;;
;;  where numargs is either an integer indicating the required number
;;  of arguments, a list containing two numbers, the low and high
;;  number of arguments or "t" if any number of arguments is ok (or we
;;  just don't want to check). This is done in ucode-validator.lisp:
;;  validate-numargs

;; may want these in an initialization? Vars are defined in
;; support/pla-suport/ulisp-defs

;; the majority of these were filled in by manual analysis of the code
;; in the TR. Eventually I want to move toward generation of these
;; tables (or their equivalent) through the defufn form which will
;; probably require some changes to that. (TBD)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq microlisp-int:*ulisp-operations-alist*
        `(,(microlisp-int:create-ulopd '&car 1)
          ,(microlisp-int:create-ulopd '&cdr 1)
          ,(microlisp-int:create-ulopd '&rplaca 2)
          ,(microlisp-int:create-ulopd '&rplacd 2)
          
          ,(microlisp-int:create-ulopd '&set-global-value 2) ; same as &rplaca per microcode.mcr notes
                                        ; (may depend on target)

          ,(microlisp-int:create-ulopd '&rplaca-and-mark! 2)
          ,(microlisp-int:create-ulopd '&rplaca-and-unmark! 2) ; my usage in storage-manager
          ,(microlisp-int:create-ulopd '&car-being-traced? 1)
          ,(microlisp-int:create-ulopd '&rplacd-and-mark-car-being-traced! 2) ; my usage in storage-manager
          ,(microlisp-int:create-ulopd '&rplacd-and-mark-car-trace-over! 2) ; my usage in storage-manager
          ,(microlisp-int:create-ulopd '&increment-scan-up 0)
          ,(microlisp-int:create-ulopd '&decrement-scan-down 0)
          ,(microlisp-int:create-ulopd '&clear-gc-needed 0)
          ,(microlisp-int:create-ulopd '&scan-up=scan-down? 0)
          ,(microlisp-int:create-ulopd '&scan-down=0? 0)
          ,(microlisp-int:create-ulopd '&in-use? 1)
          
          ,(microlisp-int:create-ulopd '&decrement-displacement 0)
          ,(microlisp-int:create-ulopd '&decrement-frame 0)

          ,(microlisp-int:create-ulopd '&pointer? 1)
          ,(microlisp-int:create-ulopd '&=type? 2)
          ,(microlisp-int:create-ulopd '&set-type 2 :register-name :type-name) ; note that type-name may
                                        ; include defreturn
                                        ; symbols, not just
                                        ; predefined, if the
                                        ; register is *stack*
                                        ; anyway

          ,(microlisp-int:create-ulopd '&frame=0? 0)
          ,(microlisp-int:create-ulopd '&displacement=0? 0)

          ,(microlisp-int:create-ulopd '&eq-val 1) ; specifically for the *val* register it appears
          ,(microlisp-int:create-ulopd '&val=0? 0)
          ,(microlisp-int:create-ulopd '&decrement-scan-down-to-val 0)
          ,(microlisp-int:create-ulopd '&increment-scan-up-to-val 0)
          ,(microlisp-int:create-ulopd '&val-displacement-to-exp-displacement 0)
          ,(microlisp-int:create-ulopd '&val-frame-to-exp-frame 0)

          ,(microlisp-int:create-ulopd '&get-interrupt-routine-pointer 0) ; reads pads
          ,(microlisp-int:create-ulopd '&read-from-pads 1)                ;reads pads
          ,(microlisp-int:create-ulopd '&write-to-pads 1)                 ; writes pads

          ;; these may not have been nanocode
          ;; operations on the original chip.
          ;; (was on *core-register-operations-alist*)
          ,(microlisp-int:create-ulopd 'assign 2 :register-name t)
          ,(microlisp-int:create-ulopd 'fetch 1 :register-name)

          ;; to be clear, we *could* reimplement these "ucode-operations" as
          ;; microcode if we don't want to imitate their nano-code which was
          ;; used to reduce reduncancy and shrink the die size - something we're
          ;; not currently worried about... and may possibly slow down the
          ;; implementation anyway (kind of a loop rolling)
          ;; (setq *ucode-operations-alist*
          ,(microlisp-int:create-ulopd 'go-to 1 :tag) ; we probably don't already know the tag name, so
                                        ; just call it anything
          ,(microlisp-int:create-ulopd 'dispatch-on-exp-allowing-interrupts 0)
          ,(microlisp-int:create-ulopd 'dispatch-on-stack 0)
          ,(microlisp-int:create-ulopd 'dispatch 1)
      
          ,(microlisp-int:create-ulopd 'eval-exp-popj-to 1 :tag)

          ,(microlisp-int:create-ulopd 'and 2 t)
          ,(microlisp-int:create-ulopd 'or 2 t)
          ,(microlisp-int:create-ulopd 'not 1 t)

          ;; instead of having these as macros they were clearly intended to
          ;; be nanocode that operate on the stack (note that 'restore' is
          ;; given as one of the nanocode examples in the AIM!)
          ,(microlisp-int:create-ulopd 'restore 1 :register-name)

          ,(microlisp-int:create-ulopd 'micro-return 0) 

          ;; special functions I defined
          ,(microlisp-int:create-ulopd 'tag 1 :tag)
          ,(microlisp-int:create-ulopd 'fetch-and-test-for-success 4 :register-name :sense-line :tag :tag)
          ,(microlisp-int:create-ulopd 'fetch-and-test-pred 4 :register-name :predicate-name :tag :tag)
          ,(microlisp-int:create-ulopd 'simple-branch 3 :sense-line :tag :tag)
          ,(microlisp-int:create-ulopd 'simple-branch-pred 3 :predicate-name :tag :tag)
          ,(microlisp-int:create-ulopd 'compare-registers 5 :predicate-name :register-name :register-name :tag :tag)
          ,(microlisp-int:create-ulopd 'compare-to-type-const 4 :register-name :tag :tag :tag)
          ,(microlisp-int:create-ulopd 'gc-needed! 0)
          ))

  (setq microlisp-int:*ulisp-macro-alist* ;; *embedded-ucode-operations-alist*
    ;; instead of having these as macros they were clearly intended to
    ;; be nanocode that operate on the stack (note that 'restore' is
    ;; given as one of the nanocode examples in the AIM!)
    ;; entries are (ucode-name number-args arg-type . embedded-ucode-list)
    `(,(microlisp-int:create-ulmd save 1 (t) &cons)     
      ,(microlisp-int:create-ulmd &cons 2 (t t) microlisp:progn &rplaca &rplacd &cdr assign assign assign microlisp:if &address=? gc-needed! fetch fetch fetch fetch fetch fetch fetch)
      ;; were on hardware operations alist, but moved here since they were implemented with embedded expressions
      ,(microlisp-int:create-ulmd &mark-car-being-traced! 1 (t) assign &cdr &rplacd-and-mark-car-being-traced! fetch fetch fetch)
      ,(microlisp-int:create-ulmd &mark-car-trace-over! 1 (t) assign &cdr &rplacd-and-mark-car-trace-over! fetch fetch fetch)
      ,(microlisp-int:create-ulmd &mark-in-use! 1 (t) assign &car &rplaca-and-mark! fetch fetch fetch)
      ,(microlisp-int:create-ulmd &unmark! 1 (t) assign &car &rplaca-and-unmark! fetch fetch fetch)

      ;; also see comments in microcode file
      ,(microlisp-int:create-ulmd micro-call 2 (:tag :tag) &set-type go-to)

      ;; these have compile-embedded-expression so the expansion ucode fns
      ;; are listed here to get the validation counts right

      ,(microlisp-int:create-ulmd &global-value 1 (t) &car) ; should be car of value cell of the symbol

      ;; actual content may vary no specific number of clauses (may
      ;; need to validate separately since each clause is not
      ;; formatted like a progn)
      ,(microlisp-int:create-ulmd microlisp:cond t (t . t) simple-branch microlisp:progn go-to go-to)

      ;;ditto has 2 or 3 clauses: expression, positive-arm, and
      ;; optional negative-arm note that it will generate a
      ;; conditional and we're just marking the branch instruction
      ;; here
      ,(microlisp-int:create-ulmd microlisp:if (2 3) (t) microlisp:progn simple-branch go-to go-to) 

      ,(microlisp-int:create-ulmd microlisp:progn t (t)) ; also no specific number of clauses
      ))
  
  (setq microlisp-int:*registers-whose-types-are-tags*
        '(*stack* *retpc-count-mark*)))

;; also make sure all the predicate symbols get defined in the shared package
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(&address=? &in-use? &not-in-use? &car-being-traced? &pointer?
            &type? &=type? &scan-up=scan-down? &scan-down=0?)))

;; make (doublely?)sure these symbols are exported for the microcode to use - 
(in-package :microlisp-int)

(eval-when (:load-toplevel :execute) ;; re-export symbols (should be in :microlisp, no?)
  (flet ((remove-scheme-79-mcr-symbols (l)
           (remove-if #'(lambda (x) (or (eq (symbol-package x) *ulang-pkg*)
                                        (eq (symbol-package x) *ulang-shared-pkg*)))
                      l)))
    (mapc #'export-ulisp-symbol (remove-scheme-79-mcr-symbols (mapcar #'car *ulisp-operations-alist*)))
    (mapc #'export-ulisp-symbol (remove-scheme-79-mcr-symbols (mapcar #'car *ulisp-macro-alist*)))
    ))


