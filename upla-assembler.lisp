(in-package :scheme-79-mcr-i)

(scheme-79:scheme-79-version-reporter "S79 upla assembler" 0 3 1
                                      "Time-stamp: <2022-02-09 12:44:55 gorbag>"
                                      "line disambiguation")

;; 0.3.1   2/ 9/22 way too many things (fns, variables) with "line" in their name
;;                    and it's ambiguous.  Splitting so "line" refers to,
;;                    e.g. an output (log) line, "expression" refers to a
;;                    'line' of code (single expression in nano or microcode
;;                    land typically, and because we used (READ) it wasn't
;;                    confined to a single input line anyway) and "wire" to
;;                    refer to, e.g., a control or sense 'line' on a register.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.17  1/10/22 more migration to support/pla-support/assembler-core-defs.lisp

;; 0.1.16  1/ 6/22 use property accessor fns

;; 0.1.15 12/15/21 use :microlisp-shared pkg

;; 0.1.14 10/20/21 after pass1 (when we assign locations) check that
;;                     *micro-pc-max-address* allows us to address 
;;                     all of the microcontroller-array.

;; 0.1.13 10/15/21 use *ulang-pkg*

;; 0.1.12 10/13/21 use input file name for temp files so we can
;;                   distinguish multiple (different) test runs

;; 0.1.11 10/ 6/21 when using microop-symbol for a pc, prefer
;;                    returning a well-defined tag (e.g. from defpc or
;;                    deftype) rather than a compiler-generated tag
;;                    (such as $COND-DONE1234) for clarity in the
;;                    front panel presentation

;; 0.1.10 10/ 1/21 add cases for from-const parallelingh from-type-const

;; 0.1.9   9/24/21 detect branch-type as a branch to force evenness of fail tag.
;;                   also add special decoder to disassembler

;; 0.1.8   9/21/21 add short version banner to pass1-stream

;; 0.1.7   9/18/21 break out assemble-code-expression from pass5 to make
;;                   debugging easier

;; 0.1.6   9/ 7/21 enhance disassembler to regenerate the type constant
;;                    if the nanoinstruction expects that.
;;                 use *from-to-nano-operations* to trigger decoding of
;;                    TO field using FROM anaphors

;; 0.1.5   9/ 4/21 track nanoops used in pass5

;; 0.1.4   9/ 3/21 microcontrol-symbol-value
;;                 set jump-table to jump to itself if we have no code
;;                   (to trap invalid usage in a way we can detect from
;;                   the console)

;; 0.1.3   9/ 2/21 parse-pla-expr detect branch-type usage

;; 0.1.2   8/31/21 use generate-ucode-with-constant when appropriate

;; 0.1.1   8/22/21 differentiate microcode 'cond' from cl:cond, if from cl:if

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.13   8/18/21 drop trailing ":" on annotation to support
;;                    multi-lists put it back to distinguish code
;;                    annotation from other kinds (so we know what we
;;                    can safely ignore when differentiating between
;;                    general log messages and those that are ucode)

;; 0.0.12   8/17/21 use note-if

;; 0.0.11   8/ 9/21 fix test for determining if expression is original
;;                      microcode for annotation

;; 0.0.10   8/ 7/21 Split out and export nanoop-symbol microop-symbol

;; 0.0.9    6/26/21 Consolodate debug flags to scheme-79-defs

;; 0.0.8    3/15/21 show nanocontrol-symtab sorted by address as well

;; 0.0.7    3/14/21 make sure we start assembly into addresses after 
;;                    the initial jump table!

;; 0.0.6    3/11/21 create jump table at start of microcontrol-array for
;;                    dispatch on type (or interrupt?)

;; 0.0.5    3/ 7/21 *-code-to-anaphor

;; 0.0.4    3/ 5/21 sort symtabs prior to printing them add microcode
;;                    expression disassembler we can use on console. For now
;;                    annotate pass5 diagnostics to check validity of
;;                    encoding

;; 0.0.3    3/ 2/21 move most debug messages from the screen to the log
;;                    files so they are separated by pass and document
;;                    that particular run

;; 0.0.2    2/27/21 translate aliased registers

;; 0.0.1    2/24/21 rewind pass1-stream before we process pass5
;;                    go-to should turn into a no-op nanocode (with a
;;                    destination address) if it hasn't been optimized
;;                    away

;; 0.0.0    2/17/21 OK, realized we needed to have a micro pla
;;                    (programmable logic array) assembler rather than
;;                    jumping directly to the microcontrol binary
;;                    representation to keep all the tags straight and
;;                    make it easier to debug particularly when
;;                    conditionals are involved (which have even/odd
;;                    address requirements)

;;                    See also ucode-compiler.lisp

;;                    The upshot is I have to reimplement the code
;;                    generated by defufun's I've already written
;;                    (AGAIN ;-) to generate this symbolic
;;                    verison. But it should be pretty easy for the
;;                    stuff I've already coded because other than
;;                    &CONS (where this emergent complexity was
;;                    discovered :-) expressions of microLisp pretty much
;;                    translated directly into a single nanocode
;;                    invocation.
;;                    

;; note in the AIM the lisp code is translated first from micro-lisp
;; into a micro PLA specification that looks vaguely similar to the
;; nanocode, e.g.

;; 
;;  *args* = *leader*; *display* = *node-pointer*
;; (defpc mark-node
;;  (assign *leader* (&car (fetch *node-pointer*)))
;;  (cond ((and (&pointer? (fetch *leader*))

;;
;; MARK-NODE
;; ((FROM *DISPLAY*) (TO *ARGS*) DO-CAR)
;; ((FROM *ARGS*) (BRANCH TYPE=POINTER-BUS MARK-NODE-3 MARK-NODE-1))
;;
;; [??: MARK-NODE] -> GO-TO 250, DO-CAR, FROM 7 TO 15 (250 200 007 15)
;; [250] -> GOTO 44, BRANCH, FROM 6 (044 307 006 00)
;;
;; [44: MARK-NODE-3] 
;; [45: MARK-NODE-1]

;; so given the above, here's a precis on the symbolic micro-PLA I'm
;; writing (this is the only reference to micro-pla, so I get to make
;; up as I go along)
;;
;; as before we load up the defufn macros with generators for this
;; symbolic version (rather than directly to the binary)
;;
;; each instruction is a list of the form
;;    ([(FROM <register>)] [(TO <register>)] <nanocode-op>)
;; or
;;    ((FROM <register>) (BRANCH <condition> <fail-tag> <success-tag>)
;; or
;;    ((<assembler-op> [<parameter>]))
;;
;; where 
;;    <register> is the usual *args* etc. names
;;
;;    <condition> is composed of the condition-name (from the defreg)
;;                and the register name (absent the '*'s) e.g.
;;                *newcell* has an address=bus sense wire, so it would
;;                be named address=bus-newcell while the bus itself
;;                has address=0 so it's named address=0-bus

;;
;; (<assembler-op> <parameter>) is one of the following 
;;     GO-TO <tag> which will generate the appropriate
;;                 next-instruction field or a no-op with a next
;;                 instruction of <tag> [note that <tag> need not be
;;                 an 'interned' symbol]

;; OK this is a simple multi-pass assembler:

;; pass 1: we will assign "instruction numbers" to the source file as
;;         well as any tags in the source

;; pass 2 (optional): if we are doing (basic) block elimination, we
;;                    look for duplicate blocks of code (q.v.) and
;;                    replace all but one with a jump to that one.
;;                    The basic idea is a block starts with an
;;                    instruction (which may or may not have a tag)
;;                    and is followed by one or more instructions
;;                    ending with some kind of transfer of control
;;                    (e.g.  jump, conditional, etc.) If two such
;;                    'basic blocks' are identical, including the tag
;;                    to which control is transfered, then they are
;;                    duplicate blocks. Note that we can either keep
;;                    running through the program identifying blocks
;;                    and eliminating them until we have a pass where
;;                    no more are eliminated (naive, simple, slow), or
;;                    we can try to eliminate likely duplicates first
;;                    by working backward from the transfer of control
;;                    points (e.g. if we see <tag-a> (go-to done) and
;;                    <tag-b> (go-to done) we should eliminate one of
;;                    the tags, e.g. tag-b, by changing any references
;;                    to tag-a. We leave <tag-b> (go-to done) in place
;;                    until the next pass where we can eliminate
;;                    unreachable code.-

;; pass 3 (optional): if we are doing unreachable code elimination we
;;                    run through and find tags that are both not
;;                    referenced AND which are not "fallen through" by
;;                    the preceeding code, e.g. <tag1> (goto b) <tag2>
;;                    (goto c) where there are no references to <tag2>
;;                    (perhaps it had originally been the success arc
;;                    of a condition but was optimized away in pass
;;                    2), then we can eliminate <tag2> (goto c). Note
;;                    we have to preserve tags and types referenced by
;;                    the user in **pointer-types**, and
;;                    **non-pointer-types** as these will be
;;                    effectively generated indirectly the
;;                    dispatch-on- u-instructions. We also have to
;;                    make sure we don't screw up how branches work
;;                    (the fail/ success results must be to the
;;                    following two words with low bit 0 meaning fail,
;;                    and 1 meaning success)

;; pass 4 (required if pass 2 and/or pass 3 are run): revisit pass 1
;;                    renumbering the lines

;; pass 5:            transform from symbolic to numeric representation 
;;                    (machine ucode) filling in *microcontrol-array*

;; and that's all folks!

;; here we define project-specific methods for each pass

(defmethod run-assembler-pass1 (input-stream output-stream)
  (note-if *debug-assembler* "assembler micro-PLA pass one output in ~s" (pathname output-stream))
  (announcement-banner (format nil "upla-assembler pass1 run ~a" (date-string)) output-stream)
  (announce-scheme-79-version output-stream t)

  ;; add line numbers (which will be offsets into the
  ;; *microcontrol-array*) and fill out *microcontrol-symtab*
  (let (line
        instruction
        (microcode-instruction "")
        (fail-tags nil)
        (success-tags nil)
        (*print-escape* nil) ; make sure we can write #\newline
        (*read-base* 10) 
        (*package* *ulang-pkg*)
        (counter (fill-pointer *microcontrol-array*))) ; start after the initial table!
    (while (setq line (read-line input-stream nil)) ; could use READ here but want to preserve comments for debugging
      (setq instruction (read-from-string line nil)) ; only have one instruction per line so can ignore end char     
      (cl:cond
        ((null instruction) ; whitespace (but could be the commented out original microcode, check
         (write line :stream output-stream)
         (terpri output-stream)
         (when (and (plusp (length line))
                    (eql (elt line (1- (length line))) #\:)) ; may be the original microcode
           (setq microcode-instruction (concatenate 'string microcode-instruction line))))
        ((and (symbolp instruction)
              (member instruction fail-tags)
              (oddp counter)) ; marked as the fail tag from a branch, but on wrong address
         (format output-stream ";; fix address boundary~%@~d (no-op)~%" counter) ; get onto right boundary
         (format output-stream "@~d ~a~%" (incf counter) line) ; fail tag
         (update-alist instruction counter *microcontrol-symtab*)) ; next will be fail instruction
        ((symbolp instruction) ;tag
         ;; validate
         (assert (cl:cond ((member instruction fail-tags)
                           (evenp counter))
                          ((member instruction success-tags)
                           (oddp counter))
                          (t
                           t))
                 ()
                 "Assembler Pass-1: branch tags out of alignment (assembler error)")
         ;; assign it a position
         (format output-stream "@~d ~a~%" counter line) ; don't increment the counter - next instruction at this posn
         (update-alist instruction counter *microcontrol-symtab*)) 
        (t
         (format output-stream "@~d ~a~%" counter line)
         ;; if we have symbolic information for this line, add it to the the annotations
         (when (plusp (length microcode-instruction))
           (update-alist counter microcode-instruction *microcontrol-annotations*)
           (setq microcode-instruction ""))
         (let ((branch-info (some #'(lambda (entry)
                                      (when (and (consp entry)
                                                 (member (car entry) '(branch branch-type)))
                                        entry))
                                  instruction)))
           (when branch-info ; is the instruction a branch?
             ;; at this point we only generate forward references to success/fail tags so this should be enough
             (push (caddr branch-info) fail-tags)
             (push (cadddr branch-info) success-tags)))
         (incf counter))))
    (when *debug-assembler*
      (print-semi-line output-stream)
      (format output-stream ";;Pass1 diagnostics:~%")
      (format output-stream ";; Microcode Lines: ~D~%" counter)
      (let ((tab-sizes (setup-tabulated-output
                        3
                        (list '(";;")
                              (mapcar #'car *microcontrol-symtab*)
                              (list (format nil "~d" counter))))))
        (dolist (entry (sort *microcontrol-symtab* #'string-lessp :key #'car))
          (print-tab-line output-stream tab-sizes ";;" (car entry) (format nil "~d" (cdr entry)))
          (terpri output-stream)))
      (print-semi-line output-stream))
    ;; even if not debugging...
    (assert (< counter *micro-pc-max-address*) ()
            "Microcontrol is too large to be addressed by *micro-pc*. Increase *micro-pc-size* in machine-defs.lisp and recompile simulator.")

    ))

(defun parse-pla-expr (expr)
  (let ((to nil)
        (from nil)
        (instruction nil)
        (next nil)
        (other nil))
    (mapc #'(lambda (x)
              (cl:cond
                ((not (consp x))
                 ;; presumably an instruction
                 (setq instruction x))
                (t
                 (case (car x)
                   (to
                    (setq to (append (cdr x) to)))
                   (from
                    (setq from (append (cdr x) from)))
                   (go-to ; probably want something driven from the operations-alist but for now...
                    (setq instruction 'microlisp-shared:no-op)
                    (setq next (cadr x)))
                   ((branch branch-type)
                    (push x other)
                    ;; note we really don't need the success branch
                    ;; because it's always fail with low order bit
                    ;; high
                    (setq next (caddr x)))
                   ((from-type-const from-const)
                    (push x other))
                   (t 
                    (push (cadr x) other))))))
          expr)
    (values to from instruction next  other)))

(defvar *debug-stream* t)

(defvar *nanoops-used-alist* nil
  "track these for metric purposes")

(defun assemble-code-expression (entry array-expression)
  (mlet (to from instruction next other) (parse-pla-expr entry)
     ;; reformat as microcode: (<next-state> <nanocode-entry>
     ;; <from-register> <to-register>)
                
     ;; note that the instruction appears only if it has no
     ;; associated arguments, otherwise will be part of a list
     ;; on other (e.g. go-to)
     ;; by default the next instruction is just after the current one
              
     ;; fix reference in next if it's symbolic
     (when (consp from)
       (assert (endp (cdr from)) (from) "invalid set of from registers in ~s" entry)
       (setq from (translate-alias (car from))))
            
     (cl:cond
       (next
        (setq next (microcontrol-symbol-value next)))
       (t
        (setq next (1+ array-expression))))
            
     (let (proposed-code)
       (cond-binding-predicate-to foo
                                  ((and (assoc 'branch other) (assoc 'from-const other))
                                   (setq proposed-code (generate-conditional-ucode-with-constant
                                                        (cadr foo)
                                                        (cadr (assoc 'from-const other))
                                                        next
                                                        'branch)))
                                  
                                  ((assoc 'branch other) ; have to handle conditionals special
                                   (setq proposed-code (generate-conditional-ucode 
                                                        (cadr foo)
                                                        next
                                                        from
                                                        'branch)))
                                  
                                  ((and (assoc 'branch-type other) (assoc 'from-type-const other))
                                   (setq proposed-code (generate-conditional-ucode-with-constant 
                                                        (cadr (assoc 'branch-type other))
                                                        (cadr (assoc 'from-type-const other))
                                                        next
                                                        'branch-type)))
                                  
                                  ((assoc 'branch-type other)
                                   (setq proposed-code (generate-conditional-ucode 
                                                        (cadr (assoc 'branch-type other))
                                                        next
                                                        from
                                                        'branch-type)))
                                  
                                  ((assoc 'from-type-const other)
                                   (setq proposed-code (generate-ucode-with-constant 
                                                        instruction
                                                        (cadr foo)
                                                        to
                                                        next)))
                                  
                                  (t
                                   (setq proposed-code (generate-ucode 
                                                        instruction
                                                        from
                                                        to
                                                        next))))
       (format *debug-stream* ";; ~s~%" proposed-code)
       ;; add to array
       (vector-push-extend proposed-code *microcontrol-array*)
       ;; also for debugging
       (mlet (symbolic symbolic-from symbolic-to symbolic-nanoop symbolic-next-tag symbolic-sense-wire)
               (disassemble-microcode proposed-code)
          (declare (ignore symbolic-from symbolic-to symbolic-next-tag symbolic-sense-wire))
                       
          (format *debug-stream* ";; symbolic: ~s~%~%" symbolic)
          ;; keep track of the nanoops actually used
          (update-alist symbolic-nanoop
                        (1+ (or (cdr (assoc symbolic-nanoop *nanoops-used-alist*))
                                0))
                        *nanoops-used-alist*)))))

(defmethod run-assembler-pass5 (input-stream output-stream)
  "Convert alpha-symbolic to numeric representation"

  (announcement-banner (format nil "upla-assembler pass5 run ~a" (date-string)) output-stream)
  (announce-scheme-79-version output-stream t)

  (when *debug-assembler*
    (note "assembler micro-PLA pass five output in ~s" (pathname output-stream))
    (dump-nanocontrol-symtab output-stream))

  ;; we should have already populated *microcontrol-symtab* so we can ignore tags

  (let (entry
        array-expression
        (*nanoops-used-alist* nil)
        (*read-base* 10)
        (*package* *ulang-pkg*) ;make sure we read into the right package
        (*debug-stream* output-stream)) ;used by assemble-code-expression
    
    ;; prepopulate nanoop-usage
    (mapc #'(lambda (entry)
              (let ((nanoop-name (car entry)))
                (update-alist nanoop-name 0 *nanoops-used-alist*)))
          *nanocontrol-symtab*)
    
    (while (setq entry (read input-stream nil)) ; parse into symbols, ignores whitespace and comments
       (format output-stream ";; ~s ->~%" entry) ; just for debugging
       (cl:cond
         ((and (symbolp entry) (eql (elt (string entry) 0) #\@))
          (setq array-expression (parse-integer (subseq (string entry) 1))))
         ((symbolp entry) ; tag
          nil) ; ignore
         (t ; should be a cons
          (assemble-code-expression entry array-expression))))
    ;; report nanoops actually used
    (dump-nanoop-usage *nanoops-used-alist* output-stream)))

(defun find-likely-microcode-tag (next &optional (adder 0) &key error-p)
  (cond-binding-predicate-to foo
    ((zerop next) ; special case
     (values 'microlisp-shared:no-op adder)) ; treat as a no-op
    ((microop-symbol next) ; found in symtab
     (assert (or (zerop adder) (not error-p)) (next) "Invalid next - not a tag")
     (values foo adder))
    (t
     (find-likely-microcode-tag (1- next) (1+ adder))))) ; keep scanning backward until we find a tag

(defun generate-tag-symbol (next-tag adder)
  "If there is an adder, modify the tag to show it"
  (cl:if (zerop adder)
      next-tag
      (intern (format nil "~A+~D" next-tag adder))))

(defun disassemble-microcode (expression)
  "Given a expression of microcode, return the equivalent symbolic version"
  (declare (special *from-to-nano-operations*))
  
  (destructuring-bind
      (next nanoop from to)
      expression ; for the moment, presume that expression is a quadruple
    (let ((nanoop-symbol (nanoop-symbol nanoop))
          (from-symbol (from-code-to-anaphor from))
          (to-symbol (to-code-to-anaphor to)))
      (mlet (next-tag adder) (find-likely-microcode-tag next)
        ;; at this point we're not disassembling into the source expressions,
        ;; but more the symbolic version of the object microcode.
        (cl:cond
          ((eql nanoop-symbol 'sense-and-branch)
           (assert (zerop adder) () "invalid next tag on branch: ~s" expression)
           ;; treat special
           (values `(,@(cl:if from-symbol
                              `((from ,from-symbol)))
                     (branch ,(sense-wire-encoding-to-symbol to)
                             ,next-tag
                             ,(find-likely-microcode-tag (1+ next) 0 :error-p t)))
                   from-symbol
                   nil
                   'branch
                   next-tag
                   (sense-wire-encoding-to-symbol to)))
          ((eql nanoop-symbol 'branch-type)
           (assert (zerop adder) () "invalid next tag on branch-type: ~s" expression)
           (values `((from-type-const ,from)
                     (branch-type ,(sense-wire-encoding-to-symbol to)
                                  ,next-tag
                                  ,(find-likely-microcode-tag (1+ next) 0 :error-p t)))
                   from-symbol
                   nil
                   'branch-type
                   next-tag
                   (sense-wire-encoding-to-symbol to)))
          ((member nanoop-symbol
                   '(do-set-typec-exp
                     do-set-typec-newcell
                     do-set-typec-val
                     do-set-typec-retpc-count-mark
                     do-set-typec-stack))
           (values `((from-type-const ,(or (int->non-pointer-type-name from)
                                           (int->pointer-type-name from)))
                     ,@(cl:if to-symbol
                              `((to ,to-symbol)))
                     (,nanoop-symbol)
                     (go-to ,(generate-tag-symbol next-tag adder))) ; kind of a fake go-to but not in original assembly...
                   from-symbol
                   to-symbol
                   nanoop-symbol
                   (generate-tag-symbol next-tag adder)))
          ((member nanoop-symbol *from-to-nano-operations*)
            ;  these are all the nanoinstructions that used from-to*
           (values `(,@(cl:if from-symbol
                              `((from ,from-symbol)))
                     ,@(cl:if to
                              `((to ,(from-code-to-anaphor to)))) ; treat as another from
                     (,nanoop-symbol)
                     (go-to ,(generate-tag-symbol next-tag adder)))))
          (t
           (values `(,@(cl:if from-symbol
                              `((from ,from-symbol)))
                     ,@(cl:if to-symbol
                              `((to ,to-symbol)))
                     (,nanoop-symbol)
                     (go-to ,(generate-tag-symbol next-tag adder))) ; kind of a fake go-to but not in original assembly...
                   from-symbol
                   to-symbol
                   nanoop-symbol
                   (generate-tag-symbol next-tag adder)))))))) 

(defun disassemble-microcontrol-array ()
  (let ((*package* *ulang-pkg*)) ; so it prints pretty
    (dotimes (i (fill-pointer *microcontrol-array*))
      (let ((entry (elt *microcontrol-array* i)))
        (when entry
          (format t "~%~d (~a): ~s [~a]"
                  i
                  (cl:if (microop-symbol i)
                      (microop-symbol i)
                      "")
                  (disassemble-microcode entry)
                  (cdr (assoc i *microcontrol-annotations*))))))))


