(in-package :scheme-mach) ; pull into scheme-mach so all the defufn can see them

(scheme-79:scheme-79-version-reporter "S79 ucode compiler defs" 0 3 7
                                      "Time-stamp: <2022-02-09 12:09:58 gorbag>"
                                      "line disambiguation")

;; 0.3.7   2/ 9/22 way too many things (fns, variables) with "line" in their name
;;                    and it's ambiguous.  Splitting so "line" refers to,
;;                    e.g. an output (log) line, "expression" refers to a
;;                    'line' of code (single expression in nano or microcode
;;                    land typically, and because we used (READ) it wasn't
;;                    confined to a single input line anyway) and "wire" to
;;                    refer to, e.g., a control or sense 'line' on a register.

;; 0.3.6   2/ 4/22 mostly revoke 9/14/21 patch in light of other
;;                   rewrites, it was overly ambitious

;; 0.3.5   2/ 2/22 use intentional upla fns

;; 0.3.4   1/31/22 refreplacop - leverage *constituent-assignment-fn* so
;;                    &cons does the right thing.

;; 0.3.3   1/27/22 generate-ucode: when asserting nano-op not defined, report
;;                    which one

;; 0.3.2   1/24/22 compile-expression now errors out when an undefined opcode is used.

;; 0.3.1   1/18/22 cleanup obsolete code: removing special treatment of registers
;;                    which required multiple control wires for TO as new
;;                    covering set computation deals with it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.1.12  1/ 7/22 moved method for compile-expression here; toplevel is now
;;                    in support files and this is the "interface" fn to
;;                    the project (for now)

;; 0.1.11  1/ 6/22 use opcode-fn instead of property, also other accessors

;; 0.1.10 12/14/21 use new special-register-p fn

;; 0.1.9  12/ 3/21 Move some variable definitions and functions to
;;                   support/pla-support/compile-core-defs
;;                 update scheme-79-mcr-i -> "" (if from
;;                   microlisp-int or fpga-pla-build-tools)
;;                 update scheme-79-mcr -> microlisp

;; 0.1.8  11/30/21 generate-ucode now signals an error when an
;;                   undefined nanoinstruction is used

;; 0.1.7  11/23/21 fix defrplacop to simplify the cons reference (should
;;                   be of form (fetch *register*)  )

;; 0.1.6  11/17/21 specialize-instruction: pulls together code that had
;;                   been in many defufn bodies.
;;                from-direct-register-p so we know if our from
;;                   field refers to a register directly (i.e. we're
;;                   getting the operand from the register) or
;;                   indirectly (i.e. we're getting what the register
;;                   points to)
;;                defrplacop because all the &rplac* forms are similar, 
;;                   this macro makes generating them as easy as naming
;;                   them and the appropriate nano-instructions they will
;;                   map to.

;; 0.1.5   9/27/21 scheme-79-mcr:progn, if, cond

;; 0.1.4   9/14/21 Compile-parameter: detect if code already written (so
;;                   return value suppressed from function application)
;;                   and if so, don't try to write the NIL to the
;;                   upla-stream!

;; 0.1.3   9/ 7/21 Encode the TO field using the FROM anaphor if the nanoop is a member of
;;                   *from-to-nano-operations* (see s79-nanocode.lisp)

;; 0.1.2   8/31/21 generate-ucode-with-constant: a variant of
;;                   generate-ucode that stuffs a constant into the 
;;                   from field

;; 0.1.1   8/23/21 Add type-constant support to the microcontroller

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.8  8/18/21 compile-embedded-expression - so we can distinguish
;;                  calling compile-parameter from the parsing process
;;                  vs. a macro-like expansion via defufn (mainly for
;;                  adding annotations)

;; 0.0.7  3/ 2/21 move *function-being-compiled* here

;; 0.0.6  2/27/21 translate aliased registers

;; 0.0.5  2-24-21 generate-ucode - use '0' for from/to register if not
;;                  used (instead of nil)

;; 0.0.4  2-23-21 generate-conditional-ucode redone for assembler
;;                  interface

;; 0.0.3  2-20-21 add with-intermediate-argument

;; 0.0.2  2-19-21 add compile-parameter-args-last

;; 0.0.1          compile-parameter should respect typical :args-first
;;                    precedence generate-conditional-ucode (new)

;; 0.0.0          defvar's moved from ucode-compiler so we can load
;;                    them earlier in the compilation process

;; While this is inspired by the notes in AIM 559, it is not a
;; faithful rendition of the original compiler as it is not adequately
;; documented for that. However, I hope it does replicate the original
;; "in spirit" and remains faithful to the underlying concepts.

;; Note that the microcode is read in and from that some information
;; is gathered that is used to create/compile the nanocode. The
;; nanocode is compiled before the microcode is, so the appropriate
;; integer bit representations for registers offsets into the nano
;; array, etc. can be determined as AIM 559 says the original did. We
;; also use thier field scheme:

;; Nano-words are selected by state information from the micro control
;; and any next state generated from the nano control. The micro
;; control specifies to and from registers (as bits per above). The
;; nano control generates pad and register controls (as well as the
;; next nano state). Note that freeze may be asserted for multi-word
;; nano sequences to prevent additional micro words from being
;; decoded. Example multiple word nano instructions are given in the
;; paper but are not exhaustive, and rely on microinstructions that
;; differ from the documented microcode (so may be either output from
;; the microword compiler based on that input, or are from a different
;; version of the same - their intent is to communicate the flavor of
;; thier implementation not document it for reproduction in the future
;; after all, though one would really have some expectation of the
;; latter as a matter of *science* - results should be reproducable!).

;; (defnano (do-car)
;;    ((from*) (ale))
;;    ((to*) (read)))

;; (defnano (do-cdr)
;;    ((from*) (ale))
;;    ((to*) (read cdr)))

;; (defnano (do-restore)
;;    (() (from-stack ale))
;;    ((to*) (read))
;;    (() (read cdr to-address-stack to-type-stack)))

;; Here the from* and to* forms are meant to be taken as pronouns that
;; refer to the values passed from the microcode, so "do-restore" can
;; be seen to ignore the from* value.

;; To translate the above into what (I think) actually happens, the
;; first do-car instruction will take whatever is in the FROM field in
;; the microcode and put it on the bus, and raise ALE (so it's
;; transmitted to the external memory as an address). The second
;; instruction reads the response from memory onto the bus and gates
;; it into the TO register. So our job is to make sure we do this
;; gating during the appropriate PH1 and PH2 (simulated) clock cycles,
;; have the external memory respond to it, etc. in the hardware
;; emulation, but for the work below it's just to capture our
;; representations for registers that will be used for both the micro
;; and nano code, and then to compile the "defnano" terms into the
;; nanocontrol array generating a symbol table for linking the
;; microcode to nanocontrolarray offsets (states).

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
;; [44: MARK-NODE-3] CONDITION-BIT CLEAR (even) - typically 'fail'
;; [45: MARK-NODE-1] CONDITION-BIT SET (odd) - typically 'success'

;; for the current release, compile-expression is the primary interface to the
;; project-specific code to compile the microlisp. The second interface is to
;; the assembler passes. See ulisp-assembler.lisp and upla-assembler.lisp
(defmethod compile-expression (expression)
  (declare (type list expression)) ; so we remember it's not a string
  (let* ((*from-register* nil)
         (*to-register* nil)
         (*expression-opcode* (car expression))
         (opcode-fn (opcode-fn *expression-opcode*))
         (opcode-fn-type (ucode-precedence *expression-opcode*)))
    
    (cl:cond
     ((null opcode-fn)
      (error "Opcode ~s has not been defined!" *expression-opcode*))
     ((eql opcode-fn-type :args-first)
      (write-generated-code *upla-stream* expression
                            ;; suppress internal code generation 9/13/21 BWM
;                            (let ((*upla-stream* nil))
                            (apply opcode-fn (mapcar #'(lambda (x)
                                                         (compile-parameter *upla-stream* x))
                                                     (cdr expression)))
                            "compile-expression"))
     (t
      (compile-parameter-args-last *upla-stream* *expression-opcode* opcode-fn (cdr expression) expression)
      ))))

;; useful for calling compile-expression interactively
(defun debug-compile-expression (exp)
  (declare (type list exp)) ; so we remember it's not a string
  (let ((*upla-stream* cl:*standard-output*))
    (compile-expression exp)))

;; this is specific to Scheme-79 I think
(defvar *intermediate-in-use* nil)

(defmacro with-intermediate-argument (&body body)
  "Make sure we don't accidentally recursively reuse the intermediate argument register"
  `(cl:progn
     (assert (not *intermediate-in-use*) () "Recursive reuse of *intermediate-argument*")
     (let ((*intermediate-in-use* t))
       ,@body)))

(defun generate-ucode (nano-operation &optional (from *from-register*) (to *to-register*) (tag nil))
  "looks up the code values for each parameter and generates a single n-bit microcode 'word'"
  ; defvar comes in machine-defs which isn't loaded yet
  (declare (special *nanocontrol-symtab* *from-to-nano-operations*)) 
  ;; for the moment, return a list
  (when (consp to)
    (assert (endp (cdr to)) (to) "generate-ucode: can't currently deal with multiple TO registers")
    (setq to (translate-alias (car to))))

  (let ((nano-op (cdr (assoc nano-operation *nanocontrol-symtab*))))
    (assert nano-op (nano-operation) "Nano operation ~s not defined?" nano-operation)
    (list
     tag ;; will be next-ustate if this is a multi-u-instruction op
     nano-op
     (or (lookup-from-anaphor from) 0)
     ;; currently only handle a singleton to register
     (cl:cond
      ((eql nano-operation 'microlisp-shared::sense-and-branch)
       to) ; just use it - it's the sense bit(s)
      ((member nano-operation *from-to-nano-operations*)
       (or (lookup-from-anaphor to) 0))
      (t
       (or (lookup-to-anaphor to) 0))))))

(defun generate-ucode-with-constant
    (nano-operation constant &optional (to *to-register*) (tag nil))
  "Similar to generate-ucode, but stuffs a constant into the FROM
reference"
  ;; defvar comes in machine-defs or s79-nanocode which isn't loaded yet
  (declare (special *nanocontrol-symtab* *from-to-nano-operations*)) 
  ;; for the moment, return a list

  (when (consp to)
    (assert (endp (cdr to)) (to)
            "generate-ucode: can't currently deal with multiple TO registers")
    (setq to (translate-alias (car to))))

  ;; We assemble our own here (instead of using generate-ucode) to
  ;; keep the constant from being interpreted as a register reference
  (list
   tag ;; will be next-ustate if this is a multi-u-instruction op
   (cdr (assoc nano-operation *nanocontrol-symtab*))
   constant
   ;; currently only handle a singleton to register
   (cl:if (member nano-operation *from-to-nano-operations*)
          (or (lookup-from-anaphor to) 0)
          (or (lookup-to-anaphor to) 0))))

(defun generate-conditional-ucode (sense-bit fail-address
                                   &optional (from *from-register*)
                                     (original-branch-type 'microlisp-shared::branch))
  "the offsets are the number of uinstructions being emitted to skip
if we fail or succeed. Note that 0 should be used for completion (no
more uinstructions needed) and 1 points to the instruction following
this one."
  (let ((nano-operation (cl:if (eql original-branch-type 'microlisp-shared::branch)
                          'microlisp-shared::sense-and-branch
                          'microlisp-shared::sense-type-and-branch)))
  
    (generate-ucode nano-operation ; nano
                    from
                    (get-sense-wire-encoding sense-bit) ; "to" (overloaded)
                    ;; Note we only need the fail address here because
                    ;; fail is an address whose low order bit is 0, and
                    ;; success is same address whose low order bit is 1.
                    fail-address)))

(defun generate-conditional-ucode-with-constant (sense-bit constant fail-address original-branch-type)
  (declare (special *nanocontrol-symtab*)) ; defvar comes in machine-defs which isn't loaded yet
  (let ((nano-operation (cl:if (eql original-branch-type 'microlisp-shared::branch)
                          'microlisp-shared::sense-and-branch-const
                          'microlisp-shared::sense-type-and-branch-const)))
    ;; We assemble our own here (instead of using generate-ucode) to
    ;; keep the constant from being interpreted as a register
    ;; reference
    (list
     fail-address ; tag
     (cdr (assoc nano-operation *nanocontrol-symtab*)) ; nano
     constant ; from (overloaded with the constant)
     (get-sense-wire-encoding sense-bit)))) ;to (overloaded with condition)

;; probably a better way to do this, but wait until I get things
;; working then refactor! (e.g., make all work like &cons ?)
(defvar *current-expression* nil
  "The current expression we are working on")

(defun compile-parameter-args-last (stream opcode opcode-fn parameter-list original-parameter)
  (declare (ignore stream)) ; for now
  (let ((*current-expression* original-parameter))
    (ecase opcode
      ;; these do own compilation (may want to make adding to this
      ;; list declaration-based)
      ((&cons microlisp:progn microlisp:cond microlisp:if)
       ;; may want to pass stream too... for now use the bound *upla-stream*
       (apply opcode-fn parameter-list))
      ((assign &rplaca &rplacd
                &rplaca-and-mark! &rplaca-and-unmark! 
                &rplacd-and-mark-car-being-traced! &rplacd-and-mark-car-trace-over!) ; moved assign here 1/28/22 BWM
       (let ((*upla-suppress-annotation* *upla-suppress-annotation*))
         (unless (from-direct-register-p (cadr parameter-list) opcode)
           (upla-write-code-annotation original-parameter)
           (setq *upla-suppress-annotation* t))
         (funcall opcode-fn (car parameter-list) (cadr parameter-list))))
      ((save)
       ;; code gets generated recursively
       (upla-write-code-annotation original-parameter) ; make sure the save itself gets into the log
       (funcall opcode-fn (car parameter-list))) ; will generate the code
      ((&global-value &car &cdr) ; already captures the expression in the output file
       (funcall opcode-fn (car parameter-list))))))
  
(defun compile-parameter (stream arg &optional (check-args-first-p t))
  "Similar to compile expression, but while a 'expression' is toplevel, an 'arg'
is an argument to another opcode (but may itself be an function)"
  (cl:cond
    ((consp arg)
     (let* ((opcode (car arg)) ; maintain the globals from compile-expression
            (opcode-fn (opcode-fn opcode))
            (opcode-fn-type (ucode-precedence opcode))
            (opcode-constituent-p (ucode-constituent opcode))
            (opcode-suppress-logging-p (ucode-suppress-logging opcode))
            (*enclosing-opcode* (cl:cond
                                  ;; ignore macro-fns like cond
                                  ((member *enclosing-opcode* *defumac-macros*)
                                   opcode)
                                  ((not (null *enclosing-opcode*))
                                   *enclosing-opcode*)
                                  (t
                                   *expression-opcode*))))
       (assert opcode-fn (arg) "compile-parameter: ~s is not a defined opcode!" opcode)
       (assert opcode-fn-type (arg) "compile-parameter: ~s does not have a valid precence!" opcode)
       (cl:cond
         ((eql opcode-fn-type :args-first)
          (let ((code (apply opcode-fn 
                             (mapcar #'(lambda (x) (compile-parameter nil x))
                                     (cdr arg)))))
            ;; if we already generated the code, the result will be nil and we can go on
            (when code
              (write-generated-code
               (cl:if (or opcode-constituent-p opcode-suppress-logging-p)
                 nil ; this will suppress writing to the pass0 file if it is a constitutent - we're called recursively.
                 stream)
               arg
               code
               "cp"))))
         (check-args-first-p
          (error "compile-parameter: args-first expected; embedded args-last opcode?! ~s (fn: ~s expression: ~s)"
                 arg *function-being-compiled* *expression-opcode*))
         (t ; args-last
          (let ((*enclosing-opcode* (or *enclosing-opcode* opcode)))
            (compile-parameter-args-last stream opcode opcode-fn (cdr arg) arg))))))
    (t
     arg)))

(defun compile-embedded-expression (exp)
  "this is the case where the defufn expands into more code (essentially a macro)"
  (let ((opcode-constituent-p (ucode-constituent (car exp)))
        (opcode-suppress-logging-p (ucode-suppress-logging (car exp))))
    (cl:cond
     ((and opcode-suppress-logging-p *upla-stream* (eql (car exp) 'microlisp:progn))
      (upla-write-code-annotation  '(microlisp:PROGN)) ;; minimum for annotation
      (upla-write-local-comment exp) ;; not used for annotation (too long)
      (compile-parameter *upla-stream* exp nil)) ;pass stream so the members can be written
     ((and opcode-constituent-p *upla-stream*)
      (upla-write-code-annotation exp) ;; stuff like &cons is worth putting in annotation
      (compile-parameter *upla-stream* exp nil)) ;pass stream so the members can be written
     (t
      (let ((retval (write-generated-code *upla-stream* exp (compile-parameter nil exp nil) "cee")))
        (cl:if *upla-stream* nil retval))))))

(defun from-direct-register-p (from-reference enclosing-opcode)
  "Returns non-nil if the passed reference is to a register and not
indirect on the register."
  ;; collect some info about this call
  ;; (note-if *debug-compiler* "from-direct-register-p: from-reference: ~s" from-reference) 

  (cl:cond
   ((not (consp from-reference))
    (cl:if (register-p from-reference)
      from-reference
      nil))
   ((member (car from-reference) '(&car &cdr &cons &get-interrupt-routine-pointer))
    nil) ; indirect
   (t ;; a cons
      (or (and 
           (eql (car from-reference) 'fetch)
           (register-p (cadr from-reference))
           (cadr from-reference))
          ;; this is the slow way to do it
          (progn
            (note-if *debug-compiler* "from-direct-register-p:slow  ~s" from-reference)
            (let* ((*enclosing-opcode* enclosing-opcode)
                   (from-code (compile-parameter *upla-stream* from-reference nil)))
              ;; what we did was compile the reference to see if we get a
              ;; register back that's the most certain, but expensive way to do
              ;; it.  Easier and cheaper is just to parse the from-reference to
              ;; see if it's a simple fetch of a register or something like &car
              ;; or &cdr of a register. (TBD)
              (cl:if (register-p from-code)
                from-code
                nil)))))))

(defmacro defrplacop (name (core-uop-symbol))
  "&rplac* operations all are pretty much the same. Simplest to macroize them"
  (let ((cons (gensym))
        (value (gensym))
        (value-ref (gensym)))
    ;; the cons better look like (fetch <register-name>)
    `(defufn ,name (,cons ,value :args-last t)
       (assert (and (consp ,cons)
                    (eql (car ,cons) 'fetch)) (,cons)
                    "Passed cons reference didn't look like (fetch <register-name>)")
       (let ((,value-ref (from-direct-register-p ,value ',name))
             (*constituent-assignment-fn* `(,',name ,,cons)))
         (cl:cond
           ((not (null ,value-ref))
            ;; generate assembly code directly
            (write-generated-code *upla-stream* *current-expression* 
                                  `(((from ,,value-ref) (to ,(cadr ,cons)) ,',core-uop-symbol))
                                  ,(string name))
            nil) ; wrote it so don't return it
           ((and (consp ,value) (member (car ,value) '(&car &cdr)))
            ;; generate an intermediate argument to store the result of the car or cdr and then assign it
            (setq *constituent-assignment-fn* nil) ; clear this
            (with-intermediate-argument
              (compile-embedded-expression
               `(microlisp:progn
                  (assign *intermediate-argument* ,,value)
                  (,',name ,,cons (fetch *intermediate-argument*))))))
           (t
            (note-if *debug-compiler* "complex argument to rplac* form: !s" ,value)
            ;; *constituent-assignment-fn* should generate the right thing
            (compile-embedded-expression ,value))
            )))))
