(in-package :scheme-mach)

(scheme-79:scheme-79-version-reporter "S79 ucode Assembler" 0 4 0
                                      "Time-stamp: <2022-04-07 14:25:24 gorbag>"
                                      "repatriated")

;; 0.4.0   4/ 7/22 split off from machine-micro - stuff use to build the PLA

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.4.0   3/18/22 snapping a line: 0.4 release of scheme-79 supports test-0 thru test-3. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; here we specify the microcontrol array and support for excuting
;; microinstructions.  Note that the array is populated by the code in
;; ucode-compiler once we've done a pre-pass on the microcode and
;; calculated symbolic bit values for registers, etc.

;; microcode looks like:

;; ustate: <next-ustate> <nano-operation> <from> <to>

;;  where <nano-operation> should be the "address" into the
;;  nanocontroller array from and to specify registers which can be
;;  referred to by the nanocode (but need not be specified - it
;;  depends on the nano-operation as to if these fields are used)

(defun translate-alias (register-name)
  (let ((alias (register-alias register-name)))
    (cl:if alias
        alias
        register-name)))

;; now generic 1/6/22 BWM
(defmethod analyze-code (expression-list)
  "Analyze code as a precompilation step and to enable encoding the
arguments to nano-operations (actual register usage in TO and FROM
fields, for instance)"
  ;; We can start with a guess as to which registers are TO and FROM
  ;; (if relevant) in the eventual object microcode the compiler will
  ;; generate.  we can then fine tune this later if we need to - the
  ;; purpose is just to allow us to encode the FROM and TO
  ;; (anaphorical) references to the nanocontroller in as few bits as
  ;; possible (per AIM 559). E.g., if only three registers are ever
  ;; used in the FROM field of a nanooperation, there's no need to
  ;; dedicate more than two bits to the FROM field.

  (mapc #'analyze-expression expression-list))

(defun analyze-expression (expression)
  ;; for now a case statement. OOP might be better in the long term so
  ;; we can add methods as we add new instructions and then everything
  ;; relevant to an instruction can be co-located in the source.
  (unless (symbolp expression) ; ignore symbols - we already analyzed the enclosing form
    (cl:cond
      ((consp (car expression))         ; thanks to cond
       (mapc #'analyze-expression expression))
      (t
       (case (car expression)           ; the instruction
         ((assign                       ; <to> <from>
           &rplaca
           &rplacd
           &rplaca-and-mark!
           &set-global-value
           &set-type)
          (analyze-to (cadr expression))
          (analyze-from (caddr expression)))

         ((&cons
           &=type?)                     ; in case it's a fetch
          (analyze-from (cadr expression))
          (analyze-from (caddr expression)))
        
         ((fetch                        ; <from>
           &write-to-pads
           &eq-val
           dispatch
           save)
          (analyze-from (cadr expression)))

         ((&read-from-pads
           restore)
          (analyze-to (cadr expression)))
         
         (&pointer? ; <from> - while pointer and in-use are just looking at bits, they might contain fetchs...
          (analyze-from (cadr expression)))

         (&in-use?
          (analyze-from `(&car ,(cadr expression)))) ; implicit car to get the bit

         ((&mark-car-being-traced! ; <from> - register pointing to memory so FROM register to *address* and then read
           &mark-in-use!
           &mark-car-trace-over!
           &car-being-traced?
           &unmark!)
          (analyze-from `(&car ,(cadr expression)))) ;implicit car as bit is on the cons cell
         
         (not
          (analyze-expression (cadr expression)))
         
         ((and or microlisp:if microlisp:progn)
          (mapc #'analyze-expression (cdr expression)))

         ;; implicit register uses
         (&increment-scan-up
          (mark-register-use :from '*scan-up*)
          (mark-register-use :to '*scan-up*))
         (&increment-scan-up-to-val
          (mark-register-use :from '*scan-up*)
          (mark-register-use :to '*val*))
         (&decrement-scan-down
          (mark-register-use :from '*scan-down*)
          (mark-register-use :to '*scan-down*))
         (&scan-up=scan-down?
          (mark-register-use :from '*scan-down*)
          (mark-register-use :from '*scan-up*))
         (&scan-down=0?
          (mark-register-use :from '*scan-down*))
         (&decrement-scan-down-to-val
          (mark-register-use :from '*scan-down*)
          (mark-register-use :to '*val*))
         (&val=0?
          (mark-register-use :from '*val*))
         (&val-displacement-to-exp-displacement
          (mark-register-use :from '*val*)
          (mark-register-use :to '*exp*))
         (&val-frame-to-exp-frame
          (mark-register-use :from '*val*)
          (mark-register-use :to '*exp*))
         (dispatch-on-stack
          (mark-register-use :from '*stack*))
         ((dispatch-on-exp-allowing-interrupts
           eval-exp-popj-to
           &frame=0?
           &displacement=0?)
          (mark-register-use :from '*exp*))
         ((&decrement-frame
           &decrement-displacement)
          (mark-register-use :from '*exp*)
          (mark-register-use :to '*exp*))
         (micro-call
          (mark-register-use :to '*retpc-count-mark*))
         (micro-return
          (mark-register-use :from '*retpc-count-mark*))
         ((go-to                        ; N/A
           &clear-gc-needed)            ; pad not register
          ;; nothing to do
          nil)

         (microlisp:cond
           ;; look at each clause
           (mapc #'analyze-expression (cdr expression)))

         (t
          ;; ok was it really t, or is this default
          (cl:if (eql (car expression) t)
                 ;; probably a cond clause
                 (mapc #'analyze-expression (cdr expression))
                 (warn "unhandled case analyze-expression: ~s" expression)))
         )))))

(defun analyze-to (to-form)
  ;; if the to-form resolves into a register, then count it. For
  ;; statistical purposes, count how many such references this
  ;; register has in the code.
  (cond-binding-predicate-to foo
    ((and (symbolp to-form)
          (register-alias to-form))
     (analyze-to foo))
    ((and (symbolp to-form)
          (register-p to-form))
     (mark-register-use :to to-form))
    ((consp to-form)
     (case (car to-form)
       (fetch ; not really a to, treat as a from since it's an indirect (from the register to *address*)
        (mark-register-use :to '*address*) ; count the use of the pseudo registers
        (mark-register-use :to '*memory*) ;effectively a write of this address
        (analyze-from (cadr to-form)))
       ((&car &cdr)
        (analyze-to (cadr to-form))) ; keep going
       (t
        (warn "unhandled cons case analyze-to: ~s" to-form))))
    (t
     (warn "unhandled case analyze-to: ~s" to-form))))

(defun analyze-from (from-form)
  ;; if the from-from resolves into a register, then count it. For
  ;; statistical purposes, count how many such references this
  ;; register has in the code.
  (cond-binding-predicate-to foo
    ((and (symbolp from-form)
          (register-alias from-form))
     (analyze-from foo))
    ((and (symbolp from-form)
          (register-p from-form))
     (mark-register-use :from from-form))
    ((symbolp from-form)
     ;; some other non-register symbol - ignore
     nil)
    ((consp from-form)
     (case (car from-form)
       ((&car
         &cdr
         fetch                          ; fetch because while we are indirect on the
                                        ; register we will need to put it on the
                                        ; *address* bus which counts as a from
         &global-value)
        ;; mark pseudo-registers, we are reading from this address
        (mark-register-use :to '*address*) ; count the use of the pseudo registers
        (mark-register-use :from '*memory*) ;effectively a write of this address
        (analyze-from (cadr from-form)))
       ((&rplaca ; go back to analyze expression to avoid duplication
         &rplacd
         &cons)
        (analyze-expression from-form))
       ((&get-interrupt-routine-pointer 
         dispatch-on-exp-allowing-interrupts)
        (mark-register-use :from '*interrupt*)
        nil) ; done
       
       (t
        (warn "unhandled cons case analyze-from: ~s" from-form))))
    (t
     (warn "unhandled case analyze-from: ~s" from-form))))

;; collect-ucode is probably only needed for debugging and console
;; support...  note essentially same as collect-ncode so could combine
;; if we generify some of the functions (next-state calculation, which
;; PLA to use, etc.) Or make into something generated by a macro...
(defun collect-ucode (offset)
  "Return a list of pairs of the offsets and the microcodes starting from
a given offset. Currently does not directly handle conditionals (TBD)"
  (declare (integer offset)) ; not used on-chip 
  (let* ((result nil)
         (current-code (elt *microcontrol-array* offset))
         (next-state (first current-code)))
    (cl:cond
      ((not current-code)
       (warn "Offset ~d does not represent a valid microcode!" offset))
      (t
       (push (cons offset current-code) result)
       (while (plusp next-state) ; terminates with a 0 or self-pointer
         (setq current-code (elt *microcontrol-array* next-state))
         (assert current-code () "Invalid next state in position ~D of microcontrol-array" next-state)
         (push (cons next-state current-code) result)
         (when (member (first current-code) (mapcar #'car result)) ; loop
           (return)) ; break out of the loop
         (setq next-state (first current-code))
         )
       (nreverse result)))))
