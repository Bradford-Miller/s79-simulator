(in-package :scheme-mach)

(scheme-79:scheme-79-version-reporter "S79 Nano Assembler" 0 4 0
                                      "Time-stamp: <2022-04-07 14:25:40 gorbag>"
                                      "repatriated")

;; 0.4.0   4/ 7/22 repatriate code used to build PLAs from machine-nano.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.4.0   3/18/22 snapping a line: 0.4 release of scheme-79 supports test-0 thru test-3. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; here we specify the nanocontrol array and define
;; nanoinstructions. Note that the array is populated by the code in
;; ucode-compiler once we've interpreted the microcode and calculated
;; symbolic bit values for registers, etc.

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
;; thier implementation not document it for reprodution in the future
;; after all).

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

;;  To translate the above into what (I think) actually happens, the
;; first do-car instruction will take whatever is in the FROM register
;; and put it on the bus, and raise ALE (so it's transmitted to the
;; external memory as an address). The second instruction reads the
;; response from memory onto the bus and gates it into the TO
;; register. So our job is to make sure we do this gating during the
;; appropriate PH1 and PH2 (simulated) clock cycles, have the external
;; memory respond to it, etc. in the hardware emulation, but for the
;; work below it's just to capture our representations for registers
;; that will be used for both the micro and nano code, and then to
;; compile the "defnano" terms into the nanocontrol array generating a
;; symbol table for linking the microcode to nanocontrol array offsets
;; (states).

;; so first let's define the terms that can come inside of a defnano
;; form: from* and to* are anaphors for the from and to fields of the
;; current microcode instruction, while other things are operations
;; that trigger control wires to be set and then shifted out at the
;; right time based on the clock.  We'll incorporate the example,
;; above into our definitions.

;; from* - anaphor for whatever register is in the FROM field of the
;;         current microinstruction. It should be moved to the bus,
;;         so set the from control wire for that register.

;; to*   - anaphor for whatever register is in the TO field of the
;;         current microinstruction. It should be filled from the bus,
;;         so set the to control wire for that register.

;; ale   - put the current content of the bus into the *address*
;;         register, set *address-pads* at the right time, and set
;;         *ale* for a clock

;; read  - drop *ale* and raise *read*, move contents of the external
;;         *memory-pads* to *memory* and the bus

;; cdr   - set *cdr* in conjunction with either *read* or *write*
;;         (else should be clear)

;; from-stack
;; to-address-stack
;; to-type-stack
;;       - these all specify a control wire and a particular register,
;;         i.e. a particular control wire.

;; gc-needed
;;       - set *gc-needed* output and latch high

;; clear-gc
;;       - clear *gc-needed* output and latch low (called by anything
;;         setting mark as a gc has been triggered)

(defun make-nanocontrol-array-element (nanocontrol-tuple)
  "create a bitvector containing the nanocontrol array row (tuple ->bitvector)"
  (declare (ignore nanocontrol-tuple))
  (tbd))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; reset nanocontrols
  (setf (fill-pointer *nanocontrol-array*) 0)
  (setq *nanocontrol-symtab* nil))

;; generate nano-instructions in the form:

;;   <pad-controls>  <register-controls> <next-state>

;;  (which follows Figure 6 in AIM 559) where the pad definitions are
;;  in sim-machine-external.lisp, and the register definitions are in
;;  machine-defs.lisp.

;;  Because of the use of anaphor in the nanoinstructions and to
;;  reduce the number of nanoinstructions that need to be generated,
;;  we will have fake register numbers for those that refer to
;;  microcode fields. We need to create (invariant) register numbers
;;  to refer to these registers which can be later translated to a
;;  control MUX but here will just translate back to appropriate
;;  simulator code. We can't just use the codes the microcode
;;  determined, because those have been compressed based on usage (to
;;  limit the number of bits needed) separately for the FROM and TO
;;  fields. That is, FROM register 3 and TO register 7 might represent
;;  the same register. So here we define an "objective" function for
;;  registers to specify a unique register, and can then set up a
;;  "translation" table between the microcode version(s) of register
;;  numbers and ours.

;; make sure we always count *intermediate-argument* which is generated by the
;; compiler...
(defparameter *default-from-registers* '(*intermediate-argument*)
  "Additional from registers that are typically generated by the compiler")

(defparameter *default-to-registers* '(*intermediate-argument*)
  "Additional to registers that are typically generated by the compiler")

;; setting up the anaphors and allowing the nanocode to access them:
(let ((from-anaphor-table nil)
      (to-anaphor-table nil))
  (defun clear-anaphors ()
    (setq from-anaphor-table nil)
    (setq to-anaphor-table nil))

  (defun debug-anaphors ()
    (values :from from-anaphor-table :to to-anaphor-table))
  
  ;; assumes the code is an int, eventually will be a bit vector

  (defun setup-anaphors ()
    (let ((to-code 1) ; start at 1
          (from-code 1))

      (dolist (to (union *to-registers-used* *default-to-registers*)) 
        (update-alist to-code to to-anaphor-table)
        (incf to-code))

      (dolist (from (union *from-registers-used* *default-from-registers*))
        (update-alist from-code from from-anaphor-table)
        (incf from-code))))

  (defun to-code-to-anaphor (code)
    "given a code, look up the to register"
    (cdr (assoc code to-anaphor-table)))

  (defun from-code-to-anaphor (code)
    "given a code, look up the from register"
    (cdr (assoc code from-anaphor-table)))
  
  (defun lookup-to-anaphor (register)
    "given a register, look up the anaphor code"
    (car (rassoc (translate-alias register)
                 to-anaphor-table)))
  
  (defun lookup-from-anaphor (register)
    (car (rassoc (translate-alias register)
                 from-anaphor-table)))
  )

(defmethod clear-project-specific-tables :after ()
  (clear-anaphors))

(defmethod initialize-project-specific-tables :after ()
  (setup-anaphors))
      
(defun parse-defnano (instructions)
  "Run at compile time to parse a defnano form into a bit vector we
can use to populate the nano control array."
  (macrolet ((ior-bit (var bit)
               `(setq ,var (logior ,var ,bit))))
    (let ((result nil))
      (dolist (expression instructions)
        (assert (endp (cddr expression)) (expression)
                "defnano expression: ~s is malformed" expression)
        (let ((register-spec (car expression))
              (register-bit-spec 0)
              (control-spec (cadr expression))
              (pad-bit-spec 0))
          ;; set control wires on register. Note that some control
          ;; specs appear to include the register (not sure why not
          ;; just put on register spec but I didn't design this
          ;; language!)
          (cl:cond
            ((null register-spec)
             ;; one of the controls should specify a register, unless this is a
             ;; no-op
             )
            (t
             (setq register-bit-spec
                   (reduce #'logior register-spec 
                           :initial-value 0
                           :key #'(lambda (r) 
                                    (rr-value r))))))
          (cl:cond
            ((null control-spec)
             ;; must be a no-op
             )
            (t
             ;; remember the flags field we defined for register? Let's
             ;; reuse that number scheme here for simplicity ignoring
             ;; the sense wires; they're of the form
             ;; +register-<control>+ to specify the bit. The register
             ;; itself is just *<register-name>* of course though later
             ;; when we interpret the nanoinstruction we'll need both to
             ;; have an argument for the register-<control-wire>-p
             ;; function.

             ;; we can't be quite as clever as we were with the register
             ;; names (above) since there are pad controls, register
             ;; references, etc. Well I guess we could be but we'd need
             ;; to define a bunch of new constants. For now let's just
             ;; use a case statement to get the right bits and or them.

             ;; since some of these terms may actually include register
             ;; references process one at a time rather than a reduction
             ;; (for now, anyway)
             (flet ((code-reg (bit-pattern)
                      (ior-bit register-bit-spec bit-pattern))
                    (code-pad (bit-pattern)
                      (ior-bit pad-bit-spec bit-pattern)))
               (dolist (spec control-spec)
                 (case spec
                   (set-condition       ; special
                    (code-pad +pad-conditional+)) ; pseudo-pad to switch how u-pc is calculated
                   (ale
                    (code-reg +rr-to-address+) ;; move what's on the bus to the address pseudo-register
                    (code-pad +pad-ale+)) ; should trigger moving *address* to the pads
                   (read
                    (code-pad +pad-read+) ; should trigger moving the pads to *memory*
                    (code-reg +rr-from-memory+)) ; and from there to the bus
                   (write
                    (code-reg +rr-to-memory+) ; move bus to *memory*
                    (code-pad +pad-write+)) ; and signal it should be written
                   (cdr
                    (code-pad +pad-cdr+))
                   (read-interrupt
                    (code-pad +pad-read-interrupt+))
                   (gc-needed
                    (code-pad +pad-gc-needed+)) ; should be latched
                   (clear-gc
                    (code-pad +pad-clear-gc+)) ; to clear the latch
                   (mask-interrupts
                    (code-pad +pad-mask-interrupts+)) ; should be latched
                   (clear-mask-interupts
                    (code-pad +pad-clear-mask-interrupts+))
                   (t
                    (code-reg (rr-value spec))))))))

          ;; don't convert to a bit-vector yet to make it easier to work with (TBD)
          (setq result (append result (list (list pad-bit-spec register-bit-spec nil)))))) 
      result)))

(defun decode-nanocontrol-pads (whole-line)
  "given a line from the nanocontrol, decode pad references into symboloic
components (e.g., for printing)"
  (let ((result nil)
        (line (first whole-line))) ; right now line is a triple of integers,
                                   ; first one is the pad encoding
    ;; test each bit
    (cl:if (plusp (logand line +pad-run-nano+))
           (push 'run-nano result))
    (cl:if (plusp (logand line +pad-ale+))
           (push 'ale result))
    (cl:if (plusp (logand line +pad-read+))
           (push 'read result))
    (cl:if (plusp (logand line +pad-write+))
           (push 'write result))
    (cl:if (plusp (logand line +pad-cdr+))
           (push 'cdr result))
    (cl:if (plusp (logand line +pad-read-interrupt+))
           (push 'read-interrupt result))
    (cl:if (plusp (logand line +pad-gc-needed+))
           (push 'gc-needed result))
    (cl:if (plusp (logand line +pad-clear-gc+))
           (push 'clear-gc result))
    (cl:if (plusp (logand line +pad-conditional+))
           (push 'set-conditional result))
    (cl:if (plusp (logand line +pad-mask-interrupts+))
           (push 'mask-interrupts result))
    (cl:if (plusp (logand line +pad-clear-mask-interrupts+))
           (push 'clear-mask-interrupts result))
    result))
        
(defun decode-nanocontrol-registers (whole-line)
  "Given a line from the nanocontrol, decode register references into symbolic
components (e.g., for printing)"
  ;; right now, just deal with the control side as an integer - fix later
  ;; maybe can get defureg to automate building this?
  (let ((result nil)
        (line (second whole-line))) ; right now line is a triple of integers,
                                    ; pick the second one
    ;; test each bit
    (cl:if (plusp (logand line +rr-from*+))
        (push 'from* result))
    (cl:if (plusp (logand line +rr-to*+))
        (push 'to* result))
    (cl:if (plusp (logand line +rr-from-to*+))
           (push 'from-to* result))
    (cl:if (plusp (logand line +rr-from*-type+))
           (push 'from*-type result))
    (cl:if (plusp (logand line +rr-from-type-const*+))
           (push 'from-type-const* result))
    (cl:if (plusp (logand line +rr-from-const*+))
           (push 'from-const* result))
    (cl:if (plusp (logand line +rr-to-memtop+))
        (push 'to-memtop result))
    (cl:if (plusp (logand line +rr-from-memtop+))
        (push 'from-memtop result))
    (cl:if (plusp (logand line +rr-to-type-newcell+))
        (push 'to-type-newcell result))
    (cl:if (plusp (logand line +rr-to-address-newcell+))
        (push 'to-address-newcell result))
    (cl:if (plusp (logand line +rr-from-newcell+))
        (push 'from-newcell result))
    (cl:if (plusp (logand line +rr-from-incremented-newcell+))
        (push 'from-incremented-newcell result)) 
    (cl:if (plusp (logand line +rr-to-type-exp+))
        (push 'to-type-exp result))
    (cl:if (plusp (logand line +rr-to-displacement-exp+))
        (push 'to-displacement-exp result))
    (cl:if (plusp (logand line +rr-to-frame-exp+))
        (push 'to-frame-exp result))
    (cl:if (plusp (logand line +rr-from-exp+))
        (push 'from-exp result))
    (cl:if (plusp (logand line +rr-from-decremented-exp+))
           (push 'from-decremented-exp result))
    (cl:if (plusp (logand line +rr-from-decremented-frame-exp+))
           (push 'from-decremented-frame-exp result))
    (cl:if (plusp (logand line +rr-from-decremented-displacement-exp+))
        (push 'from-decremented-displacement-exp result))
    (cl:if (plusp (logand line +rr-to-type-val+))
        (push 'to-type-val result))
    (cl:if (plusp (logand line +rr-to-address-val+))
        (push 'to-address-val result))
    (cl:if (plusp (logand line +rr-from-val+))
        (push 'from-val result))
    (cl:if (plusp (logand line +rr-to-args+))
        (push 'to-args result))
    (cl:if (plusp (logand line +rr-from-args+))
        (push 'from-args result))
    (cl:if (plusp (logand line +rr-to-type-stack+))
        (push 'to-type-stack result))
    (cl:if (plusp (logand line +rr-to-address-stack+))
        (push 'to-address-stack result))
    (cl:if (plusp (logand line +rr-from-stack+))
        (push 'from-stack result))
    (cl:if (plusp (logand line +rr-to-display+))
        (push 'to-display result))
    (cl:if (plusp (logand line +rr-from-display+))
        (push 'from-display result))
    (cl:if (plusp (logand line +rr-to-intermediate-argument+))
        (push 'to-intermediate-argument result))
    (cl:if (plusp (logand line +rr-from-intermediate-argument+))
        (push 'from-intermediate-argument result))
    (cl:if (plusp (logand line +rr-to-type-retpc-count-mark+))
        (push 'to-type-retpc-count-mark result))
    (cl:if (plusp (logand line  +rr-to-address-retpc-count-mark+))
        (push 'to-address-retpc-count-mark result))
    (cl:if (plusp (logand line  +rr-from-retpc-count-mark+))
        (push 'from-retpc-count-mark result))
    (cl:if (plusp (logand line +rr-from-nil+))
        (push 'from-nil result))
    (cl:if (plusp (logand line +rr-to-address+))
        (push 'to-address result))
    (cl:if (plusp (logand line +rr-from-address+))
        (push 'from-address result))
    (cl:if (plusp (logand line +rr-to-memory+))
        (push 'to-memory result))
    (cl:if (plusp (logand line +rr-from-memory+))
           (push 'from-memory result))
    (cl:if (plusp (logand line +rr-to-micro-pc+))
           (push 'to-micro-pc result))
    (cl:if (plusp (logand line +rr-from-interrupt+))
        (push 'from-interrupt result))
    result))

;; for debugging
(defun decode-nanocontrol-expression (expression)
  (list (decode-nanocontrol-registers expression)
        (decode-nanocontrol-pads expression)))

;; decode the whole array
(defun decode-nanocontrol-array ()
  (let ((result nil))
    (dotimes (i (fill-pointer *nanocontrol-array*))
      (push (decode-nanocontrol-expression (elt *nanocontrol-array* i)) result))
    (nreverse result)))

(defun find-nanocontrol-content (expression-spec
                                 &optional (starting-offset 0) (ending-offset nil))
  ;; for now expression-spec should be a triple, pad-spec control-spec
  ;; next-state if next-state is NIL it hasn't been assigned yet (as we expect
  ;; from the passed expression-spec but not the content of the array)
  (let ((posn (position-if #'(lambda (entry)
                               (and (= (car entry) (car expression-spec))
                                    (= (cadr entry) (cadr expression-spec))))
                           *nanocontrol-array*
                           :start starting-offset
                           :end ending-offset)))
    (cl:if (not (null posn))
        (values (elt *nanocontrol-array* posn) posn)
        nil)))

;; this should reproduce the behavior described in the AIM for compression of
;; the nanocontrol
(defun match-into-nanocontrol (array-expressions
                               &optional (current-offset 0) (ending-offset nil))
  ;; if all the array expressions match a sequence into nanocontrol, return the
  ;; offset of the first such control expression
  (let ((nexpression nil)
        (expression (car array-expressions)))
    (msetq (nexpression current-offset) (find-nanocontrol-content expression current-offset ending-offset))
    (unless nexpression
      (return-from match-into-nanocontrol nil)) ; fail
    
    ;; does the rest of the entry match as well?
    (cl:cond
      ((endp (cdr array-expressions))
       (values nexpression current-offset)) ; done matching
      ((match-into-nanocontrol (cdr array-expressions)
                               (third nexpression)
                               (1+ (third nexpression))) ; forces match on only
                                                         ; the next-state for
                                                         ; that instruction.
       (values nexpression current-offset))
      (t
       ;; try another entry note current-offset was updated by the
       ;;  last find, so this should find another one if it exists
       (match-into-nanocontrol array-expressions (1+ current-offset) nil)))))

(defun update-nanocontrol (array-expressions &optional force-p)
  "If force-p is non-nil, add even if a duplicate (because our system
is currently sensitive to the nanocontrol name, should typically only
be used for one-expression defnano"
  
  ;; do we already have such a set of expressions in our control array?
  (mlet (match posn) (match-into-nanocontrol array-expressions)
    (cl:cond
      ((and match (not force-p))
       ;; just use that one - optionally print a diagnostic so we know what's
       ;; going on
       (note-if *debug-defnano*
                ";;debug-defnano: found match at offset ~d" posn)
       posn)
      (t
       ;; no so just add all (?) the expressions.  check each cdr to see if we
       ;; can save space by jumping into another nanocontrol sequence (just
       ;; like in the AIM)
       (let ((first-instruction nil)) ; keep track of entry point so we can bind it to the name
         (block done
           (do ((current-expression (car array-expressions) (car remaining-expressions))
                (remaining-expressions (cdr array-expressions) (cdr remaining-expressions))
                (next-entry (fill-pointer *nanocontrol-array*) (1+ next-entry)))
               ((endp remaining-expressions)
                (cl:progn
                  (setf (third current-expression) 0)
                  (when *debug-defnano*
                    (format t "~%debug-defnano: final output: ~s~%" current-expression))
                  (let ((posn (vector-push-extend current-expression *nanocontrol-array*)))
                    (or first-instruction posn))))

           ;; we know there must be more expressions else we'd be in the
           ;; return part of the do loop. So check to see if they match
           ;; something already in the control store so we can compress
           ;; things by just jumping there (thanks Knuth ;-)
           (mlet (matchcdr posncdr) (match-into-nanocontrol (cdr array-expressions))
             (cl:cond
               (matchcdr
                (setf (third current-expression) posncdr) ; let that implement
                                                          ; the rest of this
                                                          ; nano instruction
                (when *debug-defnano* 
                  (format t "~%debug-defnano: matchcdr return: ~s~%" current-expression))
                (return-from done (or first-instruction (vector-push-extend current-expression *nanocontrol-array*))))
               (t
                (setf (third current-expression) (1+ next-entry))
                (let ((new-posn (vector-push-extend current-expression *nanocontrol-array*)))
                  (unless first-instruction
                    (setq first-instruction new-posn)))
                (when *debug-defnano* 
                  (format t "~%debug-defnano: adding: ~s~%" current-expression)))))))))))) ; go loop

  
(defmacro defnano ((name &key force-add) &body instructions)
  "note this macro expands based on the current state of *nanocontrol-array*
which is probably not the best approach (instead we should probably move the
lookup of already existing entries to load-time instead of
macro-expansion-time). If force-add is true, we create a new nanocontrol entry
for the form even if it would otherwise have been a duplicate so we can
distinguish the uses"
  
  (let* ((array-expressions (gensym))
         (array-expressions-actual (parse-defnano instructions)))
    ;; each array expression, above, is in the form of (<pad-bit-spec>
    ;; <register-bit-spec> <control-bit-spec>) (where the control-bit-spec and
    ;; register-bit-spec can be considered as combined as in the paper)

    ;; so our job here is to add the array expressions to the control array 'as
    ;; we go', but also check for already existing array expressions as we can
    ;; just make those our next state instead of duplicating states. Note that
    ;; the array-expressions won't have filled in the next-state field, so we
    ;; also do that here (generally some offset to the next empty expression
    ;; and then sequential unless we get a dup) Note that duplicate detection
    ;; needs to check that the following states are also dupes. Next-state 0
    ;; means we're done with a nanoinstruction and we can return control to the
    ;; microinstruction level.

    ;; add freeze state to all but last expression this should keep the
    ;; microcontroller from running while the subsequent nano operations can
    ;; run

    (mapc #'(lambda (expression)
              (setf (first expression) (logior (first expression) +pad-run-nano+)))
          (butlast array-expressions-actual))

    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (export-ulisp-symbol ',name)
       (let ((,array-expressions ',array-expressions-actual)) ; capture the expansion
         (note-if *debug-defnano* ";;debug-defnano: defining ~A" ',name)
         
         (update-alist ',name
                       (update-nanocontrol ,array-expressions
                                           ,force-add)
                       *nanocontrol-symtab*)))))


;; now define the nanocontroller (emulator), that gets called to
;; interpret a expression of microcode (just as the microcode gets called to
;; interpret the objective S-code).

;; collect-ncode is probably only needed for debugging and console support...
(defun collect-ncode (offset)
  "Return a list of pairs of the offsets and the nanocodes starting from
a given offset. Since nanocode doesn't handle conditionals, it's just a
linked list."
  (let* ((result nil)
         (current-code (elt *nanocontrol-array* offset))
         (next-state (third current-code)))
    (cl:cond
      ((not current-code)
       (warn "Offset ~d does not represent a valid nanocode!" offset))
      (t
       (push (cons offset current-code) result)
       (while (plusp next-state) ; terminates with a 0
         (setq current-code (elt *nanocontrol-array* next-state))
         (assert current-code ()
                 "Invalid next state in position ~D of nanocontrol-array" next-state)
         (push (cons next-state current-code) result)
         (setq next-state (third current-code)))
       (nreverse result)))))

;;; finalize the nanocontrol-array
;;
;; the nanocontrol array is intially a triple of pad-spec control-spec
;; next-state, and we know the maximum next-state is one less than the
;; fill-pointer (since that points to the next array element to be
;; filled), *nanocontrol-pad-spec* contains the associations between
;; the bits in the pad control field and the varibles that represent
;; pads, and *nanocontrol-wire-next* has the next bit to be assigned
;; if there were a new (unique) register control to be established. So
;; that lets us determine the size of each of these fields if
;; represented as bit vectors, and thus the total size for a "expression" in
;; *nanocontrol-array-bitvectors*

(defun check-for-duplicate-nanocontrol-definitions ()
  (let ((dups (duplicates-p *nanocontrol-symtab* :key #'cdr)))
    (when (not (null dups))
      (setq dups (delete-duplicates dups :key #'cdr)) ; in case we have multiple hits
      ;; now for each hit, find all the matches and print them
      (mapc #'(lambda (hit)
                (warn "Duplicates found in nanocontrol symtab!
;; We will not be able to distinguish between nanooperations: ~S"
                      (mapcan #'(lambda (entry)
                                  (if (eql (cdr hit) (cdr entry))
                                      (list (car entry))
                                      nil))
                              *nanocontrol-symtab*)))
            dups)
      (warn ";; if all entries are expected to be distinct, look for the
      force-add option on defnano"))))
  
(defun finalize-nanocontrol-array ()
  "We've loaded all of the relevant defnano forms, so now we just need
  to convert into an array of bitvectors that are suitable for FPGA usage"
  ;; quick sanity check
  (check-for-duplicate-nanocontrol-definitions)
  ;; make sure we don't blow our pc representation
  (assert (<= (fill-pointer *nanocontrol-array*) *nano-pc-max-address*) ()
          "Nanocontrol is too large to be addressed by *nano-pc*. Increase
          *nano-pc-size* in machine-defs.lisp and recompile simulator.")
  
  ;; (tbd) - need to turn into a binary array for download to FPGA
  
  (values)) ; for the moment we will work with the unfinalized forms as it's
            ; easier to debug!
