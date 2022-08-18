(in-package :scheme-mach)

(scheme-79:scheme-79-version-reporter "Scheme Machine Nano" 0 4 1
                                      "Time-stamp: <2022-05-26 11:26:18 Bradford W. Miller(on Boromir)>"
                                      "repatriation")

;; 0.4.1   4/ 7/22 move PLA support code (building the nanoPLA) to plas/ncode-assembler.
;;                    move wire defs to machine-wire

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.4.0   3/18/22 snapping a line: 0.4 release of scheme-79 supports test-0 thru test-3. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.3.6   2/ 9/22 way too many things (fns, variables) with "line" in their name
;;                    and it's ambiguous.  Splitting so "line" refers to,
;;                    e.g. an output (log) line, "expression" refers to a
;;                    'line' of code (single expression in nano or microcode
;;                    land typically, and because we used (READ) it wasn't
;;                    confined to a single input line anyway) and "wire" to
;;                    refer to, e.g., a control or sense 'line' on a register.

;; 0.3.5   1/25/22 add support for from-decremented-frame-exp and
;;                     from-decremented-displacement-exp

;; 0.3.4   1/24/22 add support for setting and clearing interrupt mask

;; xxxxx   1/19/22 remove some TBDs in the comments (they were done already)

;; 0.3.3   1/18/22 cleanup obsolete code: removing special treatment of
;;                    registers which required multiple control wires
;;                    for TO as new covering set computation deals
;;                    with it.

;; 0.3.2   1/14/22 flip order of symbols in nanocontrol constants for
;;                    consistancy with AIM

;; 0.3.1   1/13/22 change references from internal-freeze to run-nano
;;                    for consistancy with AIM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.12  1/ 7/22 use new get-pad-defn-value function

;; 0.1.11  1/ 6/22 add methods for linking functions to support library

;; 0.1.10 12/15/21 use export-ulisp-symbol

;; 0.1.9  12/13/21 import/export nanoop names into *ulang-pkg* instead
;;                     of *scheme-mach*

;; 0.1.8  10/25/21 use pla-read instead of aref on plas

;; 0.1.7  10/21/21 nano-pc now a bit vector add 'to-micro-pc' into
;;                     parse-defnano table (really should make this
;;                     more automatic from the macros! (TBD)

;; 0.1.6  10/20/21 micro-pc now a bit vector

;; 0.1.5  10/ 5/21 since we don't have actual continuous update until a
;;                     latch operation, some actions have to
;;                     be rerun on phase changes or other events to
;;                     correctly reflect their state. In this case we
;;                     add a run-sense-controls immediately after
;;                     run-register-controls.

;; 0.1.4  10/ 1/21 add +rr-from-const*+ to distinguish from from-type-const field
;;                 Also extend defnano to allow forcing a nanocontrol to be added
;;                     even if an apparent duplicate

;; 0.1.3   9/ 7/21 if +rr-from-to*+ is set, use the FROM anaphor table
;;                    instead of the TO anaphor table to decode.
;;                    (support added to code generation, disassembly,
;;                    and a parameter set up in s79-nanocode)

;; 0.1.2   8/24/21 differentiate microcode 'cond' from cl:cond, if from
;;                  cl:if

;; 0.1.1   8/21/21 +rr-from-type-const*+ - handle type constants

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.32     8-17-21 use note-if

;; 0.0.31     8-16-21 from-to*

;; 0.0.30     8-11-21 cosmetic/clarify code

;; 0.0.29     8- 3-21 cosmetic

;; 0.0.28     7-31-21 schedule sense simulation explicitly here; rerun control
;;                      simulation before p2 so we capture any pad sync changes

;; 0.0.27     7-19-21 move register copy from machine-predefs.lisp since that's the
;;                      related timing; change to :ph2-high; use variable from
;;                      clock-triggers.lisp

;; 0.0.26     7- 1-21 Better debug message for deferred actions

;; 0.0.25     6-26-21 Consolodate debug-flags to scheme-79-defs.lisp

;; 0.0.24     6-14-21 add additional debug messages to nanocontroller

;; 0.0.23     5-29-21 set-pad should set up clear-pad, not the
;;                         nanocontroller (that should ensure proper
;;                         minimum duration

;; 0.0.22     4-26-21 fix split - dangling variable reference

;; 0.0.21     4-21-21 split run-nanocontroller into two parts, so we can
;;                         run on rising and falling clock (at least
;;                         for now, seems bad practice in general)

;; 0.0.20     4-20-21 make sure we funcall the compiled setter fix
;;                         debug-nanocontroller print statement format
;;                         and add one for when the nano-pc changes

;; 0.0.19     3-29-21 exec-nano-expression: compile the setter so it's
;;                         acceptable to funcall

;; 0.0.18     3-15-21 modify run-nanocontroller so only a single cycle is
;;                         run during :ph1-rising, then it waits (if
;;                         there is more) until next :ph1-rising

;; 0.0.17     3- 8-21 debug-nanocontroller

;; 0.0.16     3- 7-21 rename translate-*-anaphor functions to
;;                         *-code-to-anaphor to make clearer?

;; 0.0.15     3- 2-21   define finalize-nanocontrol-array
;; 0.0.14     2-27-21   check for register alias when looking up anaphor
;; 0.0.13     2-25-21   add retpc-count-mark forms to defnano
;; 0.0.12     2-24-21   export defnano symbols
;; 0.0.11     2-23-21   pad-conditional (pseudo pad)
;; 0.0.10     2-19-21   cl:progn (differentiate from our ucode version)
;; 0.0.9      2-16-21   finish adding register specs to parse-defnano
;; 0.0.8      2-14-21   run-nanocode - handle multi-nanocode ops
;; 0.0.7      2- 7-21   lookup from and to anaphors
;; 0.0.6      2- 1-21   first cut at nanocontroller simulator
;; 0.0.5      1-22-21   move defnano forms to own file (s79-nanocode.lisp)
;; 0.0.4      1-20-21   rewrite defnano to do most of the work at load-time
;; 0.0.3      1-19-21   Add first of defnano forms
;; 0.0.2      1-17-21   Set freeze (via pad) for multi-step nano instructions
;; 0.0.1      1-16-21   First cut defnano
;; 0.0.0      1-14-21   new

;; Closer to a hardware simulator corresponding with AIM 559
;; based on the published microcode and description

;; here we implement the various described mechanisms on the chip
;; which we are simulating.


;;; code to run the nanocontroller simulation

(defvar *nanocontroller-deferred* nil
  "List of things to do before we start the next nanocycle")

(defun defer-nanocode (fn description)
  "Set the fn to be deferred until the start of the next nanocycle"
  (push (list fn description) *nanocontroller-deferred*))

(defun run-deferred-nanocode ()
  (mapc #'(lambda (x)
            (note-if *debug-nanocontroller*
                     "debug-nanocontroller: running deferred action: ~s" (cadr x))
            (funcall (car x))
            (note-if *debug-nanocontroller*
                     "debug-nanocontroller: post-deferred action; from-incremented: ~s"
                     (register-from-incremented-p '*newcell*)))
        *nanocontroller-deferred*) ; generally turn off control wires from prior cycle
  (setq *nanocontroller-deferred* nil)) ; clear it

(defvar *nanocontroller-ran-p* nil
  "A flag to tell the microcontroller that the nanocontroller in fact has run")

(defun run-nanocontroller-p1 ()
  "Given a new nanocontroller state, run until we reach state 0 (quiescence)
which would return control to the microcontroller state machine."
  ;; decode the state. Right now it's a triple of integers, eventually
  ;; it should be a bit vector with fields mimicking what we would have
  ;; in the hardware implementation.

  ;; microcode right now is 4 integers (eventually a single bit-vector
  ;; with fields) (see machine-micro.lisp):
  
  ;; ustate: <next-ustate> <nano-operation> <from> <to>

  (let ((microcode-expression (pla-read *microcontrol-array* *micro-pc*)))
    ;; RIGHT now using quadruples instead of bitvectors (TBD)
    (destructuring-bind (next-ucode nop from to) microcode-expression
      (declare (ignore next-ucode))

      (setq *nanocontroller-ran-p* t) ; so microcontroller can modify the upc
                                      ; when it runs next cycle

      ;; do we already have a nano-pc?
      (unless (plusp (bit-vector->integer *nano-pc*))
        (note-if *debug-nanocontroller*
                 "debug-nanocontroller: setting nano-pc to #o~o" nop)
        (copy-field (integer->bit-vector nop) *nano-pc*)) ; (tbd) should be stored as a bit vector
      (note-if *debug-nanocontroller*
               "*** executing nanoinstruction ~a at #o~o (F: #o~o T: #o~o) ***"
               (car (rassoc (bit-vector->integer *nano-pc*)
                            *nanocontrol-symtab*))
               (bit-vector->integer *nano-pc*)
               from to)
      (exec-nano-expression
       (elt *nanocontrol-array* (bit-vector->integer *nano-pc*))
       from
       to))))

(defun run-nanocontroller-p2 ()
  "Second part of run-nanocontroller - clear out the control wires on
registers and update to the next nanoinstruction if needed"

  (run-deferred-nanocode)

  ;; now set up next nano-instruction

  (destructuring-bind (pad-controls register-controls next-state)
      (elt *nanocontrol-array* (bit-vector->integer *nano-pc*))
    (declare (ignore pad-controls register-controls))
    (note-if *debug-nanocontroller*
             "debug-nanocontroller-p2: setting nano-pc for next cycle to: #o~o"
             next-state)
  
    (copy-field (integer->bit-vector next-state) *nano-pc*))) ; (tbd) should be stored as a bit vector

(defvar *memoization-fns* nil)
(defvar *cmemoization-fns* nil)

;; this will need to be transformed into a hardware PLA based approach...
;; but for simulation it's fine (TBD)
(defun exec-nano-expression (controller-entry from to)
  ;; lookup the state in the control table
  ;; again, using triples at this point.
  (destructuring-bind (pad-controls register-controls next-state) controller-entry
    (declare (ignore next-state))

    (let ((decoded-register-controls (decode-nanocontrol-registers controller-entry)))
      (declare (ignore decoded-register-controls)) ; for debugging
      ;; execute the current nanocontroller entry

      ;; treat from* and to* special as they are anaphors for using the
      ;; reference in the microcode
      (when (and (plusp (logand register-controls +rr-from*+))
                 (plusp from)) ; if zero, then no control set so will be from
                               ; the bus
        ;; do we have a saved version?
        (let* ((anaphor (from-code-to-anaphor from))
               (key (list 'from anaphor))
               (setter (or (cdr (assoc key *memoization-fns* :test #'equal))
                           (and (update-alist key
                                              (set-control-wire-fn
                                               'from
                                               anaphor)
                                              *memoization-fns*
                                              :test #'equal)
                                (cdr (assoc key *memoization-fns*
                                            :test #'equal)))))
               (csetter (or (cdr (assoc key *cmemoization-fns*
                                        :test #'equal))
                            (and (update-alist key
                                               (compile nil setter)
                                               *cmemoization-fns*
                                               :test #'equal)
                                 (cdr (assoc key *cmemoization-fns*
                                             :test #'equal))))))
          (note-if *debug-nanocontroller*
                   "debug-nanocontroller: setting control wire FROM for ~a"
                   anaphor)
          (funcall csetter t)
          (defer-nanocode #'(lambda () (funcall csetter nil))
              (format nil "Clear FROM control wire for ~a" anaphor))))

      (when (and (plusp (logand register-controls +rr-to*+))
                 (plusp to))
        (let* ((anaphor (to-code-to-anaphor to))
               (key (list 'to anaphor))
               (setter (or (cdr (assoc key *memoization-fns* :test #'equal))
                           (and (update-alist key
                                              (set-control-wire-fn 'to anaphor)
                                              *memoization-fns*
                                              :test #'equal)
                                (cdr (assoc key *memoization-fns*
                                            :test #'equal)))))
               (csetter (or (cdr (assoc key *cmemoization-fns*
                                        :test #'equal))
                            (and (update-alist key
                                               (compile nil setter)
                                               *cmemoization-fns*
                                               :test #'equal)
                                 (cdr (assoc key *cmemoization-fns*
                                             :test #'equal))))))
          (note-if *debug-nanocontroller*
                   "debug-nanocontroller: setting control wire TO for ~a"
                   anaphor)
          (funcall csetter t)
          (defer-nanocode #'(lambda () (funcall csetter nil))
              (format nil "Clear TO control wire for ~a" anaphor))))

      ;; 9-7-21 modified to use the FROM anaphor table, with support in code generation!
      (when (and (plusp (logand register-controls +rr-from-to*+))
                 (plusp to))
        ;; hack for dealing with the (rare?) case where we use the TO
        ;; field in the microcode as a FROM address (e.g. for ALE)
        (let* ((anaphor (from-code-to-anaphor to))
               (key (list 'from-to anaphor))
               (setter (or (cdr (assoc key *memoization-fns* :test #'equal))
                           (and (update-alist key
                                              (set-control-wire-fn 'from anaphor)
                                              *memoization-fns*
                                              :test #'equal)
                                (cdr (assoc key *memoization-fns*
                                            :test #'equal)))))
               (csetter (or (cdr (assoc key *cmemoization-fns*
                                        :test #'equal))
                            (and (update-alist key
                                               (compile nil setter)
                                               *cmemoization-fns*
                                               :test #'equal)
                                 (cdr (assoc key *cmemoization-fns*
                                             :test #'equal))))))
          (note-if *debug-nanocontroller*
                   "debug-nanocontroller: setting control wire FROM (via TO field) for ~a"
                   anaphor)
          (funcall csetter t)
          (defer-nanocode #'(lambda () (funcall csetter nil))
              (format nil "Clear FROM control wire (via TO field) for ~a" anaphor))))

      ;; handle from*-type 10/21/21
      (when (and (plusp (logand register-controls +rr-from*-type+))
                 (plusp from))
        ;; the type field of the register named the FROM field should be placed onto the bus
        (let* ((anaphor (from-code-to-anaphor from))
               (key (list 'from-type anaphor))
               (setter (or (cdr (assoc key *memoization-fns* :test #'equal))
                           (and (update-alist key
                                              (set-control-wire-fn 'from-type anaphor)
                                              *memoization-fns*
                                              :test #'equal)
                                (cdr (assoc key *memoization-fns*
                                            :test #'equal)))))
               (csetter (or (cdr (assoc key *cmemoization-fns*
                                        :test #'equal))
                            (and (update-alist key
                                               (compile nil setter)
                                               *cmemoization-fns*
                                               :test #'equal)
                                 (cdr (assoc key *cmemoization-fns*
                                             :test #'equal))))))
          (note-if *debug-nanocontroller*
                   "debug-nanocontroller: setting control wire FROM-TYPE for
                   ~a (csetter ~s)" anaphor csetter)
          (funcall csetter t)
          (defer-nanocode #'(lambda () (funcall csetter nil))
              (format nil "Clear FROM-TYPE control wire for ~a (csetter ~s)"
              anaphor csetter))))
      
      ;; ok, now based on the other bits, get the specs and set the control
      ;; bits on the appropriate registers
      (mapc #'(lambda (entry)
                (when (plusp (logand (car entry) register-controls))
                  (let* ((setter (cadr (assoc :setter (cdr entry))))
                         ;; force it into a form suitable for funcall (rather than eval)
                         (csetter (or (cdr (assoc :csetter (cdr entry)))
                                      (and (update-alist :csetter
                                                         (compile nil setter)
                                                         (cdr entry))
                                           (cdr (assoc :csetter (cdr entry)))))))
                    (note-if *debug-nanocontroller*
                             "debug-nanocontroller: setting control wire ~a for ~a (csetter: ~s)"
                             (cadr (assoc :control (cdr entry)))
                             (cadr (assoc :register (cdr entry)))
                             csetter)
                    (funcall csetter t)
                    ;; clear the register bits before the next nanocycle starts
                    (defer-nanocode #'(lambda () (funcall csetter nil))
                        (format nil "Clear control wire ~a for ~a (csetter: ~s)"
                                (cadr (assoc :control (cdr entry)))
                                (cadr (assoc :register (cdr entry)))
                                csetter)))))
            *nanocontrol-to-spec*)
      ;; now set pads. Presumably nanocode runs during :ph2? So some
      ;; things might have to be delayed until ph1 (which is also
      ;; when the register control wires get checked)
      (mapc #'(lambda (entry)
                (let* ((pad-bit (car entry))
                       (pad-name (cadr (assoc :name (cdr entry))))
                       (pad-clears-latch-p (cadr (assoc :clear-latch (cdr entry))))
                       (pad-type (get-pad-defn-value pad-name :type)))
                  (cl:cond
                    ((and pad-clears-latch-p
                          (plusp (logand pad-bit pad-controls)))
                     (note-if *debug-nanocontroller*
                              "debug-nanocontroller: clearing pad ~a" pad-name)
                     (clear-pad pad-name))
                    ((plusp (logand pad-bit pad-controls))
                     (note-if *debug-nanocontroller*
                              "debug-nanocontroller: setting pad ~a" pad-name)
                     (set-pad pad-name))
                    ((equal pad-type :latched-io) ; e.g. *run-nano*
                     ;; explicitly clear it since not setting it... but only if
                     ;; we set it.
                     (cl:if (and (equal pad-name '*run-nano*) ; yes we set it,
                                                              ; (OK this is
                                                              ; currently the
                                                              ; only one)
                              (test-pad-immediate pad-name)) ; and it's
                                                             ; actually set
                         (clear-pad pad-name))))))
            *nanocontrol-pad-spec*))))

;; link running the nanocontroller into the clock cycle
(defun run-register-controls ()
  (note-if *debug-dataflow* "Running register data flow simulation")                   
  (initializations '*from-controls*)
  (initializations '*to-controls*)
  (reset-initializations '*from-controls*)
  (reset-initializations '*to-controls*))

(defun run-sense-controls ()
  (note "Running register sense simulation")
  (initializations '*sense-controls*)
  (reset-initializations '*sense-controls*))

(execute-during-clock ("run-nanocontroller part 1" *run-nanocontroller-p1*)
  (run-nanocontroller-p1))

;; not ideal - want everything on rising clocks?
(execute-during-clock ("run-nanocontroller part 2" *run-nanocontroller-p2*) 
  (run-register-controls) ; rerun in case changes happened during external pad
                          ; sync
  (run-nanocontroller-p2))

;; now we have to add an initialization to run the from and to controls the
;; first time
(execute-during-clock ("Run control inits" *run-register-controls*)
                      (progn (run-register-controls)
                             (run-sense-controls))) ; after register controls
                                                    ; run, run sense controls

(execute-during-clock ("Run sense inits" *update-sense-wires*)
                      ;; rerun sense controls (really would be continuous until
                      ;; we latch, so this is really the "final run" for a
                      ;; particular tick
                      (run-sense-controls)) 
