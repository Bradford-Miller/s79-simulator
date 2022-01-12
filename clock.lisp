(in-package :scheme-mach)

(scheme-79:scheme-79-version-reporter "Scheme Machine Clock" 0 3 0
                                      "Time-stamp: <2022-01-11 15:09:00 gorbag>"
                                      "0.3 release!")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.1.7 11/24/21 use *note-output* instead of *error-output*, note-banner

;; 0.1.6  9/28/21 def run-until-tick to let us rapidly step forward in
;;                   the microcode while debugging

;; 0.1.5  9/20/21 move clock support init lists to fpga-clocked

;; 0.1.4  9/16/21 move core phase fns and variables to support

;; 0.1.3  9/14/21 move *tick* defvar to support

;; 0.1.2  8/22/21 differentiate microcode 'cond' from cl:cond

;; 0.1.0  8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.6  8-18-21 add run & stop fns

;; 0.0.5  8- 9-21 link in reset for diagnostics if needed

;; 0.0.4  7-27-21 since we created the neutral1 and neutral2 init lists, we
;;                       have to actually run them :-)

;; 0.0.3  7- 6-21 move init-list-name here from predefs, and add
;;                       neutral lists (to deal with valid-for and clear-pad)

;; 0.0.2  7- 2-21 get-end-phase now returns two values, the second
;;                       indicates the end phase is the NEXT clock
;;                       tick, not the current one if the phase would
;;                       obtain before the tick as well.

;; 0.0.1  6-26-21 Consolodate clock fns into their own file

;; clock is technically an INPUT to the chip: two phase clock (ph1,
;; ph2). However both for purposes of simulation and our eventual FPGA
;; implementation we can and will generate the clock on-chip but
;; present the clock to the pins for external circuitry to use if
;; needed.
;;
;; what we want to do is trigger actions based on that clock so we
;; simulate what the machine would do, e.g. when single stepping, etc.

;; ph1, ph2 (two phase clock, non-overlapping); during ph1 data
;;   manipulated per current microcode state and computes new microcode
;;   state specified by MICRO and NANO PLAs. chip-bus valid by the END
;;   of ph1. External data being read should be valid soon after the
;;   beginning of ph1 so any microcode branch will have time to
;;   propagate through both PLAs. During ph2 the chip transitions to
;;   the new state and the control outputs change accordingly.

;; so for each of the two clocks, we set up initialization lists we
;; can trigger on clock transitions. Set up lists for ph1 rising,
;; falling and same for ph2.

;; We will generate this clock internally based on the step/run
;; switches on the console. Since each will trigger a series of
;; function calls it may not be very similar to an actual hardware
;; clock (i.e. phase1 rising processes will run to completion before
;; phase1 falling is triggered, etc.) this won't be very useful for
;; debugging any glitches or timing issues but in hardware most if not
;; all of these initialization list processes would occur
;; simultaneously anyway (with some propegation delay for intermediate
;; results).

;; note that most (all?) of the hardware simulations occurs as a side
;; effect of calling update-clock! However, it will return after each
;; minor "tick" and there are 8 minor "ticks" to a full clock cycle
;; (major clock ticks are tracked by *tick*). So for "single stepping"
;; we want to call this 8 times, but we can micro-step by calling it
;; once!

;; clock support

(eval-when (:compile-toplevel :load-toplevel :execute)
  (configure-clock :biphase-nonoverlapping :1GHz))

;; once the clock is configured, we should be able to use a generic
;; function to do most of this work and just specialize
;; project-specific differences via, e.g. :around and :after methods
;; or other hooks? (TBD)

(let ((clock-status 0))
  (defun init-clock (&optional reset-major-p)
    (when reset-major-p
      (setq *tick* 0))
    (setq clock-status 0)
    
    ;; if DSO is present, reset it too
    #+capi
    (reset-dso)
    ;; similarly for diagnostic metrics internal state. This does NOT
    ;; reset diagnostic history, just how we determine which
    ;; instructions have been successfully executed!
    #+capi
    (s79-console:reset-uinstruction-metrics)) 

  (defun get-clock-status ()
    clock-status)
  
  (defun update-clock ()
    ;; step through clock states (FSM) and call/reset initializations
    ;; as appropriate

    ;; See figures 8, 9 in AIM 559
    ;; (S; no-clock)0 -> (ph1-rising)1 -> (ph1-high)2 -> (ph1-falling)3 
    ;; -> (no-clock)4 -> (ph2-rising)5 -> (ph2-high)6 -> (ph2-falling)7
    ;; -> S
    (initializations '*all-ph-pre-update-list* t)

    (when (zerop (setq clock-status (mod (1+ clock-status) *total-clock-phases*)))
      (incf *tick*))
    (setq *symbolic-clock-phase* (nth clock-status *phase-names*)) ; should set to the upcoming phase
    (when *debug-timing*
      (terpri *note-output*)
      (note "Start of phase ~A~%~%" *symbolic-clock-phase*))
    
    (case clock-status
      (0 ; assert ph1 and call *ph1-rising-list*
       (when *debug-timing*
         (terpri *note-output*)
         (note-banner (list (format nil "Start of tick ~d" *tick*)) 4)
         (terpri *note-output*))
       (setq *ph1-clock-state* t) ; internal version
       (set-pad '*ph1*)           ; external version
       (reset-initializations '*neutral2-list*)
       (initializations '*ph1-rising-list*))
      (1 ; neutral (ph1-high)
       (initializations '*ph1-high-list*)
       (reset-initializations '*ph1-rising-list*))
      (2 ; deassert ph1 and call *ph1-falling-list*
       (setq *ph1-clock-state* nil)
       ; (clear-pad '*ph1*) should already be on ph1-falling-list thanks to set-pad...
       (reset-initializations '*ph1-high-list*)
       (initializations '*ph1-falling-list*))
      (3 ; neutral (no clock)
       (reset-initializations '*ph1-falling-list*)
       (initializations '*neutral1-list*))
      (4 ; assert ph2 and call *ph2-rising-list*
       (setq *ph2-clock-state* t)
       (set-pad '*ph2*)
       (initializations '*ph2-rising-list*)
       (reset-initializations '*neutral1-list*))
      (5 ; neutral (ph2-high)
       (initializations '*ph2-high-list*)
       (reset-initializations '*ph2-rising-list*))
      (6 ; deassert ph2 and call *ph2-falling-list*
       (setq *ph2-clock-state* nil)
       ; (clear-pad '*ph2*) ; should already be on ph2-falling-list thanks to set-pad...
       (reset-initializations '*ph2-high-list*)
       (initializations '*ph2-falling-list*))
      (7 ; neutral (no clock; return to state 0)
       (initializations '*neutral2-list*)
       (reset-initializations '*ph2-falling-list*)))
    (initializations '*all-ph-list* t) ; some inits get run on every micro-clock tick
    ))

(defun single-step (&optional finish-cycle-p)
  "Calls the micro clock 8 times to go through a complete ph1/ph2
cycle. Note if we've already micro-stepped the clock this will NOT end
at the beginning of a cycle. To do that set finish-cycle-p"

  ;; we use this in the console, but it's not realistic - the original
  ;; chip would have left the clock run and used *freeze*.  Generally
  ;; the clock in a digital circuit doesn't stop (except possibly
  ;; during RESET)!

  (let ((cycle-length *total-clock-phases*)) ; see update-clock
    (when finish-cycle-p
      (setq cycle-length (- *total-clock-phases* (get-clock-status))))
       
    (dotimes (i cycle-length)
      (declare (ignorable i))
      (update-clock))))

(let ((running nil))
  (defun run ()
    (set-running-p t)
    (while running
      (update-clock)))

  (defun run-until-tick (tick)
    (set-running-p t)
    (while (and running (< *tick* tick))
      (update-clock))
    (stop))

  (defun running-p ()
    running)

  (defun set-running-p (val)
    (setq running val))
  
  (defun stop ()
    (set-running-p nil)))



