(in-package :scheme-79)

(scheme-79-version-reporter "S79 Dev Support" 0 4 1
                            "Time-stamp: <2022-04-12 16:58:55 gorbag>"
                            "move (reset) to console directory")

;; 0.4.1   4/12/22 move reset, power-on-reset to console directory

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.4.0   3/18/22 snapping a line: 0.4 release of scheme-79 supports test-0 thru test-3. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun reset ()
  "Same as setting the reset-line on the chip for a clock cycle"
  (note-if (or *debug-microcontroller* *debug-external-pads*) "Setting *reset* pad")
  (set-pad 'scheme-mach:*reset*))

(defun power-on-reset ()
  (scheme-mach:single-step t) ; prep UI
  (reset) ; send reset signal
  (scheme-mach:single-step t)) ; allow it to be processed
