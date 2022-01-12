(in-package :s-code)

(scheme-79:scheme-79-version-reporter "Scheme Machine S-Code" 0 3 0
                                      "Time-stamp: <2022-01-11 15:17:03 gorbag>"
                                      "0.3 release!")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; SEE AIM 559 pp 3-4

;; global variable references translate into a special pointer which
;; points at the global value of the symbol. A local variable
;; reference is transformed into an instruction containing a slexical
;; address of its value in the environment structure. Constants are
;; transformed into instrucitons which move the appropriate constant
;; into the accumulated value. Procedure calls are convered from
;; prefix to postfix notations. Conditions and control sequences are
;; performed somewhat differently than the source language

