
;;; ===============================
;;; BlockCompiler demo files
;;; Real-time sine wave output
;;; Matti Karjalainen, 05.02.2008
;;; ===============================

(in-package :BC)


(defpatch sine-tone ((freq (.var 1000.0)) ;;; frequency
                     (ampl (.var 0.5)) ;;; amplitude
                     (sin (.sin-osc :freq freq :ampl ampl))
                     (out (.da))) ;;; D/A converter
  (-> sin (inputs out))) ;;; sine wave to D/A

#|
;;; Start streaming:
(run-patch sine-tone)

;;; Stop streaming:
(stop-patch sine-tone)
|#
