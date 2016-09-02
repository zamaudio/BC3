
;;; ========================================
;;; BlockCompiler demo files
;;; Real-time modulated sine wave generation
;;; FM synthesis works a bit this way
;;; Matti Karjalainen, 05.02.2008
;;; ========================================

(in-package :BC)


(defpatch fm ((modf (.var 300.0))    ;;; modulator frequency
              (moda (.var 0.95))     ;;; modulator amplitude
              (carrf (.var 500.0))   ;;; carrier frequency
              (carra (.var 0.50))    ;;; carrier amplitude
              (msin (.sin-osc :freq modf :ampl moda))
              (csin (.sin-osc :ampl carra))
              (out (inputs (.da))))  ;;; D/A converter
  (defun set-modf (f) (setf (at modf) f))   ;;; set mod freq
  (defun set-moda (a) (setf (at moda) a))   ;;; set mod ampl
  (defun set-carrf (f) (setf (at carrf) f)) ;;; set carr freq
  (-> (.mul (.add msin 1.0) carrf) (param csin 'freq))
  (-> csin out)) ;;; sine wave to D/A

#|
;;; Start streaming:
(run-patch fm)

;;; Stop streaming:
(stop-patch fm)

;;; Set fast modulation frequency
(set-modf 410.0)

;;; Slow modulation
(set-modf 5.0)

;;; Set modulation amplitude
(set-moda 0.3)
|#
