
;;; ========================================
;;; BlockCompiler demo files
;;; Delay processor with delay modulation
;;; Requires sound I/O devices
;;; Matti Karjalainen, 05.02.2008
;;; ========================================

(in-package :BC)

(defpatch delay ((modf (.var 4.0))  ;;; modulator frequency
                 (moda (.var 0.0))  ;;; modulator depth
                 (msin (.sin-osc :freq modf :ampl moda))
                 (dval (.var 0.1))  ;;; nominal delay
                 (dx (.mul (.add msin 1.0) dval)) ;;; delay
                 (dblock (.delay :time dx :max-time 0.2))
                 (in (.ad))         ;;; A/D converter
                 (out (.da)))       ;;; D/A converter
  (defun set-modf (f) (setf (at modf) f))
  (defun set-moda (a) (setf (at moda) a))
  (defun set-dval (x) (setf (at dval) x))
  (-> in dblock (inputs out))) ;;; signal path

; (c-code delay)

#|
;;; Start streaming:
(run-patch delay)

;;; Stop streaming:
(stop-patch delay)

;;; Set modulation amplitude
(set-moda 0.3)

;;; Set nominal delay
(set-dval 0.03)
|#
