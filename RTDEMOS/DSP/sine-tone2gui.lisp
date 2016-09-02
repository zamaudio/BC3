
;;; ========================================
;;; BlockCompiler demo files
;;; Real-time modulated sine wave generation
;;; wit graphical user interface
;;; Matti Karjalainen, 30.06.2008
;;; ========================================

(in-package :BC)

(defpatch fm ((modf (.slider :name "Modulation frequency"
                             :init 300.0 :stop 1000.0))
              (moda (.slider :name "Modulation amplitude" 
                             :init 0.95 :stop 1.0  :format "~5,3f"))
              (carrf (.slider :name "Carrier frequency"
                              :init 500.0 :stop 1000.0))
              (carra (.slider :name "Carrier amplitude"
                              :init 0.50 :stop 1.0  :format "~5,3f"))
              (msin (.sin-osc :freq modf :ampl moda))
              (csin (.sin-osc :ampl carra))
              (out (inputs (.da))))  ;;; D/A converter
  (defun set-modf (f) (setf (at modf) f))   ;;; set mod freq
  (defun set-moda (a) (setf (at moda) a))   ;;; set mod ampl
  (defun set-carrf (f) (setf (at carrf) f)) ;;; set carr freq
  (-> (.mul (.add msin 1.0) carrf) (param csin 'freq))
  (-> csin out) ;;; sine wave to D/A
  (patch-controls (sliders modf moda carrf carra)))

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
