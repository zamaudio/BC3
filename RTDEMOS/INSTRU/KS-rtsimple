
;;; ===========================================
;;; BlockCompiler demo files
;;; Simple realtime Karplus-Strong string model
;;; Matti Karjalainen, 10.02.2008
;;; ===========================================

(in-package :BC)

(defparameter *KS-table*
  '(0.1 0.2 0.3 0.4 0.5 0.4 0.3 0.2 0.1))

(defpatch KS-realtime ((tr (.trig-data *KS-table*))
                       (adder (.add)))
  (defun pluck () (trig tr))
  (-> tr adder (.fir :coeffs '(0.498 0.498))
      (.delay :length 200) (in adder 1) (inputs (.da))))

#| ;;; operations:
(run-patch KS-realtime)
(stop-patch KS-realtime)
(pluck)
|#

;;; EOF
