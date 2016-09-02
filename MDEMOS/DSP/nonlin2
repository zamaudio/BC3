
;;; ===============================
;;; BlockCompiler demo files
;;; Smooth nonlinearity (tanh)
;;; applied to a sinewave of
;;; linearly increasing amplitude
;;; Matti Karjalainen, 04.02.2008
;;; ===============================

(in-package :BC)


(defpatch nonlin2 ((src (.ramp 150.0))
                   (sin (.sin-osc :freq 1000.0 :ampl src)))
  (-> sin (.tanh) (.probe "out")))


(matlab-response nonlin2 ;;; To Matlab
                 :samples (* 2 441)
                 :outputs '("out")
                 :post "
figure(10); clf; subplot(2,1,1);
plot_sig(out_resp,SRATE); grid on; 
axis([0 0.02 -1.1 1.1]);
xlabel('Time [s]');
ylabel('Amplitude');
title('Saturating nonlinearity');")
