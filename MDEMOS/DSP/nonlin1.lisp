
;;; ===============================
;;; BlockCompiler demo files
;;; Smooth nonlinearity (tanh)
;;; Matti Karjalainen, 05.02.2008
;;; ===============================

(in-package :BC)


(defpatch nonlin1 ((src (.sin-osc :freq 1000.0 :ampl 2.5))
                   (g1 (.coeff 1.0)))
   (-> src g1 (.tanh) (.probe "out")))


(matlab-response nonlin1 ;;; To Matlab
                 :samples 441
                 :outputs '("out")
                 :post "
figure(10); clf; subplot(2,1,1);
plot_sig(out_resp,SRATE); grid on; 
xlabel('Time [s]');
ylabel('Amplitude');
")
