
;;; ===============================
;;; BlockCompiler demo files
;;; Sine wave generation
;;; Matti Karjalainen, 31.01.2008
;;; ===============================

(in-package :BC)


(defpatch sinosc2 ((osc (.sin-osc :freq 1000.0 :ampl 1.0))
                   (out (.probe "out")))
  (-> osc out))   ;;; sine wave 1 kHz amplitude 1.0 to "out"


(matlab-result sinosc2 ;;; To Matlab
               :samples 441
               :outputs '("out")
               :post "
figure(10); clf; subplot(2,1,1);
plot_sig(out_resp,SRATE); grid on; 
axis([0 0.01 -1.1 1.1]);
xlabel('Time [s]');
ylabel('Amplitude');")
