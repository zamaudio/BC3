
;;; ===============================
;;; BlockCompiler demo files
;;; Simple 1st order lowpass filter
;;; Matti Karjalainen, 04.02.2008
;;; ===============================

(in-package :BC)


(defpatch LPF1 ((adder (.add)) ;;; adder
                (k 0.995))     ;;; coefficient
  (-> (.imp1 1.0) (.coeff (- 1.0 k)) adder)
  (-> adder (.d) (.coeff k) (in adder 1))
  (-> adder (.probe "out")))


(matlab-response LPF1 ;;; to Matlab
                 :samples 4000
                 :outputs '("out")
                 :post "
figure(10); clf; subplot(2,2,1);
plot_sig(out_resp,SRATE); grid on; 
axis([0 0.02 -0.0003 0.006]);
xlabel('Time [s]');
ylabel('Amplitude');
title('Lowpass impulse response');
subplot(2,2,2);
plot_logspect(out_resp,SRATE); grid on;
axis([10 10000 -50 5]);
xlabel('Frequency [Hz]');
ylabel('Level [dB]');
title('Lowpass magnitude response');")

;;; EOF
