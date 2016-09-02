
;;; ==================================
;;; BlockCompiler demo files
;;; Simple Karplus-Strong string model
;;; Matti Karjalainen, 04.02.2008
;;; ==================================

(in-package :BC)


(defpatch KS-simple ((add (.add)))
  (-> (.imp1) add (.fir :coeffs '(0.5 0.5)) 
      (.dn 200) (in add 1) (.probe "out")))


(matlab-response KS-simple
                 :samples 44100
                 :outputs '("out")
                 :post "
figure(10); clf; subplot(2,1,1);
plot_sig(out_resp,SRATE); grid on; 
axis([0 0.2 -0.1 1.1]);
xlabel('Time [s]');
ylabel('Amplitude');
title('KS string time response');
%
subplot(2,1,2);
plot_spect(out_resp,SRATE); grid on;
axis([0 5000 -10 50]);
xlabel('Frequency [Hz]');
ylabel('Level [dB]');
title('KS string magnitude response');
%
sound(out_resp,SRATE);")


;;; EOF
