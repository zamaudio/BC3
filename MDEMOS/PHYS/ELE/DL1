
;;; ==========================================
;;; BlockCompiler demo files
;;; Delay line circuit response
;;; Oscillation due to impedance mismatch
;;; Matti Karjalainen, 05.12.2006
;;; ==========================================

(in-package :BC)


(defpatch DL1 ((src (.E 1.0 0.1))   ;;; 1V, 0.1 Ohm
               (dl (.dline-n 10.0 :delay-length 10))
               (r (.R 100.0)))      ;;; 100 Ohm
  (.par src (port dl 0))            ;;; parallel
  (.par (port dl 1) r)              ;;; parallel
  (-> (.voltage r) (.probe "out"))) ;;; R voltage


(matlab-response DL1
                 :samples 441
                 :outputs '("out")
                 :post "
figure(10); clf; subplot(2,1,1)
plot_sig(out_resp,SRATE); grid on; 
%axis([0 0.0005 -1 1]);
xlabel('Time [s]');
ylabel('V_R [V]');
title('Charging of a capacitor');")

;;; End of file
