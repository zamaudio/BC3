
;;; ===========================================
;;; BlockCompiler demo files
;;; Charging of a capacitor from voltage source
;;; Matti Karjalainen, 05.02.2008
;;; ===========================================

(in-package :BC)


(defpatch RC1 ((c (.C 2e-6))       ;;; C 2 uF
               (e (.E 1.0 1e3)))   ;;; 1V 1 kOhm
  (.par e c)                       ;;; parallel
  (-> (.voltage c) (.probe "CV"))  ;;; C voltage
  (-> (.current c) (.probe "CI"))) ;;; C current


(matlab-response RC1
                 :samples 441
                 :outputs '("CV" "CI")
                 :post "
figure(10); clf; subplot(2,2,1)
plot_sig(CV_resp,SRATE); grid on; 
xlabel('Time [s]');
ylabel('V_C [V]');
title('Capacitor voltage');
subplot(2,2,2);
plot_sig(1000*CI_resp,SRATE); grid on;
xlabel('Time [s]');
ylabel('I_C [mA]');
title('Capacitor current');")

;;; End of file
