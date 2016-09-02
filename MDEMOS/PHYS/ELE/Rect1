
;;; ================================
;;; BlockCompiler demo files
;;; Rectifier made by an ideal diode
;;; Matti Karjalainen, 05.12.2006
;;; ================================

(in-package :BC)


(defpatch Rect1 ((src (.sin-osc :freq 50.0))
                 (e (.E src 20.0)) ;;; Ri = 20 Ohm
                 (d (.diode))
                 (r (.R 1.0e3)))
  (.root d (.ser e (.par r (.C 2.0e-4))))
  (-> (.voltage r) (.neg) (.probe "UR"))
  (-> (.current d) (.neg) (.probe "IE")))


(matlab-response Rect1
                 :samples 8820
                 :outputs '("UR" "IE")
                 :post "
figure(10); clf; subplot(2,1,1)
plot_sig(UR_resp,SRATE); grid on; 
xlabel('Time [s]');
ylabel('V_R [V]');
title('Load resistor voltage');
subplot(2,1,2);
plot_sig(1000*IE_resp,SRATE); grid on; 
xlabel('Time [s]');
ylabel('I_D [mA]');
axis([0 0.2 -5 35]);
title('Diode current');")

;;; End of file
