
;;; ==========================================
;;; BlockCompiler demo files
;;; Delay line made of unit delay lines
;;: Otherwise same as model DL1
;;; Matti Karjalainen, 05.02.2008
;;; ==========================================

(in-package :BC)

;;; THIS MAY BE EXTREMELY SLOW IN CREATING THE PACH ON SOME PLATFORMS !!!

(defpatch DL1b ((src (.E 1.0 0.1))   ;;; 1V 1 Ohm
                (dx (.dline-1 10.0)) ;;; furst delay
                (r (.R 100.0)))      ;;; 100 Ohm
  (.par (port dx 1) r)               ;;; parallel
  (loop for i from 1 below 10        ;;; indexing
        for di = (.dline-1 10.0)     ;;; new delay
        do (.par (port dx 0) (port di 1)) ; parallel
        do (setq dx di)              ;;; di -> dx
        finally
        (.par (port di 0) src))      ;;; parallel
  (-> (.voltage r) (.probe "out")))  ;;; R voltage


(matlab-response DL1b
                 :samples 441
                 :outputs '("out")
                 :post "
figure(10); clf; subplot(3,1,1)
plot_sig(out_resp,SRATE); grid on; 
%axis([0 0.0005 -1 1]);
xlabel('Time [s]');
ylabel('V_R [V]');
")

;;; End of file
