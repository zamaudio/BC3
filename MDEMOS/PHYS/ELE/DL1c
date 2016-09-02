
;;; ==========================================
;;; BlockCompiler demo files
;;; Delay line made of unit delay lines
;;: Otherwise same as model DL1
;;; Matti Karjalainen, 05.02.2008
;;; ==========================================

(in-package :BC)


(defpatch DL1c ((z (.var 10.0))      ;;; wave Z
                (src (.E 1.0 0.1))   ;;; 1V, 0.1 Ohm
                (dx (.dline-1 z))    ;;; first delay
                (r (.R 100.0)))      ;;; 100 Ohm
  (.par (port dx 1) r)               ;;; parallel
  (loop for i from 1 below 10        ;;; indexing
        for di = (.dline-1 z)        ;;; new delay
        do (.pair (port dx 0) (port di 1)) ;;; pairwise
        do (setq dx di)              ;;; di -> dx
        finally
        (.par src (port dx 0)))      ;;; parallel
  (-> (.voltage r) (.probe "out")))  ;;; voltage of R


(matlab-response DL1c
                 :samples 441
                 :outputs '("out")
                 :post "
figure(10); clf; subplot(2,1,1)
plot_sig(out_resp,SRATE); grid on; 
xlabel('Time [s]');
ylabel('V_R [V]');")

;;; End of file
