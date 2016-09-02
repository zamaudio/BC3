
;;; ===============================================
;;; BlockCompiler demo files
;;; Delay line as a macro block of unit delay lines
;;: Otherwise same as model DL1
;;; Matti Karjalainen, 05.02.2008
;;; ===============================================

(in-package :BC)

(def-macro-block .dline-nx (&key z delay-length)
  (let* ((d0 (.dline-1 z)))
    (loop with dx = d0
          for i from 1 below delay-length
          for di = (.dline-1 z)
          do (.pair (port di 1) (port dx 0))
          do (setq dx di)
          finally
          (set-ports (port dx 0) (port d0 1)))))

(defpatch DL1d ((src (.E 1.0 0.1))   ;;; 1V, 0.1 Ohm
                (dl (.dline-nx :z 10.0 :delay-length 10))
                (r (.R 100.0)))      ;;; 100 Ohm
  (.par src (port dl 0))             ;;; parallel
  (.par (port dl 1) r)               ;;; parallel
  (-> (.voltage r) (.probe "out")))  ;;; R voltage


(matlab-response DL1d
                 :samples 441
                 :outputs '("out")
                 :post "
figure(11); clf; subplot(2,1,1)
plot_sig(out_resp,SRATE); grid on; 
xlabel('Time [s]');
ylabel('V_R [V]');
")

;;; End of file
