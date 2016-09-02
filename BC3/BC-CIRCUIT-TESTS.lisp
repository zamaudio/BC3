
(in-package :bc)

#|
;;; Baxandall tone control experiments

(defparameter P1 100d3)
(defparameter P2 500d3)
(defparameter R1 10d3)
(defparameter R2 1d3)
(defparameter R3 5d3)
(defparameter C1 47d-9)
(defparameter C2 470d-9)
(defparameter C3 820d-12)
(defparameter C4 8.2d-9)

(defparameter potc1 (/ 1.0d0 11.0d0))
(defparameter potc2 (/ 1.0d0 11.0d0))

; (defparameter potc1 (.var (/ 1.0 11.0d0)))
; (defparameter potc2 (.var (/ 1.0 11.0d0)))

; (defparameter potc1 0.999)
; (defparameter potc2 0.999)


(defpatch koe ((bax (.baxandall
                     :potc1 potc1 :p1 P1
                     :potc2 potc2 :p2 P2
                     :r1 R1 :c1 C1
                     :r2 R2 :c2 C2
                     :r3 R3
                     :c3 C3 :c4 C4))
               (e (.E (.imp1 1.0d0) 50.0d0))
               (rl (.R 500d3)))
  (.par rl (port bax 1))
  (.par e (port bax 0))
  (-> (.voltage rl) (.coeff 11.0d0) (.probe "out")))

; (inspect koe)
; (c-code koe)
; (load-patch koe)
; (step-patch-n koe 44100 t)
; (at (out (find-block koe "out")))
; (defparameter s (probe-response koe :samples 10))

(matlab-response koe ;;; export model to Matlab/Octave
                 :samples 4000
                 :outputs '("out")
                 :post "
figure(10);
%plot_sig(out_resp,SRATE); grid on; 
%plot(out_resp); grid on; 
plot_logspect(out_resp,SRATE); grid on; 
%axis([0 0.0005 -1 1]);
")

(data-to-matlab ;;; Simulate in BC and export the result only
 `(("SRATE" ,(srate koe))
   ("RESP" ,(probe-response koe))))

;;; Now you can evaluate in Matlab/Octave

plot_logspect(RESP,SRATE); axis([20 20000 -20 20]); grid on;

(response-to-matlab

|#

