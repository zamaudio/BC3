
(in-package :BC)

#| SOTKUA ????

;;; Biquad with constant coeffs

(defclass .bq-c (macro-block) ())

(defmethod initialize-macro-block ((b .bq-c) &key
                                   bs as (b0 1.0) (b1 0.0) (b2 0.0)
                                   (a0 1.0) (a1 0.0) (a2 0.0)
                                   &aux in out d1 d2)
  (let* ((lena (length as))
         (lenb (length bs)))
    (when bs (setq b0 (elt bs 0)))
    (when (and bs (> lenb 1)) (setq b1 (elt bs 1)))
    (when (and bs (> lenb 2)) (setq b2 (elt bs 2)))
    (when (> lenb 3) (error "Too many bs for biquad"))
    (when as (setq a0 (elt as 0)))
    (when (and as (> lena 1)) (setq a1 (elt as 1)))
    (when (and as (> lena 2)) (setq a2 (elt as 2)))
    (when (> lena 3) (error "Too many as for biquad"))
    (setq b0 (/ b0 a0) b1 (/ b1 a0) b2 (/ b2 a0))
    (setq a2 (/ a2 a0) a1 (/ a1 a0) a0 1.0)
    (when (and (= b1 0.0) (= b2 0.0) (= a1 0.0) (= a2 0.0))
      (let* ((c (.coeff (/ b0 a0))))
        (setf (inputs b) (list (in c))
              (outputs b) (list (out c)))
        (return-from initialize-macro-block nil)))
    (cond ((and (= a1 0.0) (= a2 0.0)) (setq in (.x.)))
          ((= a2 0.0) (setq in (.add :inputs 2))
           (setq d1 (.d)) (-> in d1)
           (-> d1 (.coeff (- a1)) (in in 1)))
          (t (setq in (.add :inputs 3))
             (setq d1 (.d) d2 (.d)) (-> in d1 d2)
             (-> d1 (.coeff (- a1)) (in in 1))
             (-> d2 (.coeff (- a2)) (in in 2))))
    (cond ((and (= b1 0.0) (= b2 0.0))
           (setq out (.coeff b0)) (-> in out))
          )
    
    (setf (inputs b) (list (in in))
          (outputs b) (list (out out)))))
          
  

(defparameter bq (make-instance '.bq-c))
  
|#

#|
(iir-map-to-reflectance '(3.0 2.0) '(1.0 0.5))
(iir-map-to-reflectance '(3.0 2.0 1.0) '(1.0 0.5 0.25 0.125))
(iir-map-to-reflectance '(1.0) '(1.0 0.5 0.25 0.125))
(iir-map-to-reflectance '(3.0 2.0 1.0 0.5) '(1.0))
|#

; (defparameter wa (.Zratio* '(3.0 2.0) '(1.0 0.5)))
; (port wa)

#|
(defpatch koe ((z (.Zratio* '(3.0 2.0) '(1.0 0.5)))
               (e (.e (.imp1 1000.0) 1000.0)))
  (.par e z)
  (-> (.voltage z) (.probe "out")))

(defpatch koe ((z (.Zratio* '(-2.0 0.0) '(1.0 0.0)))
               (e (.e (.imp1 1000.0) 1000.0)))
  (.par e z)
  (-> (.voltage z) (.probe "out")))

(defpatch koe ((z (.Zratio* '(1.0 0.0) '(1.0 0.0 0.99)))
               (e (.e (.imp1 1000.0) 1000.0)))
  (.par e z)
  (-> (.voltage z) (.probe "out")))

(load-patch koe)
(step-patch koe)

(to-matlab koe)

(defparameter dbs
  '(  -0.544609506967119
   0.055082494360094
   0.517071622358111
   ))

(defparameter das
 '(   1.000000000000000
   2.124417849769416
   0.826932111804406
  ))


(defparameter dbs
  '(-0.812917070006370
  -1.152438050236672
   0.869550453984502
   1.109043772030484))

(defparameter das
 '(1.000000000000000
   4.731908614918329
   5.384208276107525
   1.428639108357226
  -0.101392507481632
   0.062334606529651))


(defparameter dbs
  '( -1.220894137042612
     -10.358722359692125
     -23.752575141300728
     -12.260330370983313
     18.209393705279478
     22.614988179487725
     6.768966317657909))

(defparameter das
  (mapcar #'(lambda (x) (* 100.0 x))
          '(0.010000000000000
            0.161902145149767
            0.720111599449425
            1.378563749832628
            1.270438473998181
            0.534343600366040
            0.070523535455984
            -0.002977370766447
            0.000572229095329
            -0.000133072415288
            0.000056195023958)))
|#

#|
(defpatch koe ((z (.Zratio* dbs das))
               (e (.e (.imp1 1000.0) 1000.0)))
  (.par e z)
  (-> (.voltage z) (.probe "out")))

(load-patch koe)
(step-patch koe)

(to-matlab koe)

(matlab-response koe ;;; To Matlab
                 :samples 4410
                 :outputs '("out")
                 :post "
figure(10); clf; subplot(2,1,1);
plot_sig(out_resp,SRATE); grid on; 
% axis([0 0.01 -1.1 1.1]);
xlabel('Time [s]');
ylabel('Amplitude');
subplot(2,1,2);
plot_spect(out_resp,SRATE); grid on;
xlabel('Frequency [s]');
ylabel('Magnitude');")


(defpatch koe ((rs 1.0)
               (r (.R -0.99))
               (c (.C 0.0005))
               (e (.E (.imp1 rs)
                      rs))
               (ser (.ser r c)))
  (.par ser e)
  (-> (.voltage ser) (.probe "out")))

|#

#|
(bq-map-to-reflectance '(1.0 2.0) '(20 10) nil nil '(100 50) '(-1 -2) 1.0)

(defparameter y (.Y2* :b0 '(1.0 2.0) :b1 '(20 10) :a1 '(100 50) :a2 '(-1 -2) :fir 1.0))

(defparameter cc (make-list 13 :initial-element 0.1))
(defparameter cc (make-list 20 :initial-element 0.1))
(defparameter cc (make-list 180 :initial-element 0.1))

(defpatch modp ((f (.bq-vc1 :b0 cc :b1 cc))
                (src (.imp1)))
  (-> src f (.probe "out")))

(defpatch modp ((f (.bq-vc1 :b0 cc :b1 cc :a1 cc :a2 cc))
                (src (.imp1)))
  (-> src f (.probe "out")))

(load-patch modp)
(step-patch-n modp 441000 t)


(defparameter bqx1 (.bq-vc1 :b0 '(2.0 3.0)))

(defparameter bqx (.bq-vc :b0 '(2.0 3.0)))
(defparameter bqx (.bq-vc :b0 '(2.0 3.0) :b1 '(1.0 0.0) :b2 '(1.0 -1.0)))
(defparameter bqx (.bq-vc :b0 '(2.0 3.0) :a1 '(1.0 0.0) :a2 '(1.0 -1.0)))
(defparameter bqx (.bq-vc :b0 '(2.0 3.0) :a0 '(2.0 1.0) :a1 '(1.0 0.0) :a2 '(1.0 -1.0)))

(defpatch bqp ((bq (.bq-vc :a0 '(1.0 1.0)
                           :a1 '(0.0 0.0)
                           :a2 '(0.9 0.9)
                           :b0 '(2.0 4.0)
                           :b1 '(0.0 0.0)
                           ))
               (src (.imp1 '(1.0 1.0))))
  (-> src bq (.probe "out")))

(load-patch bqp)
(step-patch bqp)

(matlab-response bqp ;;; To Matlab
                 :samples 441
                 :outputs '("out")
                 :post "
figure(10); clf; subplot(2,1,1);
plot_sig(out_resp,SRATE); grid on; 
xlabel('Time [s]');
ylabel('Amplitude');
subplot(2,1,2);
plot_spect(out_resp,SRATE); grid on;
xlabel('Frequency [s]');
ylabel('Magnitude');")


(defpatch bqp ((bq (.bq-vc1 :b0 '(2.0 3.0) :a1 '(-0.5 0.5) :a2 '(0.9 0.9)))
               (src (.imp1 1.0)))
  (-> src bq (.probe "out")))
|#
