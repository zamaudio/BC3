
(in-package :BC)

(defun onep (x) (= x 1.0))


;;; CONSOLIDATED IMMITTANCES

;;; CONSOLIDATED BLOCKS WITH STATIC MAPPED REFLECTANCE

;;; IIR filter coeff mapper with constant vector coeffs
;;; ---------------------------------------------------

(defun iir-map-to-reflectance (bs as)
  (unless (and (typep bs 'sequence)
               (typep as 'sequence)
               (every 'numberp bs)
               (every 'numberp as))
    (error "Args of iir-mapping not number sequences"))
  (let* ((lena (length as))
         (lenb (length bs))
         (len (max lenb lena))
         (b0 (elt bs 0))
         (a0 (elt as 0))
         (rp (/ b0 a0))
         (rp2 (* 2.0 rp))
         (rbs (make-list len :initial-element 0.0))
         (ras (make-list len :initial-element 0.0)))
    (loop for i from 0 below lenb
          do (incf (elt rbs i) (elt bs i)))
    (loop for i from 0 below lena
          do (decf (elt rbs i) (* rp (elt as i))))
    (loop for i from 0 below lenb
          do (incf (elt ras i) (elt bs i)))
    (loop for i from 0 below lena
          do (incf (elt ras i) (* rp (elt as i))))
    (loop for i from 0 below len ;;; normalize denom
          do (setf (elt rbs i) (/ (elt rbs i) rp2))
          do (setf (elt ras i) (/ (elt ras i) rp2)))
    (values rp (cdr rbs) (cons 1.0 (cdr ras)))))


;;; .Zratio*, a rational Z of constant coeffs
;;; -----------------------------------------

(defclass .Zratio* (one-port-macro) ()
  (:default-initargs
    :domain 'electric))

(defmethod initialize-macro-block ((b .Zratio*) &key as bs)
  (multiple-value-bind (rp rbs ras)
      (iir-map-to-reflectance bs as)
    (let* ((filt (.iir :bs rbs :as ras))
           (pb (make-instance '.wa-port-block))
           (p (port pb)))
      (-> (.const rp) (param pb 'z))
      (-> (.const (/ 1.0 rp)) (param pb 'y))
      (-> (out pb) filt (.d) (in pb))
      (setf (domain p) (domain b))
      (setf (ports b) (list p)))))

(defun .Zratio* (bs as)
  (make-instance '.Zratio* :bs bs :as as))


;;; MIMO Biquad filter with constant vector coeffs
;;; ----------------------------------------------
;;; (to BC-filter file ?)
  
(defclass .bq-vc (macro-block) ())

(defmethod initialize-macro-block ((b .bq-vc) &key
                                   a0 a1 a2 b0 b1 b2)
  (unless b0 (error "Coeff b0 not available for .bq-vc"))
  (let* ((dim (length b0))
         (d1 (.d)) (d2 (.d))
         (in (.sub :inputs 3))
         mid out)
    (when a0 (unless (= (length a0) dim)
               (error "a0 error in .bq-vc")))
    (cond ((or (null a0) (every #'onep a0))
           (setq mid in) (-> in d1))
          (t (setq mid (.coeff (mapcar #'/ a0)))
             (-> in mid d1)))
    (if a1 (unless (= (length a1) dim)
             (error "a1 error in .bq-vc"))
      (setq a1 (make-list dim :initial-element 0.0)))
    (-> d1 (.coeff a1) (in in 1))
    (if a2 (unless (= (length a2) dim)
             (error "a2 error in .bq-vc"))
      (setq a2 (make-list dim :initial-element 0.0)))
    (-> d1 d2 (.coeff a2) (in in 2))
    (when b1 (unless (= (length b1) dim)
               (error "b1 error in .bq-vc")))
    (when b2 (unless (= (length b2) dim)
               (error "b2 error in .bq-vc")))
    (cond ((and (not b1) (not b2))
           (setq out (.coeff b0)) (-> mid out))
          (b1 (setq out (.add :inputs 2))
              (-> mid (.coeff b0) (in out 0))
              (-> d1 (.coeff b1) (in out 1)))
          (b2 (setq out (.add :inputs 2))
              (-> mid (.coeff b0) (in out 0))
              (-> d1 (.coeff b2) (in out 1)))
          (t (setq out (.add :inputs 3))
             (-> mid (.coeff b0) (in out 0))
             (-> d1 (.coeff b1) (in out 1))
             (-> d2 (.coeff b2) (in out 2))))
    (setf (inputs b) (list (in in))
          (outputs b) (list (out out)))))

(defun .bq-vc (&rest rest)
  (apply #'make-instance '.bq-vc rest))


;;; SISO (parallel) biquad with constant vector coeffs, summed output
;;; -----------------------------------------------------------------
;;; (to BC-filter file ?)

(defclass .bq-vc1 (macro-block) ())

(defmethod initialize-macro-block ((b .bq-vc1) &rest rest &key
                                   a0 a1 a2 b0 b1 b2)
  (let* ((bqn (apply #'.bq-vc rest))
         (dim (length b0))
         (inx (.copy :fanout dim))
         (outx (.sum :out-size '(1))))
    (-> inx bqn outx)
    (setf (inputs b) (list (in inx))
          (outputs b) (list (out outx)))))

(defun .bq-vc1 (&rest rest)
  (apply #'make-instance '.bq-vc1 rest))


;;; Biquad filter coeff mapper with constant vector coeffs
;;; ------------------------------------------------------

(defun bq-map-to-reflectance (b0 b1 b2 a0 a1 a2 fir)
  (when (numberp b0) (setq b0 (list b0)))
  (when (numberp a0) (setq a0 (list a0)))
  (when (numberp a1) (setq a1 (list a1)))
  (when (numberp a2) (setq a2 (list a2)))
  (when (numberp b1) (setq b1 (list b1)))
  (when (numberp b2) (setq b2 (list b2)))
  (unless (every #'numberp b0) (error "b0 spec error"))
  (let* ((len (length b0)) rp b0x b1x)
    (when a0 (unless (and (= (length a0) len) (every #'numberp a0))
               (error "a0 spec error")))
    (cond (a1 (unless (and (= (length a1) len) (every #'numberp a1))
                (error "a1 spec error"))
              (when a0 (setq a1 (map 'list #'/ a1 a0))))
          (t (setq a1 (make-list len :initial-element 0.0))))
    (cond (a2 (unless (and (= (length a2) len) (every #'numberp a2))
                (error "a2 spec error"))
              (when a0 (setq a2 (map 'list #'/ a2 a0))))
          (t  (setq a2 (make-list len :initial-element 0.0))))
    (when a0 (setq b0 (map 'list #'/ b0 a0)))
    (cond (b1 (unless (and (= (length b1) len) (every #'numberp b1))
                (error "b1 spec error"))
              (when a0 (setq b1 (map 'list #'/ b1 a0))))
          (t (setq b1 (make-list len :initial-element 0.0))))
    (cond (b2 (unless (and (= (length b2) len) (every #'numberp b2))
                (error "b2 spec error"))
              (when a0 (setq b2 (map 'list #'/ b2 a0))))
          (t (setq b2 (make-list len :initial-element 0.0))))
    (setq rp (reduce #'+ b0))
    (when fir (incf rp fir))
    (setq b0x (map 'list #'- b1 (map 'list #'* b0 a1)))
    (setq b1x (map 'list #'- b2 (map 'list #'* b0 a2)))
    (values rp b0x b1x a1 a2)))

(defun bq-refl-filter (b0 b1 b2 a0 a1 a2 fir)
  (multiple-value-bind (rp b0 b1 a1 a2)
      (bq-map-to-reflectance b0 b1 b2 a0 a1 a2 fir)
    (values (.bq-vc1 :b0 b0 :b1 b1 :a1 a1 :a2 a2) rp)))


;;; .Zratio*, a rational Z of constant coeffs
;;; -----------------------------------------

(defclass .Y2* (one-port-macro) ()
  (:default-initargs
    :domain 'electric))

(defmethod initialize-macro-block ((b .Y2*) &key
                                   b0 b1 b2 a0 a1 a2 fir)
  (multiple-value-bind (filt rp)
      (bq-refl-filter b0 b1 b2 a0 a1 a2 fir)
    (let* ((pb (make-instance '.wa-port-block))
           (p (port pb))
           (sub (.sub)))
      (-> (.const rp) (param pb 'z))
      (-> (.const (/ 1.0 rp)) (param pb 'y))
      (-> (out pb) (.d) (in sub 0)
          (.coeff (/ rp)) filt (in pb))
      (-> filt (.d) (in sub 1))
      (setf (domain p) (domain b))
      (setf (ports b) (list p)))))

(defun .Y2* (&rest rest)
  (apply #'make-instance '.Y2* rest))


;;; ----

(provide :BC-consolid)
