
(in-package :bc)


;;; Making of basic circuit elements

;;; Laplace domain inductance

(defun _Ls (val)
  (when (or (symbolp val) (stringp val))
    (setq val (_param val)))
  (_mul val (_tvar "s")))


;;; Bilinear inductance

(defun _L (val &aux (srate *srate*))
  (when (or (symbolp val) (stringp val))
    (setq val (_param val)))
  (when (or (symbolp srate) (stringp srate))
    (setq srate (_param srate)))
  (let* ((c (_mul 2 srate val))
         (one 1) (z (_tvar "z"))
         (num (_sub one (_pow z -1)))
         (den (_add one (_pow z -1))))
    (_mul c (_div num den))))

;;; Simple inductance 

(defun _L- (val &aux (srate *srate*))
  (when (or (symbolp val) (stringp val))
    (setq val (_param val)))
  (when (or (symbolp srate) (stringp srate))
    (setq srate (_param srate)))
  (let* ((c (_mul srate val))
         (num (_mul c (_sub 1 (_pow (_tvar "z") -1)))))
    num))

;;; Magnitude optimized inductance 

(defun _L* (val &aux (srate *srate*))
  (when (or (symbolp val) (stringp val))
    (setq val (_param val)))
  (when (or (symbolp srate) (stringp srate))
    (setq srate (_param srate)))
  (let* ((one 1) (coeff 0.15) (z (_tvar "z"))
         (c (_mul (_mul (+ 1 coeff) srate) val))
         (num (_sub one (_pow z -1)))
         (den (_add coeff (_pow z -1))))
    (_mul c (_div num den))))

;;; Laplace domain capacitance

(defun _Cs (val)
  (when (or (symbolp val) (stringp val))
    (setq val (_param val)))
  (_inv (_mul val (_tvar "s"))))

;;; Bilinear capacitance

(defun _C (val &aux (srate *srate*))
  (when (or (symbolp val) (stringp val))
    (setq val (_param val)))
  (when (or (symbolp srate) (stringp srate))
    (setq srate (_param srate)))
  (let* ((c (_mul (_mul 2 srate) val))
         (one 1) (z (_tvar "z"))
         (num (_add one (_pow z -1)))
         (den (_sub one (_pow z -1))))
    (_mul (_div 1 c) (_div num den))))

;;; Simple capacitance

(defun _C- (val &aux (srate *srate*))
  (when (or (symbolp val) (stringp val))
    (setq val (_param val)))
  (when (or (symbolp srate) (stringp srate))
    (setq srate (_param srate)))
  (let* ((c (_mul srate val))
         (one 1) (z (_tvar "z"))
         (den (_sub one (_pow z -1))))
    (_mul (_div 1 c) (_div one den))))

;;; Magnitude optimized capacitance 

(defun _C* (val &aux (srate *srate*))
  (when (or (symbolp val) (stringp val))
    (setq val (_param val)))
  (when (or (symbolp srate) (stringp srate))
    (setq srate (_param srate)))
  (let* ((one 1) (coeff 0.15) (z (_tvar "z"))
         (c (_mul (_mul (+ 1 coeff) srate) val))
         (den (_sub one (_pow z -1)))
         (num (_add coeff (_pow z -1))))
    (_mul (_div one c) (_div num den))))

;;; Resistance

(defun _R (val) (_val val))


;;; CONNECTIONS

;;; Impedances in series

;;; series, no simplification
(defun _ser- (&rest forms)
  (apply #'_add forms))

;;; series, with simplification
(defun _ser (&rest forms)
  (_simpz (apply #'_add forms)))


;;; Impedances in parallel

;;; parallel, no simplification
(defun _par- (&rest forms)
  (let* ((ys (mapcar #'_inv forms))
         (ysum (apply #'_add ys)))
    (_inv ysum)))

;;; parallel, with simplification
(defun _par (&rest forms)
  (_simpz (apply #'_par- forms)))


;;; Y-to-Delta impedance transform

;;; Y2D, no simplification
(defun _Y2D- (z1 z2 z3)
  (let* ((z12 (_add (_make-ratio (_add z1 z2))
                    (_make-ratio (_div (_mul z1 z2) z3))))
         (z23 (_add (_make-ratio (_add z2 z3))
                    (_make-ratio (_div (_mul z2 z3) z1))))
         (z31 (_add (_make-ratio (_add z3 z1))
                    (_make-ratio (_div (_mul z3 z1) z2)))))
    (list z12 z23 z31)))

;;; Y2D, with simplification
(defun _Y2D (z1 z2 z3)
  (let* ((z12 (_add (_make-ratio (_add z1 z2))
                    (_make-ratio (_div (_mul z1 z2) z3))))
         (z23 (_add (_make-ratio (_add z2 z3))
                    (_make-ratio (_div (_mul z2 z3) z1))))
         (z31 (_add (_make-ratio (_add z3 z1))
                    (_make-ratio (_div (_mul z3 z1) z2)))))
    (list (_simpz z12) (_simpz z23) (_simpz z31))))

#|
(defun _Y2D (z1 z2 z3)
  (let* ((z12 (to-latex (_add (_make-ratio (_add z1 z2))
                    (_make-ratio (_div (_mul z1 z2) z3)))))
         (z23 (_add (_make-ratio (_add z2 z3))
                    (_make-ratio (_div (_mul z2 z3) z1))))
         (z31 (_add (_make-ratio (_add z3 z1))
                    (_make-ratio (_div (_mul z3 z1) z2)))))
    (list (_simpz z12) (_simpz z23) (_simpz z31))))

(defun _Y2D- (z1 z2 z3)
  (let* ((z12 (_add z1 z2 (_div (_mul z1 z2) z3)))
         (z23 (_add z2 z3 (_div (_mul z2 z3) z1)))
         (z31 (_add z3 z1 (_div (_mul z3 z1) z2))))
    (list z12 z23 z31)))

(defun _Y2D (z1 z2 z3)
  (let* ((z12 (_add z1 z2 (_simpz (_div (_mul z1 z2) z3))))
         (z23 (_add z2 z3 (_simpz (_div (_mul z2 z3) z1))))
         (z31 (_add z3 z1 (_simpz (_div (_mul z3 z1) z2)))))
    (list (_simpz z12) (_simpz z23) (_simpz z31))))
|#


;;; Delta-to-Y impedance transform

;;; D2Y, no simplification
(defun _D2Y- (z12 z23 z31)
  (let* ((sum (_add z12 z23 z31))
         (z1 (_div (_mul z12 z31) sum))
         (z2 (_div (_mul z12 z23) sum))
         (z3 (_div (_mul z23 z31) sum)))
    (list z1 z2 z3)))

;;; D2Y, with simplification
(defun _D2Y (z12 z23 z31)
  (let* ((sum (_add z12 z23 z31))
         (z1 (_div (_mul z12 z31) sum))
         (z2 (_div (_mul z12 z23) sum))
         (z3 (_div (_mul z23 z31) sum)))
    (list (_simpz z1) (_simpz z2) (_simpz z3))))


;;; Collect filter coeffs (z-domain)

(defmethod collect-zfiltcoeffs ((item t))
  (error "Cannot collect filter coefficient of ~a" item))

(defmethod collect-zfiltcoeffs ((item _elem))
  (list (%collect-zfiltcoeffs item)))

(defmethod collect-zfiltcoeffs ((item _mul))
  (list (%collect-zfiltcoeffs item)))

(defmethod collect-zfiltcoeffs ((item _add))
  (loop for x in (forms item)
        collect (%collect-zfiltcoeffs x)))
  

(defmethod %collect-zfiltcoeffs ((item t))
  (error "Cannot collect filter coefficient of ~a" item))

(defmethod %collect-zfiltcoeffs ((item _elem))
  (list item 0))

(defmethod %collect-zfiltcoeffs ((item _pow))
  (let* ((f1 (first (forms item)))
         (f2 (second (forms item))) exp)
    (unless (and (= (length (forms item)) 2)
                 (_eql f1 (_tvar "z"))
                 (typep (setq exp f2) '_const))
      (error "Cannot collect filter coefficient of ~a" item))
    (list (_const 1) (- (value exp)))))
                 
(defmethod %collect-zfiltcoeffs ((item _mul))
  (let* ((f1 (first (forms item)))
         (f2 (second (forms item))) exp)
    (unless (and (= (length (forms item)) 2)
                 (typep f1 '_elem)
                 (typep f2 '_pow)
                 (_eql (first (forms f2)) (_tvar "z"))
                 (typep (setq exp (second (forms f2))) '_const))
      (error "Cannot collect filter coefficient of ~a" item))
    (list f1 (- (value exp)))))

#|
(collect-zfiltcoeffs (num (_serz (_R "R") (_C "C"))))
(inspect (collect-zfiltcoeffs (num (_simpz (_C "C")))))
(inspect (collect-zfiltcoeffs (den (_simpz (_C "C")))))
(inspect (num (_simpz (_C "C"))))
|#


#|
;;; One-port impedance block

;;; Scalar impedance, resistor implementation
(defmethod .Z ((arg _elem))
  (let* ((val (value arg)))
    (cond ((typep val 'basic-block)
           (value arg))
          (t (.R val)))))

;;; Polynomial expression implementation



(defclass .Zratio (macro-block one-port-block) ()
  (:default-initargs
    :domain 'electric))

;;; Rational expression implementation
(defmethod .Z ((arg _div))
  (unless (_eql (ivar arg) (_tvar "z"))
    (setq arg (_simpz arg))
    (unless (_eql (ivar arg) (_tvar "z"))
      (error "Invalid form ~a to .Z" arg)))
  (let* ((n (collect-zfiltcoeffs (num arg)))
         (d (collect-zfiltcoeffs (den arg)))
         (o 0) b0 a0 1/b0 1/a0 rp gp)
    (unless (find 0 n :key #'second)
      (error "Port impedance 0 for Z = ~a" arg))
    (loop for x in (append n d) ;;; max coeff length
          for y = (second x)
          do (when (> y o) (setq o y)))
    (setq b0 (first (find 0 n :key #'second))
          a0 (first (find 0 d :key #'second))
          1/b0 (_inv b0) 1/a0 (_inv a0)
          rp (_mul b0 1/a0) gp (_mul a0 1/b0))
    (list n d o)))
|#

#|
(.Z (_R 1.0))
(.Z (_par (_C 0.001) (_R 1.0)))
(inspect (_param nil (.R 1.0)))
|#


;;; .TF transfer function, z-domain
;;; from symbolic form

;;; .TFC = controller for .TF

(defclass .TFC (macro-group)
  ((num :initform nil :accessor num)
   (den :initform nil :accessor den)))

(defmethod initialize-macro-block ((b .TFC) &key form)
  (unless (typep form '_form)
    (error "~a is invalid as a form to .TFC" form))
  (unless (_eql (ivar form) (_tvar "z"))
    (setq form (_simpz form))
    (unless (_eql (ivar form) (_tvar "z"))
      (error "Invalid form ~a to .TF" form)))
  (let* ((n (collect-zfiltcoeffs (num form)))
         (d (when (den form) (collect-zfiltcoeffs (den form))))
         (o 0) a0 b0 as bs)
    (loop for x in (append n d) ;;; max coeff length
          for y = (second x)
          do (when (> y o) (setq o y)))
    (setq b0 (first (find 0 n :key #'second))
          a0 (first (find 0 d :key #'second)))
    (unless (or a0 (not (den form)))
      (error "Zero denominator ~a for .TFC" form))
    (push (when b0 (.bc b0)) bs) ;;; b0
    (if (or (not a0) (_eql a0 (_const 1)))
      (push nil as)
        (push (.bc (_inv a0)) as))
    (loop for i from 1 to o
          for ni = (first (find i n :key #'second))
          for di = (first (find i d :key #'second))
          do (push (when ni (.bc ni)) bs)
          do (push (when di (.bc di)) as))
    (setf (num b) (reverse bs)
          (den b) (reverse as))
    nil))

(defun .TFC (form &rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.TFC :form form rest))



#|
(let* ((x (_div (_zpoly 1 0 2) ;;; %%%%
                (_zpoly 2 0 1 1.5))))
  (inspect (.TFC x)))

(let* ((x (_div (_zpoly 1 0 2) ;;; %%%%
                (_zpoly 2 0 1 1.5))))
  (inspect (.TFC x :mrate 1/44)))

(let* ((x (_div (_add 2 (_mul 3 (_pow (_tvar "z") -1)))
                (_add 1))))
  (inspect (.TFC x)))

(let* ((x (_div (_add 2 )
                (_add 2 (_mul 4 (_pow (_tvar "z") -1))))))
  (inspect (.TFC x)))
|#

;;; .TF transfer fuction

(defclass .TF (macro-block) ())

(defmethod initialize-macro-block ((b .TF) &key form)
  (unless (typep form '.tfc) (setq form (.tfc form)))
  (let* ((n (num form))
         (d (den form))
         (len (length d))
         zs ns ds in ini ino out)
    (loop with zp = nil
          for i from 1 below len
          for z = (.d)
          do (when zp (-> z zp))
          do (push z zs)
          do (setq zp z))
    (setq in (.x.))
    (if (first d)
      (setq ino (.mul in (first d)))
      (setq ino in))
    (-> ino (first zs))
    (when (first n)
      (push (.mul (first n) ino) ns))
    (loop for i from 1 below len
          for nx = (nth i n)
          for dx = (nth i d)
          for zx = (nth (1- i) zs)
          do (when nx (push (.mul nx zx) ns))
          do (when dx (push (.mul dx zx) ds)))
    (if (= (length ns) 1)
      (setq out (first ns))
      (setq out (apply #'.add ns)))
    (cond ((= (length ds) 0) (setq ini in))
          ((= (length ds) 1) (setq ini (.sub))
           (-> (first ds) (in ini 1) in))
          (t (setq ini (.sub))
             (-> (apply #'.add ds) (in ini 1) in)))
    (setf (outputs b) (list (out out)))
    (setf (inputs b) (list (in ini))))
  nil)
    
(defun .TF (form &rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.TF :form form rest))


;;; .Z consolidated impedance, z-domain
;;; from symbolic form

;;; .ZC = controller for .Z

(defclass .ZC (macro-group)
  ((num :initform nil :accessor num)
   (den :initform nil :accessor den)
   (1/b0 :initform nil :accessor 1/b0)
   (1/2b0 :initform nil :accessor 1/2b0)
   (1/a0 :initform nil :accessor 1/a0)
   (rp :initform nil :accessor rp)
   (gp :initform nil :accessor gp)))

(defmethod initialize-macro-block ((b .ZC) &key form)
  (unless (typep form '_form)
    (error "~a is invalid as a form to .ZC" form))
  (unless (_eql (ivar form) (_tvar "z"))
    (setq form (_simpz form))
    (unless (_eql (ivar form) (_tvar "z"))
      (error "Invalid form ~a to .Z" form)))
  (let* ((n (collect-zfiltcoeffs (num form)))
         (d (when (den form) (collect-zfiltcoeffs (den form))))
         (o 0) a0 b0 1/b0 1/a0 as bs)
    (loop for x in (append n d) ;;; max coeff length
          for y = (second x)
          do (when (> y o) (setq o y)))
    (setq b0 (first (find 0 n :key #'second))
          a0 (first (find 0 d :key #'second)))
    (unless (or a0 (not (den form)))
      (error "Zero denominator ~a for .ZC" form))
    (push (when b0 (.bc b0)) bs) ;;; b0
    
    (if (or (not a0) (_eql a0 (_const 1)))
      (push nil as)
        (push (.bc (_inv a0)) as))
    
    (loop for i from 1 to o
          for ni = (first (find i n :key #'second))
          for di = (first (find i d :key #'second))
          do (push (when ni (.bc ni)) bs)
          do (push (when di (.bc di)) as))
    (setf (num b) (reverse bs)
          (den b) (reverse as))
    nil))

(defun .ZC (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.ZC :form rest))


;;; .Z consolidated impedance

(defclass .Z (macro-group) ())


(defun .Z (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.Z :form rest))

#|
(_val

(inspect (_zpoly 2 0 3))

(defpatch koe ((x (_div (_zpoly 1 0 2) ;;; %%%%
                        (_zpoly 2 0 1 1.5)))
               (filt (.TF x)) (out (.probe "out")))
  (-> (.imp1 1.0) filt out)
  (defun x () (at out)))

(defpatch koe ((c0 0) ;;; %%%%
               (c1 (.var 1.0 'a))
               (x (_div (_zpoly c1 c0 (.var 2.0 'b))
                        (_zpoly 2 c0 c1 (.var 1.5 'd))))
               (filt (.TF x)) (out (.probe "out")))
  (-> (.imp1 1.0) filt out)
  (defun x () (at out)))

(defpatch koe ((x (_div (_zpoly 1 4 2) ;;; WWWWWWWWWWWWWWWWWW
                        (_zpoly 2.0)))
               (filt (.TF x)) (out (.probe "out")))
  (-> (.imp1 1.0) filt out)
  (defun x () (at out)))

(defpatch koe ((x (_div (_zpoly 1 4 2) ;;; WWWWWWWWWWWWWWWWWW
                        (_zpoly (.var 2.0))))
               (filt (.TF x)) (out (.probe "out")))
  (-> (.imp1 1.0) filt out)
  (defun x () (at out)))

(defpatch koe ((x (_div (_zpoly 1 4 2) ;;; WWWWWWWWWWWWWWWWWW
                        (_zpoly (_add 1 (.var 2.0)))))
               (filt (.TF x)) (out (.probe "out")))
  (-> (.imp1 1.0) filt out)
  (defun x () (at out)))

(defpatch koe ((x (_div (_zpoly (.var 1)) ;;; %%%%
                        (_zpoly 2 0 1 1.5)))
               (filt (.TF x)) (out (.probe "out")))
  (-> (.imp1 1.0) filt out)
  (defun x () (at out)))

(c-code koe)
(load-patch koe)
(inspect koe)

(loop for i from 0 below 7
      do (print (x))
      do (step-patch koe))

(let* ((x (_div (_zpoly 2 3) (_zpoly 1 4))))
  (inspect (.TF x :mrate 1/44)))

(let* ((x (_div (_zpoly 2 3)
                (_zpoly 1))))
  (inspect (.TF x)))

(let* ((x (_div (_add 2 )
                (_add 2 (_mul 4 (_pow (_tvar "z") -1))))))
  (inspect (.TF x)))

(_simpz (_div (_add 2 'a)
              (_add 3 'b)))

(_simpz (_div (_add 2)
              (_add 3 'b)))

(_simpz (_div (_add 2 'a)
              (_add 'b)))

(_collect-term (_const 2) (_tvar "z"))

(defmethod link ((arg t)) arg)

(inspect (_var 'a 2))
|#


(provide :BC-circuit)
