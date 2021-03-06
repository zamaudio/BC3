
(in-package :BC)


;;; TRANSDUCERS

;;; Mechanic-acoustic transducer ??????

(defclass .mech-acoust (.xformer) ())

(defmethod initialize-macro-block :after ((b .mech-acoust) &key)
  (setf (domain b) '(mechanic acoustic)
        (domain (port b 0)) 'mechanic
        (domain (port b 1)) 'acoustic))

(defclass .acoust-mech (.xformer) ())

(defmethod initialize-macro-block :after ((b .acoust-mech) &key)
  (setf (domain b) '(acoustic mechanic)
        (domain (port b 0)) 'acoustic
        (domain (port b 1)) 'mechanic))

#|
(inspect (make-instance '.mech-acoust))
(inspect (make-instance '.acoust-mech))
|#


;;; Electrodynamic (electrical -> mechanical)

(defclass .el-dyn (macro-block) ())

(defmethod initialize-macro-block ((b .el-dyn) &key bl)
  (let* ((xf (.xformer/ bl))
         (g (.dualizer)))
    (connect (port xf 1) (port g 0))
    (setf (domain (port xf 0)) 'electric
          (domain (port g 1)) 'mechanic)
    (setf (ports b) (list (port xf 0) (port g 1)))
    (when (params xf)
      (setf (params b) (list (param xf))))))

(defun .el-dyn (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.el-dyn :bl (car rest) (cdr rest))
    (apply #'make-instance '.el-dyn :bl nil rest)))

(defun .x-el-dyn (obj &rest rest)
  (let* ((x (if (and (car rest) (not (keywordp (car rest))))
              (apply #'make-instance '.el-dyn :bl (car rest) (cdr rest))
              (apply #'make-instance '.el-dyn :bl nil rest))))
    (connect (port obj) (port x 0))
    (port x 1)))


;;; Electrodynamic (mechanical -> electrical)

(defclass .dyn-el (macro-block) ())

(defmethod initialize-macro-block ((b .dyn-el) &key bl)
  (let* ((xf (.xformer bl))
         (g (.dualizer)))
    (connect (port g 1) (port xf 0))
    (setf (domain (port xf 1)) 'electric
          (domain (port g 0)) 'mechanic)
    (setf (ports b) (list (port g 0) (port xf 1)))
    (when (params xf)
      (setf (params b) (list (param xf))))))

(defun .dyn-el (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.dyn-el :bl (car rest) (cdr rest))
    (apply #'make-instance '.dyn-el :bl nil rest)))

(defun .x-dyn-el (obj &rest rest)
  (let* ((x (if (and (car rest) (not (keywordp (car rest))))
              (apply #'make-instance '.dyn-el :bl (car rest) (cdr rest))
              (apply #'make-instance '.dyn-el :bl nil rest))))
    (connect (port obj) (port x 0))
    (port x 1)))

#|
(defpatch lsp ((in (.var 0.0 "in"))
               (e (.e in (.var 1.0e-5 "rs")))
               (re (.r (.var 6.1 "re")))
               (le (.l (.var 0.0006 "le")))
               (bl (.var 8.5 "bl"))
               (ze (.ser e re le))
               (zme (.x-el-dyn ze bl))
               (cm (.cm (.var 0.0014 "cm")))
               (lm (.lm (.var 0.016 "lm")))
               (rm (.rm (.var 3.0 "rm")))
               (vm (.velocity cm)))
  (.ser zme cm lm rm)
  (-> vm (.probe "vm"))
  (-> (.diffb vm) (.probe "am"))
  (-> (.integ vm) (.probe "xm"))
  (-> (.force cm) (.probe "fc"))
  (-> (.current re) (.probe "ie")))

(defpatch lsp ((in (.var 0.0 "in"))
               (e (.e in (.var 1.0e-5 "rs")))
               (re (.r (.var 6.1 "re")))
               (le (.l (.var 0.0006 "le")))
               (bl (.var 8.5 "bl"))
               (ze (.ser e re le))
               (cm (.cm (.var 0.0014 "cm")))
               (lm (.lm (.var 0.016 "lm")))
               (rm (.rm (.var 3.0 "rm")))
               (vm (.velocity cm)))
  (.ser ze (.x-dyn-el (.ser cm lm rm) bl))
  (-> vm (.probe "vm"))
  (-> (.diffb vm) (.probe "am"))
  (-> (.integ vm) (.probe "xm"))
  (-> (.force cm) (.probe "fc"))
  (-> (.current re) (.probe "ie")))

(to-matlab lsp)
|#


;;; Loudspeaker 

(defclass .ldriver (macro-block)
  ((re :initarg :re :accessor re)
   (le :initarg :le :accessor le)
   (bl :initarg :bl :accessor bl)
   (lm :initarg :lm :accessor lm)
   (cm :initarg :cm :accessor cm)
   (rm :initarg :rm :accessor rm)
   (sm :initarg :sm :accessor sm))
  (:default-initargs
    :re t :le t :bl t
    :cm t :lm t :rm t :sm t))

(defmethod initialize-macro-block ((b .ldriver) &key) ;;; ADD Bl-control !!! Direction control !!!
  (let* ((pz (.z.))
         (re (if (member (re b) '(nil t)) (.r) (.r (re b))))
         (le (if (member (le b) '(nil t)) (.l) (.l (le b))))
         (ze (.ser le re (port pz 1)))
         (zem (.x-el-dyn ze (bl b)))
         (lm (if (member (lm b) '(nil t)) (.lm) (.lm (lm b))))
         (cm (if (member (cm b) '(nil t)) (.cm) (.cm (cm b))))
         (rm (if (member (rm b) '(nil t)) (.rm) (.rm (rm b))))
         (zmp (.ser cm lm rm zem)))
    (when (params cm)
      (setf (name (param cm)) 'cm)
      (push (param cm) (params b)))
    (when (params lm)
      (setf (name (param lm)) 'lm)
      (push (param lm) (params b)))
    (when (params rm)
      (setf (name (param rm)) 'rm)
      (push (param rm) (params b)))
    (when (params le)
      (setf (name (param le)) 'le)
      (push (param le) (params b)))
    (when (params re)
      (setf (name (param re)) 're)
      (push (param re) (params b)))
    (setf (ports b) (list (port pz 0) zmp))))

#|
(inspect (make-instance '.ldriver))
(inspect (make-instance '.P17REX))

(defpatch lsp ((in (.var 0.0 "in"))
               (e (.e in 1.0e-3))
               (sp (make-instance '.P17REX))
               (rl (.rm 0.001))
               (vm (.velocity rl)))
  (.ser (port sp 1) rl)
  (.par e (port sp 0))
  (-> vm (.probe "vm"))
  (-> (.diffb vm) (.probe "am"))
  (-> (.integ vm) (.probe "xm"))
  (-> (.current e) (.probe "ie")))

(to-matlab lsp)
(compile-patch lsp)
(step-patch-n lsp 100000 t)
|#

(defclass .P17REX (.ldriver) ()
  (:default-initargs
    :re 6.1
    :le 0.0006
    :bl 8.5
    :lm 0.016
    :cm 0.0014
    :rm 3.0
    :sm 0.0013))
    



#|
(defpatch Engl ((re (.R 7.3))
                (le (.L 0.0005))
                (le2 (.L 0.0011))
                (rle (.R 6.0))
                (ze (.ser re le (.par le2 rle)))
                (rm (.R 107.0))
                (lm (.L 0.0238))
                (cm (.C 0.00021))
                (zm (.par rm lm cm))
                (rm2 (.R 4.0))
                (lm2 (.L 0.00075))
                (cm2 (.C 0.0007))
                (zm2 (.par rm2 lm2 cm2))
                (e (.E (.imp1 10000.0) 10000.0))
                (z (.ser ze zm zm2))
                (out (.probe "out")))
  (.par e z)
  (-> (.voltage z) out))

(load-patch Engl)
(step-patch-n Engl 44100 t)
(to-matlab Engl)

(matlab-response Engl
                 :samples 8000
                 :outputs '("out")
                 :post "
%figure(10); clf;
%plot_sig(out_resp,SRATE); grid on; 
%plot(out_resp); grid on; 
%plot_logspect(out_resp,SRATE); grid on; 
%axis([0 0.0005 -1 1]);
")
|#





#|
;;; Electro->mechanical =LD ???

(defclass .el-dyn (macro-block) ())

(defmethod initialize-macro-block ((b .el-dyn) &key value)
  (let* ((xf (.xformer value))
         (p0 (port xf 0))
         (p1 (port xf 1)))
    (setf (name (param xf)) 'BL)
    (setf (ports b) (list p0 p1))
    (setf (params b) (list (param xf)))))

(defun .el-dyn (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.el-dyn :value (car rest) (cdr rest))
    (apply #'make-instance '.el-dyn :value nil rest)))

(inspect (.el-dyn 2.0))

(defparameter pat
  (patch ((eldyn (.el-dyn 1.0))
          (e (.e 1.0 1.0))
          (r (.r 1.0)))
    (connect (port e) (port eldyn 0))
    (connect (port eldyn 1) (port r))))

(defparameter pat
  (patch ((eldyn (.el-dyn 1.0))
          (e (.e 1.0 1.0))
          (r (.r 1.0)))
    (connect (port e) (port eldyn 0))
    (.par (port eldyn 1) (port r))))

(defparameter koe
  (patch ((r (.r 1.0))
          (e (.e 1.0 1.0)))
    (.par2 e (.xform r 2.0))
    (-> (.across r) (.probe "out"))))

(defparameter koe
  (patch ((r (.r 1.0))
          (e (.e 1.0 1.0)))
    (.par2 r (.xform e 2.0))
    (-> (.across r) (.probe "out"))))

(defparameter koe
  (patch ((r (.r 1.0))
          (e (.e 1.0 2.0)))
    (.par2 r (.gyrate (.xform e 1.0)))
    (-> (.across r) (.probe "out"))))

(inspect koe)

(defun .xform (obj &rest rest)
  (let* ((x (if (and (car rest) (not (keywordp (car rest))))
              (apply #'make-instance '.xformer :value (car rest) (cdr rest))
              (apply #'make-instance '.xformer :value nil rest))))
    (connect (port obj) (port x 0))
    (port x 1)))
|#


(provide :BC-xducer)
