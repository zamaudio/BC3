
(in-package :BC)

;;; ELEMENTARY BLOCKS

;;; External (Matlab) blocks

(defclass .extfun (siso-block)
  ((mfun :initarg :mfun :accessor mfun)
   (cfun :initarg :cfun :accessor cfun))
  (:default-initargs
    :mfun nil
    :cfun nil))

(defun .extfun (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.extfun rest))

(defclass .extfun2 (miso-block)
  ((mfun :initarg :mfun :accessor mfun)
   (cfun :initarg :cfun :accessor cfun))
  (:default-initargs
    :mfun nil
    :cfun nil))

(defun .extfun2 (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.extfun2 rest))

;;; .EXTFUN

(defmethod mcode ((b .extfun) &optional (stream t))
  (format stream "~%~a = " (varname (out b)))
  (format stream "~a(~a);" (mfun b)
          (varname (prev-out (in b)))))

;;; .EXTFUN2

(defmethod mcode ((b .extfun2) &optional (stream t))
  (format stream "~%~a = " (varname (out b)))
  (format stream "~a(~a,~a);" (mfun b)
          (varname (prev-out (in b 0)))
          (varname (prev-out (in b 1)))))



;;; ELEMENTARY FUNCTIONS

(defclass amiso-block (miso-block) ())

(defmethod lcode ((b amiso-block))
  (let* ((lout (lvar (out b)))
         (op (get (type-of b) 'op))
         (type (get (datatype (out b)) 'ltype))
         forms)
    (push (arr-op-to (lvar (fwd-out (link (in b))))
                     lout '%setf type) forms)
    (loop for inx in (cdr (inputs b))
          for lvari = (lvar (fwd-out (link inx)))
          do (push (arr-op-to lvari lout op type) forms))
    (cons 'progn (nreverse forms))))

(defclass asiso-block (siso-block) ())

(defmethod lcode ((b asiso-block))
  (let* ((lout (lvar (out b)))
         (op (get (type-of b) 'op))
         (type (get (datatype (out b)) 'ltype)))
    (arr-op (lvar (fwd-out (link (in b))))
            lout op type)))

;;; .ADD

(defclass .add (amiso-block) ())
(def.miso .add)
(setf (get '.add 'op) '%incf)
(def.c .add "OUT_0 = IN_0" "_IN*1_+IN**" ";")
(defmethod mcode ((b .add) &optional (stream t))
  (infix-code b "+" stream))

;;; .SUB

(defclass .sub (amiso-block) ())
(def.miso .sub)
(setf (get '.sub 'op) '%decf)
(def.c .sub "OUT_0 = IN_0" "_IN*1_-IN**" ";")
(defmethod mcode ((b .sub) &optional (stream t))
  (infix-code b "-" stream))

;;; .MUL

(defclass .mul (amiso-block) ())
(def.miso .mul)
(setf (get '.mul 'op) '%mulf)
(def.c .mul "OUT_0 = IN_0" "_IN*1_*IN**" ";")
(defmethod mcode ((b .mul) &optional (stream t))
  (infix-code b ".*" stream))

;;; .DIV

(defclass .div (amiso-block) ())
(def.miso .div)
(setf (get '.div 'op) '%divf)
(def.c .div "OUT_0 = IN_0" "_IN*1_/IN**" ";")
(defmethod mcode ((b .div) &optional (stream t))
  (infix-code b "./" stream))

;;; .POW (power)

(defclass .pow (amiso-block) ())
(def.miso .pow)
(setf (get '.pow 'op) '%pwrf)

(def.c .pow "RD_0 = IN_0;"
  "_IN*1_
RD_1 = IN**; RD_0 = pow(RD_0,RD_1); " "
OUT_0 = RD_0;")

(defmethod mcode ((b .pow) &optional (stream t))
  (infix-code b ".^" stream))


;;; .COEFF (constant coefficient)

(defclass .coeff (macro-block) ())

(defmethod initialize-macro-block ((b .coeff) &key 
                                       coeff
                                       (coeff-type *default-type*)
                                       (out-type nil))
  (let* ((c (funcall coeff-type coeff))
         (m (.mul :out-type out-type)))
    (-> c (in m 1))
    (setf (inputs b) (list (in m 0))
          (outputs b) (list (out m 0)))))


(defun .coeff (value &rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.coeff :coeff value rest))

;;; .MEAN

(defclass .mean (macro-block) ())

(defmethod initialize-macro-block ((b .mean) &key params)
  (let* ((add (apply #'.add params))
         (ins# (length (inputs add)))
         (mul (.mul)))
    (-> add (.datac (/ 1.0d0 ins#)) (in mul 1))
    (-> add mul)
    (setf (outputs b) (list (out mul))
          (inputs b) (inputs add)
          (params b) nil)))

(defun .mean (&rest rest)
  (make-instance '.mean :params rest))

;;; .MAX

(defclass .max (amiso-block) ())
(def.miso .max)
(setf (get '.max 'op) '%maxf)
(def.c .max "RD_0 = IN_0;"
  "_IN*1_
RD_1 = IN**; if (RD_1>RD_0) {RD_0 = RD_1;}" "
OUT_0 = RD_0;")

(defmethod mcode ((b .max)  &optional (stream t))
  (let* ((nout (varname (out b)))
         (nin0 (varname (prev-out (in b 0)))))
    (format stream "~%~a = ~a;" nout nin0)
    (loop for in in (cdr (inputs b))
          for nin = (varname (prev-out in))
          do (format stream "~%~a = max(~a,~a);"
                     nout nin nout))))
; .maxv

;;; .MIN

(defclass .min (amiso-block) ())
(def.miso .min)
(setf (get '.min 'op) '%minf)
(def.c .min "RD_0 = IN_0;"
  "_IN*1_
RD_1 = IN**; if (RD_1<RD_0) {RD_0 = RD_1;}" "
OUT_0 = RD_0;")

(defmethod mcode ((b .min)  &optional (stream t))
  (let* ((nout (varname (out b)))
         (nin0 (varname (prev-out (in b 0)))))
    (format stream "~%~a = ~a;" nout nin0)
    (loop for in in (cdr (inputs b))
          for nin = (varname (prev-out in))
          do (format stream "~%~a = min(~a,~a);"
                     nout nin nout))))
; .minv


;;; .SUM {sum(s) of row(s)}: (.sum src) -> dst

(defclass .sum (siso-block) ())

(defmethod initialize-instance :after ((b .sum) &key arg)
  (when arg (-> (.val arg) (in b)))
  nil)

(defmethod set-size ((b .sum) size)
  (cond ((> (length size) 1)
         (setq size (list (first size))))
        (t (setq size '(1))))
  (setf (sized b) size)
  (loop for o in (outputs b)
        do (unless (datasize o)
             (setf (datasize o) size))))

(defmethod check-sizes ((b .sum)) nil)

(defmethod lcode ((b .sum))
  (let* ((inx (prev-out (in b)))
         (din (datasize inx))
         (outx (out b))
         (dout (datasize outx)))
    (cond ((and (= (length din) 1)
                (equal dout '(1)))
           `(loop with sum = 0.0d0
                  for i from 0 below ,(first din)
                  do (incf sum (aref ,(lvar inx) i))
                  finally (setf (aref ,(lvar outx) 0) sum)))
          ((and (= (length din) 2)
                (equal dout (list (first din))))
           `(loop for i from 0 below ,(first din)
                  for sum = 0.0d0
                  do (loop for j from 0 below ,(second din)
                           do (incf sum (aref ,(lvar inx) i j)))
                  do (setf (aref ,(lvar outx) i) sum)))
          (t (error "Dimension error in ~a" b)))))

(defmethod c-code ((b .sum) &optional (stream t))
  (let* ((inx (prev-out (in b)))
         (in (cref inx))
         (din (datasize inx))
         (outx (out b))
         (out (cref outx))
         (dout (datasize outx)))
    (cond ((and (equal din '(1)) (equal dout '(1)))
           (with-c stream
             "|out| = |in|;"))
          ((and (= (length din) 1) (equal dout '(1)))
           (with-c stream
             "RD_0 = 0.0;"
             "for (LI=0; LI<|(first din)|; LI++)"
             "{RD_0 += |in|[LI];}"
             "|out| = RD_0;"))
          ((and (= (length din) 2)
                (equal dout (list (first din))))
           (with-c stream
             "for (LJ=0; LJ<|(first din)|; LJ++)"
             "{RL_0 = 0.0;"
             "for (LI=0; LI<|(second din)|; LI++)"
             "{RL_0 += |in|[LJ][LI];}"
             "|out|[LJ] = RL_0;}"))
          (t (error "Dimension error in ~a" b)))))

(defmethod mcode ((b .sum)  &optional (stream t))
  (let* ((nout (varname (out b)))
         (nin0 (varname (prev-out (in b 0)))))
    (format stream "~%~a = sum(~a')';" nout nin0)))

(defun .sum (&rest rest)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (apply #'make-instance '.sum :arg (car rest) (cdr rest))
    (apply #'make-instance '.sum rest)))

#|
; (inspect (.sum))
; (inspect (.sum 12.0))

(defpatch pp ()
  (-> (.var '(1.0 2.0)) (.sum) (.probe "out")))

(defpatch pp ()
  (-> (.var '((1.0 2.0 3.0) (3.0 4.0 5.0))) (.sum) (.probe "out")))

; REF???
(defpatch pp ()
  (-> (.add (.var '((1.0 2.0 3.0) (3.0 4.0 5.0)))
            (.var '((3.0 4.0 5.0) (4.0 5.0 6.0)))) (.probe "out")))

(c-code pp)
(load-patch pp)
(step-patch pp)
(inspect pp)
(to-matlab pp)
|#


;;; .COPY scalar copied to vector or vector to matrix

(defclass .copy (siso-block)
  ((fanout :initarg :fanout :accessor fanout))
  (:default-initargs
    :fanout nil))

(defmethod initialize-instance :after ((b .copy) &key arg)
  (when arg (-> (.val arg) (in b)))
  (cond ((integerp (fanout b)) nil)
        ((fanout b) (datasize (out (fanout b))))
        (t (setf (params b) 
                 (list (make-instance 'param
                         :host-block b
                         :name 'fanout)))))
  nil)

(defmethod include-size-params ((b .copy)) nil)

(defmethod set-size ((b .copy) size)
  (let* ((out (out b))
         (par (when (params b)
                (datasize (prev-out (param b)))))
         (fout (fanout b)))
    (unless (or (integerp fout) (params b))
      (error "Fanout not given for ~a" b))
    (cond ((and (equal size '(1)) (integerp fout))
           (setf (datasize out) (list (fanout b))))
          ((and (= (length size) 1) (integerp fout))
           (setf (datasize out) (list (car size) fout)))
          ((and (equal size '(1)) (= (length par) 1))
           (setf (datasize out) (list (car par)))
           (setf (fanout b) (car par)))
          ((and (equal size '(1)) (= (length par) 2))
           (setf (datasize out) par)
      ;   (setf (fanout b) par)
           )
          (t (error "Fanout error in ~a" b)))
    (setf (sized b) size)))

(defmethod check-sizes ((b .copy)) nil)

(defmethod lcode ((b .copy))
  (let* ((inx (prev-out (in b)))
         (din (datasize inx))
         (outx (out b))
         (dout (datasize outx)))
    (cond ((and (equal din '(1))
                (= (car dout) (fanout b)))
           `(loop with x = (aref ,(lvar inx) 0)
                  for i from 0 below ,(fanout b)
                  do (setf (aref ,(lvar outx) i) x)))
          ((and (= (length din) 1)
                (= (second dout) (fanout b)))
           `(loop for i from 0 below ,(car din)
                  for x = (aref ,(lvar inx) i)
                  do (loop for j from 0 below ,(fanout b)
                           do (setf (aref ,(lvar outx) i j) x))))
          (t (error "Dimension error in ~a" b)))))

(defmethod c-code ((b .copy) &optional (stream t))
  (let* ((inx (prev-out (in b)))
         (in (cref inx))
         (din (datasize inx))
         (outx (out b))
         (out (cref outx))
         (dout (datasize outx)))
    (cond ((and (equal din '(1))
                (= 1 (fanout b)))
           (with-c stream
             "|out| = |in|;"))
          ((and (equal din '(1))
                (= (car dout) (fanout b)))
           (with-c stream
             "RD_0 = |in|;"
             "for (LI=0; LI<|(first dout)|; LI++)"
             "{|out|[LI] = RD_0;}"))
          ((and (= (length din) 1)
                (= (second dout) (fanout b)))
           (with-c stream
             "for (LI=0; LI<|(first din)|; LI++)"
             "{RD_0 = |in|[LI];"
             "for (LJ=0; LJ<|(fanout b)|; LJ++)"
             "{|out|[LI][LJ] = RD_0;}}"))
          (t (error "Dimension error in ~a" b)))))

(defmethod mcode ((b .copy)  &optional (stream t))
  (let* ((outx (out b))
         (nout (varname outx))
         (dout (datasize outx))
         (inx (prev-out (in b)))
         (nin0 (varname inx))
         (din (datasize inx)))
    (cond ((and (equal din '(1))
                (= (car dout) (fanout b)))
           (format stream "~%~a = ~a*ones(~a,1);"
                   nout nin0 (fanout b)))
          ((and (= (length din) 1)
                (= (second dout) (fanout b)))
           (format stream
                   "~a=[]; for k=1:~a ~a(k,:)=~a(k)*ones(1,~a); end"
                   nout (fanout b) nout nin0 (fanout b)))
          (t (error "Dimension error in ~a" b)))))

(defun .copy (&rest rest)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (apply #'make-instance '.copy :arg (car rest) (cdr rest))
    (apply #'make-instance '.copy rest)))

#|
(inspect (make-instance '.copy))
(inspect (make-instance '.copy :fanout 5))

(defpatch pp ()
  (-> (.var 1.0) (.copy :fanout 5) (.probe "out")))

(defpatch pp ((y (.var '(1.0 2.0 3.0)))
              (f (.copy (.var 1.0))))
  (-> y (param f))
  (-> f (.probe "out")))

(defpatch pp ((y (.var '((1.0 2.0) (3.0 4.0))))
              (f (.copy (.var 1.0))))
  (-> y (param f))
  (-> f (.probe "out")))

(defpatch pp ()
  (-> (.var '(1.0 2.0)) (.copy :fanout 5) (.probe "out")))

(defpatch pp ()
  (-> (.var '(2.0)) (.copy :fanout 5) (.probe "out")))

(defpatch pp ((c (.copy)))
  (-> (.var '(1.0 2.0 3.0)) (param c))
  (-> (.var 2.0) c (.probe "out")))

(defpatch pp ((c (.copy)))
  (-> (.var '(1.0)) (param c))
  (-> (.var 2.0) c (.probe "out")))

(inspect pp)
(c-code pp)
(load-patch pp)
(step-patch pp)
(to-matlab pp)
|#


;;; .PROBE

(defclass .probe (asiso-block) ())

(defmethod initialize-instance :after ((b .probe) &key)
  (setf (creader (out b)) t))

(setf (get '.probe 'op) '+)

(def.c .probe "OUT_0 = IN_0;")

(defmethod get-variables ((b .probe))
  (setf (var-p b) t (var-p (out b)) t)
  (get-outvars b))

(defmethod print-object ((b .probe) (stream t))
  (let* ((name (name b))
         (value (value (out b)))) ;; (lvar (out b))))
    (print-unreadable-object (b stream :type t :identity t)
      (when name (format stream "\"~a\"" name))
      (format stream " ~a" value))
    b))

(defmethod report-toplevel :after ((b .probe) &key (stream t))
  (format stream ",  VALUE = ~a" (lvar (out b)))
  (let* ((pout (link (in b)))
         (thost (top-host-block pout)))
    (format stream " from ~a of ~a" pout thost)))

(defmethod mcode ((b .probe) &optional (stream t))
  (format stream "~%~a = ~a;"
          (varname (out b))
          (varname (prev-out (in b)))))

(defun .probe (name &rest rest)
  (declare (dynamic-extent rest))
  (unless (or (typep name 'symbol)
              (typep name 'string))
    (error "No name for a .probe"))
  (apply #'make-instance '.probe
         :name name :varname name rest))


(defmethod probe-response ((b patch) &key
                              (probe-name "out")
                              (samples 4000))
  (load-patch b)
  (let* ((outb (find-block b probe-name))
         (out (out outb))
         (resp (list (at out))))
    (loop for i from 1 below samples
          do (step-patch b)
          do (push (at out) resp))
    (nreverse resp)))


;;; .NEG

(defclass .neg (asiso-block) ())
(def.siso .neg)
(setf (get '.neg 'op) '-)
(def.c .neg "OUT_0 = -IN_0;")
(defmethod mcode ((b .neg) &optional (stream t))
  (format stream "~%~a = -~a;"
          (varname (out b))
          (varname (prev-out (in b)))))
;;; .INV

(defclass .inv (asiso-block) ())
(def.siso .inv)
(setf (get '.inv 'op) '/)
(def.c .inv "OUT_0 = 1.0/IN_0;")
(defmethod mcode ((b .inv) &optional (stream t))
  (format stream "~%~a = 1./~a;"
          (varname (out b))
          (varname (prev-out (in b)))))
;;; .ABS

(defclass .abs (asiso-block) ())
(def.siso .abs)
(setf (get '.abs 'op) 'abs)
(def.c .abs "OUT_0 = abs(IN_0);")
(defmethod mcode ((b .abs) &optional (stream t))
  (format stream "~%~a = abs(~a);"
          (varname (out b))
          (varname (prev-out (in b)))))
;;; .SIGN

(defclass .sign (asiso-block) ())
(def.siso .sign)
(setf (get '.sign 'op)
      '(lambda (x) (if (>= x 0.0d0) 1.0d0 -1.0d0)))
(def.c .sign "RD_0 = IN_0; OUT_0 = (RD_0>=0.0) ? 1.0 : -1.0;")
;;; mcode ?????

;;; .RECTIFY (HALF WAVE)

(defclass .rect (asiso-block) ())
(def.siso .rect)
(setf (get '.rect 'op)
      '(lambda (x) (if (>= x 0.0d0) x 0.0d0)))
(def.c .rect "RD_0 = IN_0; OUT_0 = (RD_0>=0.0) ? RD_0 : 0.0;")
(defmethod mcode ((b .rect) &optional (stream t))
  (let ((vnout (varname (prev-out (in b)))))
    (format stream "~%~a = sign(~a).*~a;"
            (varname (out b)) vnout vnout)))
;;; .SIN

(defclass .sin (asiso-block) ())
(def.siso .sin)
(setf (get '.sin 'op) 'sin)
(def.c .sin "OUT_0 = sin(IN_0);")
(defmethod mcode ((b .sin) &optional (stream t))
  (format stream "~%~a = sin(~a);"
          (varname (out b))
          (varname (prev-out (in b)))))
;;; .COS

(defclass .cos (asiso-block) ())
(def.siso .cos)
(setf (get '.cos 'op) 'cos)
(def.c .cos "OUT_0 = cos(IN_0);")
(defmethod mcode ((b .cos) &optional (stream t))
  (format stream "~%~a = cos(~a);"
          (varname (out b))
          (varname (prev-out (in b)))))

;;; .COSH

(defclass .cosh (asiso-block) ())
(def.siso .cosh)
(setf (get '.cosh 'op) 'cosh)
(def.c .cosh "OUT_0 = cosh(IN_0);")
(defmethod mcode ((b .cosh) &optional (stream t))
  (format stream "~%~a = cosh(~a);"
          (varname (out b))
          (varname (prev-out (in b)))))

;;; .TAN

(defclass .tan (asiso-block) ())
(def.siso .tan)
(setf (get '.tan 'op) 'tan)
(def.c .tan "OUT_0 = tan(IN_0);")
(defmethod mcode ((b .tan) &optional (stream t))
  (format stream "~%~a = tan(~a);"
          (varname (out b))
          (varname (prev-out (in b)))))
;;; .TANH

(defclass .tanh (asiso-block) ())
(def.siso .tanh)
(setf (get '.tanh 'op) 'tanh)
(def.c .tanh "OUT_0 = tanh(IN_0);")
(defmethod mcode ((b .tanh) &optional (stream t))
  (format stream "~%~a = tanh(~a);"
          (varname (out b))
          (varname (prev-out (in b)))))

;;; .COT

(defclass .cot (asiso-block) ())
(def.siso .cot)
(setf (get '.cot 'op) 'cot)
(def.c .cot "OUT_0 = cot(IN_0);")
(defmethod mcode ((b .cot) &optional (stream t))
  (format stream "~%~a = cot(~a);"
          (varname (out b))
          (varname (prev-out (in b)))))

;;; .SQR

(defclass .sqr (asiso-block) ())
(def.siso .sqr)
(setf (get '.sqr 'op)
      '(lambda (x) (* x x)))
(def.c .sqr "RD_0 = IN_0; OUT_0 = RD_0 * RD_0;")
(defmethod mcode ((b .sqr) &optional (stream t))
  (let ((vnout (varname (prev-out (in b)))))
    (format stream "~%~a = ~a.*~a;"
            (varname (out b)) vnout vnout)))

;;; .EXP

(defclass .exp (asiso-block) ())
(def.siso .exp)
(setf (get '.exp 'op) 'exp)
(def.c .exp "OUT_0 = exp(IN_0);")
(defmethod mcode ((b .exp) &optional (stream t))
  (let ((vnout (varname (prev-out (in b)))))
    (format stream "~%~a = exp(~a);"
            (varname (out b)) vnout)))

;;; .LOG*

(defclass .log* (asiso-block) ())
(def.siso .log*)
(setf (get '.log* 'op)
      '(lambda (x) (if (>= x 0.0d0) (log x) 0.0d0)))
(def.c .log* "RD_0 = IN_0; OUT_0 = (RD_0>=0.0) ? log(RD_0) : 1.0e-100;")
(defmethod mcode ((b .log*) &optional (stream t))
  (let ((vnout (varname (prev-out (in b)))))
    (format stream "~%~a = log(~a.*(~a>0));"
            (varname (out b)) vnout vnout)))

; .log ??

;;; .LOG10*

(defclass .log10* (asiso-block) ())
(def.siso .log10*)
(setf (get '.log10* 'op)
      '(lambda (x) (if (>= x 0.0d0) (log x 10.0d0) 0.0d0)))
(def.c .log10* "RD_0 = IN_0; OUT_0 = (RD_0>=0.0) ? log10(RD_0) : 1.0e-100;")
(defmethod mcode ((b .log10*) &optional (stream t))
  (let ((vnout (varname (prev-out (in b)))))
    (format stream "~%~a = log10(~a.*(~a>0));"
            (varname (out b)) vnout vnout)))

;;; .DB

(defclass .db (asiso-block) ())
(def.siso .db)
(setf (get '.db 'op)
      '(lambda (x) (setq x (abs x))
        (when (< x 1.0d-30) (setq x 1.0d-30))
        (* 20.0d0 (log x 10.0d0))))
(def.c .db "RD_0 = IN_0; if (RD_0<0.0) RD_0=-1*RD_0;
if (RD_0<1.0e-30) RD_0=1.0d-30; OUT_0 = 20.0*log10(RD_0);")
(defmethod mcode ((b .db) &optional (stream t))
  (let ((vnin (varname (prev-out (in b))))
        (vnout (varname (out b))))
    (format stream "~%~a = abs(~a); 
if (~a<1.0d-30) ~a = -600.0; 
else ~a = 20.0d0*log10(~a); end"
            vnout vnin vnout vnout vnout vnout)))

;;; .SQRT*

(defclass .sqrt* (asiso-block) ())
(def.siso .sqrt*)
(setf (get '.sqrt* 'op)
      '(lambda (x) (if (>= x 0.0d0) (sqrt x) 0.0d0)))
(def.c .sqrt* "RD_0 = IN_0; OUT_0 = (RD_0>=0.0) ? sqrt(RD_0) : 0.0;")
(defmethod mcode ((b .sqrt*) &optional (stream t))
  (let ((inname (varname (prev-out (in b))))
        (outname (varname (out b))))
    (format stream "~% if (~a<0) ~a = 0; else ~a = sqrt(~a); end"
            inname outname outname inname)))

; .sqrt ??

; .truncate ??
; .floor
; .ceiling
; .round

; .counter
; .rand

#|
;;; .POLY

(defclass .poly (macro-block) ())

(defmethod initialize-macro-block ((b .poly) &key arg coeffs)
  (unless coeffs (error "No coeffiecients for .poly"))
  (let* ((add (.add)) (x (.x.)))
    (setf (outputs b) (list (output add)))
    (setf (name (in x)) 'sigin)
    (-> x add)
    (if arg (-> arg x)
        (setf (inputs b) (list (in x))))))
    
|#



;;; LOGIC FUNCTIONS

(defclass lamiso-block (amiso-block) ()
  (:default-initargs
    :out-type '.short))

;;; .AND

(defclass .and (lamiso-block) ())
(def.miso .and)
(setf (get '.and 'op) '%andf)

(def.c .and "RL_0 = IN_0;"
  "_IN*1_
RL_1 = IN**; RL_0 = (RL_0&RL_1); " "
OUT_0 = (RL_0!=0)?1:0;")

(defmethod mcode ((b .and) &optional (stream t))
  (infix-code b "&" stream))

;;; .OR

(defclass .or (lamiso-block) ())
(def.miso .or)
(setf (get '.or 'op) '%orf)

(def.c .or "RL_0 = IN_0;"
  "_IN*1_
RL_1 = IN**; RL_0 = (RL_0|RL_1); " "
OUT_0 = (RL_0!=0)?1:0;")

(defmethod mcode ((b .or) &optional (stream t))
  (infix-code b "|" stream))

; .nand
; .nor
; .xor

(defclass .not (asiso-block) ())
(def.siso .not)

; .not

; .eq


;;; COMPARISON PREDICATE FUNCTIONS

;;; .GT (greater than)

(defmacro %gtf (dst src type)
  `(setf ,dst (map.type1 (min ,dst ,src) ,type)))

(defclass .gt (amiso-block) ()
  (:default-initargs
    :value 1
    :out-type '.short))

(def.miso .gt)
(setf (get '.gt 'op) '%gtf)

(def.c .gt "RL_0 = 1; RD_0 = IN_0;"
  "_IN*1_
RD_1 = IN**; if (RD_1<=RD_0) {RL_0 = 0;} {RD_0 = RD_1;}" "
OUT_0 = RL_0;")

(defmethod mcode ((b .gt)  &optional (stream t)) ;;; ????
  (let* ((nout (varname (out b)))
         (nin0 (varname (prev-out (in b 0)))))
    (format stream "~%~a = ~a;" nout nin0)
    (loop for in in (cdr (inputs b))
          for nin = (varname (prev-out in))
          do (format stream "~%~a = max(~a,~a);"
                     nout nin nout))))

#|
(defpatch gtp ((gt (.gt :inputs 3)))
  (-> (.var 1.0) (in gt 0))
  (-> (.var 0.1) (in gt 2))
  (-> (.var 0.5) (in gt 1) (.probe "out")))

(defpatch gtp ((gt (.gt)))
  (-> (.var '(1.0 1.5)) (in gt 0))
  (-> (.var '(0.5 2.0)) (in gt 1) (.probe "out")))

(c-code gtp)
(inspect gtp)
(compile-patch gtp)
|#

; .ge
; .lt
; .le

; .limit
; .limit+
; .limit-
; .smooth
; .ramp1
; .ramp
; .pan
; .pan-smooth
; .pan-ramp1
; .select
; .mix
; .wtable
; .rtable
; .io-table
; .imp



;;; SELECT one (constant index)

(defclass .select-chan (siso-block)
  ((channel :initarg :channel :accessor channel)))

(defmethod finalize-sizes ((b .select-chan))
  (setf (datasize (out b)) '(1)))

(defmethod check-sizes ((b .select-chan))
  (let* ((pout (prev-out (in b)))
         (chan (channel b)))
    (unless (= (length (datasize pout)) 1)
      (error "Input dimensions error in ~a" b))
    (unless (< -1 chan (first (datasize pout)))
      (error "Channel index ~a not valid in ~a" chan b))))

(defmethod lcode ((b .select-chan))
  (let* ((in (lvar (prev-out (in b))))
         (out (lvar (out b)))
         (chan (channel b)))
    `(setf (aref ,out 0) (aref ,in ,chan))))

(defmethod mcode ((b .select-chan) &optional (stream t))
  (format stream "~%~a = ~a(~a);"
          (varname (out b))
          (varname (prev-out (in b)))
          (1+ (channel b))))

(defmethod c-code ((b .select-chan) &optional (stream t))
  (let* ((in (cref (in b)))
         (out (cref (out b)))
         (chan (channel b)))
    (with-c stream
      "|out| = |in|[|chan|];")))

(defun .select-chan (channel &rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.select-chan
         :channel channel rest))


;;; PACK SCALARS INTO A VECTOR

(defclass .pack (miso-block) ())

(defmethod finalize-sizes ((b .pack))
  (setf (datasize (out b))
        (list (length (inputs b)))))

(defmethod check-sizes ((b .pack))
  (loop for In in (inputs b)
        for o = (prev-out in)
        do (unless (equal (datasize o) '(1))
             (error "Inputs to .pack must be scalars")))
  nil)

(defmethod lcode ((b .pack))
  (let* ((ins (inputs b))
         (out (lvar (out b))))
    `(loop for ix in ',ins
           for ox = (prev-out ix)
           for i from 0
           do (setf (aref ,out i)
                    (aref (lvar ox) 0)))))

(defmethod c-code ((b .pack) &optional (stream t))
  (loop with out = (cref (out b))
        for ix in (inputs b)
        for ox = (cref (prev-out ix))
        for n from 0
        do (with-c stream
             "|out|[|n|] = |ox|;")))

(defmethod mcode ((b .pack) &optional (stream t))
  (format stream "~%~a = [~a"
          (varname (out b))
          (varname (prev-out (in b))))
  (loop for ix in (cdr (inputs b))
        for o = (prev-out ix)
        do (format stream ";~a" (varname o)))
  (format stream "];")
  nil)

(defun .pack (&rest inputs)
  (let* ((b (make-instance '.pack
              :inputs (length inputs))))
    (loop for ix in (inputs b)
          for iy in inputs
          for iz = (.val iy)
          do (connect (out iz) ix))
    b))

#|
(defparameter px (.pack (.const 1.0) (.const 2.0)))
(defparameter px (.pack (.const 1.0) (.add 1.0 2.0)))
(defparameter px (.pack (.const 1.0) 1.0))
(inspect px)
(lcode px)
(mcode px)

(defpatch koe ((c1 (.const '(1 2) :out-type '.long))
               (v1 (.var 2 :out-type '.long)))
  (-> (.pack c1 v1) (.probe "out" :out-type '.double)))

(defpatch koe ((c1 (.const 1.0))
               (v1 (.var 2.0)))
  (-> (.pack c1 v1) (.probe "out")))

(inspect koe)
(c-code koe)
(load-patch koe)
(step-patch koe)
(to-matlab koe)
|#

(provide :BC-elemfun)
