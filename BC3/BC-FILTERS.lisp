
(in-package :bc)


;;; FRACTIONAL PROCESSING

(defun lgint0 (x &optional xdst xind)
  (let* ((ind (round x))
         (dx (- x ind)))
    (unless xdst (setq xdst #(0.0d0)))
    (setf (aref xdst 0) 1.0d0)
    (unless xind (setq xind #(0)))
    (setf (aref xind 0) ind)
    (values xdst xind dx)))

(defun lgint1 (x &optional xdst xind)
  (let* ((ind (floor x))
         (dx (- x ind)))
    (unless xdst (setq xdst #(0.0d0 0.0d0)))
    (setf (aref xdst 0) (float (- 1.0 dx) 1.0d0)
          (aref xdst 1) (float dx 1.0d0))
    (unless xind (setq xind #(0)))
    (setf (aref xind 0) ind)
    (values xdst xind dx)))

(defun lgint3 (x &optional xdst xind)
  (let* ((ind (- (floor x) 1))
         (dx (float (- x ind) 1.0d0))
         (dx0 dx)
         (dx1 (- dx 1.0d0))
         (dx2 (- dx 2.0d0))
         (dx3 (- dx 3.0d0))
         (h0 (* -0.16666666666667d0 dx1 dx2 dx3))
         (h1 (* 0.5d0 dx0 dx2 dx3))
         (h2 (* -0.5d0 dx0 dx1 dx3))
         (h3 (* 0.16666666666667d0 dx0 dx1 dx2)))
    (unless xdst (setq xdst #(0.0d0 0.0d0 0.0d0 0.0d0)))
    (setf (aref xdst 0) h0
          (aref xdst 1) h1
          (aref xdst 2) h2
          (aref xdst 3) h3)
    (unless xind (setq xind #(0)))
    (setf (aref xind 0) ind)
    (values xdst xind dx)))

(defun lgint5 (x &optional xdst xind)
  (let* ((ind (- (floor x) 2))
         (dx (float (- x ind) 1.0d0))
         (dx0 dx)
         (dx1 (- dx 1.0d0))
         (dx2 (- dx 2.0d0))
         (dx3 (- dx 3.0d0))
         (dx4 (- dx 4.0d0))
         (dx5 (- dx 5.0d0))
         (h0 (* -0.00833333333333d0 dx1 dx2 dx3 dx4 dx5))
         (h1 (* 0.04166666666667d0 dx0 dx2 dx3 dx4 dx5))
         (h2 (* -0.08333333333333d0 dx0 dx1 dx3 dx4 dx5))
         (h3 (* 0.08333333333333d0 dx0 dx1 dx2 dx4 dx5))
         (h4 (* -0.04166666666667d0 dx0 dx1 dx2 dx3 dx5))
         (h5 (* 0.00833333333333d0 dx0 dx1 dx2 dx3 dx4)))
    (unless xdst (setq xdst #(0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0)))
    (setf (aref xdst 0) h0
          (aref xdst 1) h1
          (aref xdst 2) h2
          (aref xdst 3) h3
          (aref xdst 4) h4
          (aref xdst 5) h5)
    (unless xind (setq xind #(0)))
    (setf (aref xind 0) ind)
    (values xdst xind dx)))


(defclass .fractn (basic-block)
  ((order :initarg :order :accessor order)
   (offset :initarg :offset :accessor offset)
   (scaler :initarg :scaler :accessor scaler)
   (limit+ :initarg :limit+ :accessor limit+)
   (limit- :initarg :limit- :accessor limit-))
  (:default-initargs
    :order nil
    :offset nil
    :scaler nil
    :limit+ nil
    :limit- nil))

(defmethod initialize-instance :after ((b .fractn) &key)
  (setf (inputs b)
        (list (make-instance 'input
                :host-block b)))
  (let* ((xi (make-instance 'output 
               :host-block b :name 'xi
               :type '.long :size '(1)))
         (dx (make-instance 'output
               :host-block b :type '.double
               :name 'dx :value nil
               :size (list (1+ (order b))))))
    (setf (outputs b) (list xi dx))))

(defmethod check-sizes ((b .fractn)) nil)

(defmethod lcode ((b .fractn))
  (let* ((inv (prev-out (in b)))
         (o0v (out b 0))
         (o1v (out b 1))
         (ofun (case (order b)
                 (0 #'lgint0)
                 (1 #'lgint1)
                 (3 #'lgint3)
                 (5 #'lgint5))))
    `(let* ((inx (aref (lvar ,inv) 0)))
       ,(when (scaler b)
          `(setq inx (* inx ,(scaler b))))
       ,(when (limit- b)
          `(setq inx (max inx ,(limit- b))))
       ,(when (limit+ b)
          `(setq inx (min inx ,(limit+ b))))
       ,(when (offset b)
          `(incf inx ,(offset b)))
       (multiple-value-bind
         (dx% xi%)
         (funcall ,ofun inx)
         (when dx% (setf (lvar ,o1v) dx%))
         (setf (lvar ,o0v) xi%)))))

(defmethod mcode ((b .fractn) &optional (stream t))
  (let* ((onam0 (varname (out b 0)))
         (onam1 (varname (out b 1)))
         (inam (varname (prev-out (in b))))
         (ofun (case (order b)
                 (0 "lgint0")
                 (1 "lgint1")
                 (3 "lgint3")
                 (5 "lgint5"))))
    (format stream "~%[~a,~a] = ~a(~a);"
            onam1 onam0 ofun inam)))


(defmethod %offs ((b .fractn))
  (if (offset b)
    (format nil "RD_0 += ~a; "
            (offset b)) ""))

(defmethod %lim+ ((b .fractn))
  (if (limit+ b)
    (format nil "if (RD_0>~a) RD_0=~a; "
            (limit+ b) (limit+ b)) ""))

(defmethod %lim- ((b .fractn))
  (if (limit- b)
    (format nil "if (RD_0<~a) RD_0=~a; "
            (limit- b) (limit- b)) ""))

(defmethod %scale ((b .fractn))
  (if (scaler b)
    (format nil "RD_0 *= ~a; "
            (scaler b)) ""))


(defclass .fract0 (.fractn) ()
  (:default-initargs
    :order 0))

(defmethod c-code ((b .fract0) &optional (stream t))
  (let* ((in (cref (in b)))
         (sc (%scale b))
         (offs (%offs b))
         (lim+ (%lim+ b))
         (lim- (%lim- b))
         (o0 (cref (out b 0))))
    (with-c stream
      "RD_0=|in|; |sc||lim+||lim-||offs|RL_0=round(RD_0); |o0| = RL_0;")))

(defun .fract0 (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.fract0 rest))


(defclass .fract1 (.fractn) ()
  (:default-initargs
    :order 1))

(defmethod c-code ((b .fract1) &optional (stream t))
  (let* ((in (cref (in b)))
         (sc (%scale b))
         (offs (%offs b))
         (lim+ (%lim+ b))
         (lim- (%lim- b))
         (o0 (cref (out b 0)))
         (o1 (cref (out b 1))))
    (with-c stream
      "RD_0=|in|; |sc||lim+||lim-||offs|RL_0=floor(RD_0); RD_1=RD_0-RL_0;"
      "|o0|=RL_0; |o1|[0]=1.0-RD_1; |o1|[1]=RD_1;")))

(defun .fract1 (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.fract1 rest))


(defclass .fract3 (.fractn) ()
  (:default-initargs
    :order 3))

(defmethod c-code ((b .fract3) &optional (stream t))
  (let* ((in (cref (in b)))
         (sc (%scale b))
         (offs (%offs b))
         (lim+ (%lim+ b))
         (lim- (%lim- b))
         (o0 (cref (out b 0)))
         (o1 (cref (out b 1))))
    (with-c stream
      "RD_0=|in|; |sc||lim+||lim-||offs|RL_0=floor(RD_0)-1;"
      "|o0| = RL_0; RD_0 = RD_0-RL_0;"
      "RD_1 = RD_0; RD_2 = RD_0-1.0;"
      "RD_3 = RD_0-2.0; RD_4 = RD_0-3.0;"
      "|o1|[0] = -0.16666666666667 * RD_2 * RD_3 * RD_4;"
      "|o1|[1] =  0.5 * RD_1 * RD_3 * RD_4;"
      "|o1|[2] = -0.5 * RD_1 * RD_2 * RD_4;"
      "|o1|[3] =  0.16666666666667 * RD_1 * RD_2 * RD_3;")))

(defun .fract3 (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.fract3 rest))


(defclass .fract5 (.fractn) ()
  (:default-initargs
    :order 5))

(defmethod c-code ((b .fract5) &optional (stream t))
  (let* ((in (cref (in b)))
         (sc (%scale b))
         (offs (%offs b))
         (lim+ (%lim+ b))
         (lim- (%lim- b))
         (o0 (cref (out b 0)))
         (o1 (cref (out b 1))))
    (with-c stream
      "RD_0=|in|; |sc||lim+||lim-||offs|RL_0=floor(RD_0)-2;"
      "|o0| = RL_0; RD_0 = RD_0-RL_0;"
      "RD_1 = RD_0; RD_2 = RD_0-1.0;"
      "RD_3 = RD_0-2.0; RD_4 = RD_0-3.0;"
      "RD_5 = RD_0-4.0; RD_6 = RD_0-5.0;"
      "|o1|[0] = -0.00833333333333 * RD_2 * RD_3 * RD_4 * RD_5 * RD_6;"
      "|o1|[1] =  0.04166666666667 * RD_1 * RD_3 * RD_4 * RD_5 * RD_6;"
      "|o1|[2] = -0.08333333333333 * RD_1 * RD_2 * RD_4 * RD_5 * RD_6;"
      "|o1|[3] =  0.08333333333333 * RD_1 * RD_2 * RD_3 * RD_5 * RD_6;"
      "|o1|[4] = -0.04166666666667 * RD_1 * RD_2 * RD_3 * RD_4 * RD_6;"
      "|o1|[5] =  0.00833333333333 * RD_1 * RD_2 * RD_3 * RD_4 * RD_5;")))

(defun .fract5 (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.fract5 rest))


;;; TABLE INTERPOLATOR

(defclass .tab-interp1 (basic-block)
  ((circular :initarg :circular :accessor circular))
  (:default-initargs
    :circular nil))

(defmethod initialize-instance :after ((b .tab-interp1) &key)
  (setf (inputs b) (list (make-instance 'input :name 'table :host-block b)))
  (setf (params b) (list (make-instance 'param :name 'index :host-block b)
                         (make-instance 'param :name 'fract :host-block b)))
  (setf (outputs b) (list (make-instance 'output :name 'vect :host-block b))))

(defmethod finalize-sizes :after ((b .tab-interp1))
  (let* ((in (prev-out (in b)))
         (dim (datasize in))
         (p0 (prev-out (param b 0)))
         (p1 (prev-out (param b 1))))
    (unless (= (length dim) 2)
      (error "Input data to ~a not 2-D array" b))
    (unless (and (equal (datasize p0) '(1))
                 (eq (datatype p0) '.long))
      (error "Invalid index param to ~a" b))
    (unless (= (length (datasize p1)) 1)
      (error "Invalid fract param to ~a" b))
    (setf (datasize (out b)) (list (second dim)))))

(defmethod c-code ((b .tab-interp1) &optional (stream t))
  (let* ((ino (prev-out (in b)))
         (in (cref ino))
         (idim (datasize ino))
         (cols (first idim))
         (rows (second idim))
         (out (cref (out b)))
         (ind (cref (param b 'index)))
         (pfract (prev-out (param b 'fract)))
         (vec (cref (param b 'fract)))
         (csiz cols)
         (fsiz (first (datasize pfract))))
    (if (circular b)
      (with-c stream
        "for (RL_0=0; RL_0<|rows|; RL_0++){"
        "RL_1 = |ind|; RD_0 = 0.0;"
        "for (RL_2=0; RL_2<|fsiz|; RL_2++)"
        "{RL_3 = abs(RL_1)%|csiz|;"
        "if (RL_1<0) {RL_3 = (|csiz|-RL_3)%|csiz|;}"
        "RD_1 = |in|[RL_3][RL_0]; RL_1++;"
        "RD_0 += RD_1*|vec|[RL_2];}"
        "|out|[RL_0] = RD_0;}")
      (with-c stream
        "for (RL_0=0; RL_0<|rows|; RL_0++){"
        "RL_1 = |ind|; RD_0 = 0.0;"
        "for (RL_2=0; RL_2<|fsiz|; RL_2++) {"
        "RD_1 = ((RL_1>=0)&(RL_1<|cols|)) ? |in|[RL_1][RL_0] : 0.0;"
        "RL_1++; RD_0 += RD_1*|vec|[RL_2];}"
        "|out|[RL_0] = RD_0;}"))))

(defmethod lcode ((b .tab-interp1))
  (let* ((ino (lvar (prev-out (in b))))
         (idim (datasize (prev-out (in b))))
         (cols (first idim))
         (rows (second idim))
         (out (lvar (out b)))
         (ind (lvar (prev-out (param b 'index))))
         (pfract (prev-out (param b 'fract)))
         (vec (lvar pfract)) (csiz cols)
         (fsiz (first (datasize pfract))))
    (if (circular b)
      `(loop with cs = ,csiz and rd1
             for rd0 = 0.0d0
             for rl0 from 0 below ,rows
             for rl1 = (aref ,ind 0)
             do (loop for rl2 from 0 below ,fsiz
                      for rl3 = (mod (abs rl1) cs)
                      do (if (< rl1 0)
                           (setq rl3 (mod (- cs rl3) cs)))
                      do (setq rd1 (aref ,ino rl3 rl0))
                      do (incf rl1)
                      do (incf rd0 (* rd1 (aref ,vec rl2))))
             do (setf (aref ,out rl0) rd0))
      `(loop for rd0 = 0.0d0
             for rl0 from 0 below ,rows
             for rl1 = (aref ,ind 0)
             do (loop for rl2 from 0 below ,fsiz
                      for rd1 = (if (and (>= rl1 0) (< rl1 ,cols))
                                  (aref ,ino rl1 rl0) 0.0d0)
                      do (incf rl1)
                      do (incf rd0 (* rd1 (aref ,vec rl2))))
             do (setf (aref ,out rl0) rd0)))))

#|
(defmethod mcode ((b .tab-interp1) &optional (stream t))
  (let* ((ino (prev-out (in b)))
         (idim (datasize ino))
         (inam (varname ino))
         (cols (first idim))
         (rows (second idim))
         (onam (varname (out b)))
         (indnam (varname (prev-out (param b 'index))))
         (pfract (prev-out (param b 'fract)))
         (vec (varname (param b 'fract)))
         (csiz cols)
         (fsiz (first (datasize pfract))))
    (cond ((circular b)
           (format stream "~%~a = ~a(:,mod(~a:~a+~a,~a))*~a;"
                   onam inam indnam indnam fsiz csiz vec))
          (t (format stream "~%~a = ~a(:,??)*~a;"
                     onam inam      vec)))))
    
(defmethod mcode ((b .fir%) &optional (stream t))
  (let* ((onam (varname (out b)))
         (inam (varname (prev-out (in b))))
         (offnam (varname (offs b)))
         (snam (varname (state b)))
         (coeffs (prev-out (param b)))
         (cnam (varname coeffs))
         (size (ssize b))
         (dsiz (dsize b)))
    (format stream "~%if (~a<=1) ~a(~a:~a) = ~a(1:~a);"
            offnam snam (+ 1 dsiz) (+ size dsiz) snam size)
    (format stream " ~a = ~a; end" offnam (+ 1 dsiz))
    (format stream "~%~a(~a) = ~a;" snam offnam inam)
    (format stream " ~a = ~a(~a:~a+~a)'*~a;"
            onam snam offnam offnam (1- size) cnam)
    (format stream " ~a = ~a-1;" offnam offnam)))
|#

(defun .tab-interp1 (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.tab-interp1 rest))



;;; This is the higher-level table interpolator
;;; for interpolating in a 2-D table by vector output
;;; data, if given by list of array, is the 2-D array
;;; if not given, param with name 'table is created
;;; param named 'interp is a control for interpolation
;;; if circular non-nil, interpolation is circular
;;; order {0,1,3,5} is order of Lagrange interpolation
;;; decimate is decimation factor (mrate = 1/decimate)

(defclass .interp-2>1 (macro-block) ())

(defmethod initialize-macro-block ((b .interp-2>1) &key
                                       (decimate 1)
                                       (order 1) data
                                       (circular nil))
  (let* ((fr (case order (0 '.fract0) (1 '.fract1) (3 '.fract3) (5 '.fract5)
                   (t (error "Invalid fractional order in ~a" b))))
         (fract (funcall fr :mrate (/ decimate)))
         (inter (.tab-interp1 :circular circular :mrate (/ decimate)))
         (p1 (.p.)))
    (-> (out fract 0) (param inter 0))
    (-> (out fract 1) (param inter 1))
    (if data (-> (.var data) (in inter))
        (let* ((p2 (.p.)))
          (setf (name (param p2)) 'table)
          (push (param p2) (params b))
          (-> p2 (in inter))))
    (setf (outputs b) (list (out inter)))
    (setf (name (param p1)) 'interp)
    (-> p1 (in fract))
    (push (param p1) (params b))))

(defun .interp-2>1 (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (consp (car rest)) (not (keywordp (car rest))))
    (apply #'make-instance '.interp-2>1 :data (car rest) (cdr rest))
    (apply #'make-instance '.interp-2>1 rest)))



;;; DELAY TAPS

;;; .DTAP is for controlling tap outputs of a delay
;;; Each tap can have independent controls as the
;;; keywords given in the initialization method
;;; (order, length/time max-length/max-time, decimate)

(defclass .dtap (macro-block)
  ((max-length :initform nil :accessor max-length)
   (min-length :initform nil :accessor min-length)
   (order :initform nil :accessor order)))

(defmethod initialize-macro-block ((b .dtap) &key
                                       (order 3)
                                       length
                                       time
                                       max-length
                                       max-time
                                       (decimate 1))
  (let* ((fr (case order (0 '.fract0) (1 '.fract1) (3 '.fract3) (5 '.fract5)
                   (t (error "Invalid fractional order in ~a" b))))
         (min-length (case order (0 1.0d0) (1 1.0d0) (3 2.0d0) (5 3.0d0)))
         (fract (funcall fr :order order :mrate (/ decimate)
                         :offset -1.0d0 :limit- min-length))
         (sr (srate b)) p)
    (setf (order b) order)
    (cond ((and length (numberp length)) (setq p (.const length)))
          ((typep length 'basic-block) (setq p length))
          ((and time (numberp time)) (setq p (.const time)))
          ((typep time 'basic-block) (setq p time))
          ((and (not length) (not time)) (setq p (.p.)))
          (t (error "Delay spec error in ~a" b)))
    (cond ((numberp max-length) nil)
          ((numberp max-time) (setq max-length (* sr max-time)))
          ((numberp length) (setq max-length length))
          ((numberp time) (setq max-length (* sr time)))
          ((error "Max-length of ~a not specified" b)))
    (setf (max-length b) max-length
          (min-length b) min-length)
    (setf (limit+ fract) max-length)
    (when (and (not length) (or time max-time)))
    (setf (scaler fract) sr)
    (-> p (in fract))
    (when (typep p '.p.) (push (param p) (params b)))
    (setf (outputs b) (outputs fract))))

(defun .dtap (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.dtap rest))
          

#|
(inspect (.dtap :max-length 5.0))
(inspect (.dtap :max-time 0.001))
(inspect (.dtap :length 5.0))
(inspect (.dtap :time 0.001))
(inspect (make-instance '.delaybase))
|#

;;; DELAYBASE

(defclass .delaybase (delayed-block basic-block)
  ((taps :initarg :taps :accessor taps)
   (offset :initform nil :accessor offset)
   (state :initform nil :accessor state)
   (ssize :initarg :ssize :accessor ssize)
   (dsize :initarg :dsize :accessor dsize))
  (:default-initargs
    :taps nil
    :ssize nil
    :dsize 10))

(defmethod initialize-instance :after ((b .delaybase) &key)
  (let* ((offs (make-instance 'indexvar
                 :name 'offs :value nil
                 :host-block b :size '(1))))
    (setf (offset b) offs)
    (push offs (variables b))
    (setf (inputs b) 
          (list (make-instance 'input
                  :name 'sigin
                  :host-block b)))))

(defmethod finalize-sizes :before ((b .delaybase))
  (let* ((ssiz (+ (ssize b) (dsize b) 5))
         (type (datatype (out b)))
         (st (make-instance 'blockvar0+
               :size (list ssiz)
               :type type :value nil
               :host-block b)))
    (setf (state b) st)
    (push st (variables b))))

(defmethod c-code ((b .delaybase) &optional (stream t))
  (let* ((in (cref (in b)))
         (offs (cref (offset b)))
         (state (cref (state b)))
         (dsiz (dsize b))
         (slen (ssize b))
         (slen-1 (1- slen))
         (slen-1+dsiz (+ slen-1 dsiz)))
    (with-c stream
      "if (|offs|<=0) {RL_1 = |slen-1+dsiz|;"
      "for (RL_0=|slen-1|; RL_0>=0;)"
      "{|state|[RL_1--] = |state|[RL_0--];}"
      "|offs| = |dsiz|;}"
      "|state|[|offs|] = |in|;")
    (loop for tap in (taps b)
          for i from 0
          for cs = (cref (out tap 1))
          for xi = (cref (out tap 0))
          for out = (cref (out b i))
          for len = (1+ (order tap))
          do (format stream "~%/* TAP~a */" i)
          do (if (= (order tap) 0)
               (with-c stream
                 "RL_1 = |offs|+|xi|;"
                 "{RD_0 = |state|[RL_1++];}"
                 "|out| = RD_0;")
               (with-c stream
                 "RL_1 = |offs|+|xi|; RD_0 = 0.0;"
                 "for (RL_0=0; RL_0<|len|;RL_0++)"
                 "{RD_0 += |state|[RL_1++]*|cs|[RL_0];}"
                 "|out| = RD_0;")))
    (with-c stream
      "|offs| -= 1;")))

(defmethod lcode ((b .delaybase)) ;;; enough for initialization
  nil)


#|
(defmethod mcode ((b .delaybase) &optional (stream t))
  ....
|#


;;; .TAP-DELAY

(defclass .tap-delay (macro-block) ())

(defmethod initialize-macro-block ((b .tap-delay) &key
                                       taps)
  (let* (db pars outputs (max-length 0.0d0))
    (loop for tap in taps
          for i from 0
          for out = (make-instance 'output
                      :name (format nil "TAP~a" i)
                      :host-block b)
          for mlen = (max-length tap)
          do (when (params tap) (push (param tap) pars))
          do (setf max-length (max mlen max-length))
          do (setf (host-patch tap) b)
          do (push tap (block-items b))
          do (push out outputs))
    (setf (outputs b) (reverse outputs))
    (setf (params b) (reverse pars))
    (setq db (make-instance '.delaybase
               :taps taps :ssize (ceiling max-length)))
    (setf (outputs db) outputs)
    (loop for out in outputs
          do (setf (host-block out) db))
    (setf (inputs b) (inputs db))
    (loop for tap in taps
          for inx = (make-instance 'input :host-block db)
          do (-> (out tap) inx)
          do (push inx (inputs db)))
    (setf (inputs db) (reverse (inputs db)))))

(defun .tap-delay (taps &rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.tap-delay
         :taps taps rest))

(defun .delay (&rest rest &key length time
                      max-length max-time
                      (order 3) (decimate 1))
  (declare (dynamic-extent rest))
  (apply #'make-instance '.tap-delay
         :taps (list (.dtap :length length :time time
                            :max-length max-length
                            :max-time max-time
                            :order order :decimate decimate))
         rest))


;;; .D (UNIT DELAY)

(defclass .d (delay-mixin) ())

(defmethod lcode ((b .d))
  (let* ((type (get (datatype (out b)) 'ltype)))
    (arr-op (lvar (fwd-out (link (in b))))
            (lvar (out b)) '+ type)))

(def.c .d "OUT_0 = IN_0;")

(defmethod mcode ((b .d) &optional (stream t))
  (format stream "~%~a = ~a;"
          (varname (out b))
          (varname (prev-out (in b)))))

(defun .d (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.d rest))


;;; Delay with negation

(defclass .d- (delay-mixin) ())

(defmethod lcode ((b .d-))
  (let* ((type (get (datatype (out b)) 'ltype)))
    (arr-op (lvar (fwd-out (link (in b))))
            (lvar (out b)) '- type)))

(def.c .d- "OUT_0 = -IN_0;")

(defmethod mcode ((b .d-) &optional (stream t))
  (format stream "~%~a = -~a;"
          (varname (out b))
          (varname (prev-out (in b)))))

(defun .d- (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.d- rest))


;;; .DN (N-LENGTH DELAY)

(defclass .dn (delay-mixin)
  ((dlines :initform nil :accessor dlines)
   (offs :initform nil :accessor offs)
   (dlen :initarg :delay-length :accessor dlen))
  (:default-initargs
    :delay-length nil))

(defmethod initialize-instance :after ((b .dn) &key)
  (when (integerp (dlen b))
    (setf (dlen b) (list (dlen b)))))

(defmethod dline-size ((b .dn) d)
  (- (* 2 d) 3))

(defmethod finalize-types :after ((b .dn) &aux vars offs type)
  (unless (and (consp (dlen b)) (integerp (first (dlen b))))
    (error "Invalid delay-length spec ~a for ~a" (dlen b) b))
  (setq type (datatype (out b)))
  (loop for d in (dlen b)
        for n from 0
        for v = (make-instance 'blockvar0
                  :name n :size (list (dline-size b d))
                  :type type :value nil :host-block b)
        do (push v vars))
  (setq vars (reverse vars))
  (setf (dlines b) vars)
  (setq offs (make-instance 'indexvar
               :name 'offs :value nil
               :size (list (length (dlen b)))
               :host-block b))
  (setf (offs b) (list offs))
  (setf (variables b) (cons offs vars)))

(defmethod finalize-sizes :after ((b .dn))
  (let* ((dl (dlen b)))
    (unless (= (length dl) (first (datasize (out b))))
      (error "Incompatible :dlen and datasize in ~a" b))))


(defmethod lcode ((b .dn)) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ????
  nil)

(defmethod mcode ((b .dn) &optional (stream t))
  (let* ((onam (varname (out b)))
         (innam (varname (prev-out (in b)))))
    (loop for dl in (dlines b)
          for len in (dlen b)
          for off = (first (offs b))
          for k from 1
          for dlnam = (varname dl)
          for offnam = (varname off)
          do (format stream "~%~a(~a) = ~a(~a(~a));"
                     onam k dlnam offnam k)
          do (format stream " ~a(~a(~a)) = ~a(~a);"
                     dlnam offnam k innam k)
          do (format stream "~%if(~a(~a)<=1) ~a(~a) = ~a;"
                     offnam k offnam k (1- len))
          do (format stream " else ~a(~a) = ~a(~a)-1; end"
                     offnam k offnam k))))

(defun .dn (len &rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.dn :delay-length len rest))



;;; .ndelay+ (unit delay backbone)

(defclass .ndelay+ (delay-mixin siso-block) ())

(defmethod initialize-instance :after ((b .ndelay+) &key)
  (setf (params b)
        (list (make-instance 'param :name 'S1)
              (make-instance 'param :name 'S21))))

(defmethod check-sizes ((b .ndelay+))
  (unless (>= (elt (datasize (out b)) 0) 1)
    (error "Datasize error in ~a"  b)))

(defmethod set-size :after ((b .ndelay+) size)
  (unless (= (length size) 1)
    (error "Datasize error in ~a" b))
  (decf (elt (datasize (out b)) 0)))

(defmethod include-size-inputs ((b .ndelay+)) nil)

(defmethod c-code ((b .ndelay+) &optional (stream t))
  (let* ((out (out b))
         (siz (datasize out))
         (siz-2 (- (first siz) 2))
         (arr (cref out))
         (in (cref (in b))))
    (with-c stream
      "for (LI=|siz-2|; LI>=0; LI--)"
      "{|arr|[LI+1] = |arr|[LI];}"
      "|arr|[0] = |in|;")))

(defmethod mcode ((b .ndelay+) &optional (stream t))
  (let* ((onam (varname (out b)))
         (innam (varname (prev-out (in b)))))
    (format stream "~%~a(2:end) = ~a(1:end-1);"
            onam onam)
    (format stream "~%~a(1) = ~a(1);" onam innam)))

(defmethod lcode ((b .ndelay+)) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ????
  nil)


;;; .dot+1 (dot product with offset of one)

(defclass .dot+1 (siso-block) ())

(defmethod initialize-instance :after ((b .dot+1) &key)
  (setf (params b)
        (list (make-instance 'param :name 'coeffs))))

(defmethod check-sizes ((b .dot+1))
  (let* ((isiz (datasize (prev-out (in b))))
         (psiz (datasize (prev-out (param b)))))
    (unless (and (= (length isiz) 1)
                 (= (length psiz) 1))
      (error "Datasize error in ~a" b))
    (unless (<= (car psiz) (+ (car isiz) 1))
      (error "Datasize error in ~a" b))))

(defmethod set-size ((b .dot+1) size)
  (unless (= (length size) 1)
    (error "Datasize error in ~a" b))
  (setf (datasize (out b)) '(1)))

(defmethod c-code ((b .dot+1) &optional (stream t))
  (let* ((out (cref (out b)))
         (siz (datasize (prev-out (param b))))
         (siz-1 (- (first siz) 1))
         (par (cref (param b)))
         (in (cref (in b))))
    (with-c stream
      "RD_1 = 0.0; LJ = 1;"
      "for (LI=0; LI<|siz-1|; LI++)"
      "{RD_1 += |in|[LI]*|par|[LJ++];}"
      "|out| = RD_1;")))

(defmethod mcode ((b .dot+1) &optional (stream t))
  (let* ((onam (varname (out b)))
         (pnam (varname (prev-out (param b))))
         (innam (varname (prev-out (in b))))
         (siz (datasize (prev-out (param b))))
         (size (first siz)))
    (format stream "~%~a(1) = ~a(1:~a)*~a(2:~a);"
            onam innam size pnam size)))

(defmethod lcode ((b .dot+1)) ; ???????????
  nil)


#|
;;; Rational impedance

(defclass 
|#

#|
(defpatch koe ((nd (make-instance '.ndelay+))
               (x (.var '(1.0 1.0 1.0))))
  (-> x (param nd 0))
  (-> (.var '(1.0 1.0 1.0 1.0)) (param nd 1))
  (-> (.imp1 1.0) nd (.probe "out")))

(inspect koe)
(c-code koe)
(mcode koe)
(load-patch koe)
(step-patch koe)

(inspect (make-instance '.dot+1))

(defpatch koe ((nd (make-instance '.dot+1))
               (p (.var '(1.0 1.0 1.0 1.0))))
  (-> p (param nd 0))
  (-> (.var '(1.0 1.0 1.0)) nd (.probe "out")))
|#


;;; FILTERS
;;; =======

;;; .FIR filter
;;; -----------
;;; input & output dimension 1 (scalar)
;;; Realized with periodically shifted 
;;; ring buffering and offset pointer
;;; Coefficient vector coeff bs size >=1

(defclass .fir% (dynamic-block siso-block)
  ((state  :initform nil :accessor state)
   (offs :initform nil :accessor offs)
   (ssize :initarg :size :accessor ssize)
   (dsize :initarg :dsize :accessor dsize)
   (cbeg :initarg :cbeg :accessor cbeg))
  (:default-initargs
    :size nil
    :dsize 10
    :cbeg 0))

(defmethod initialize-instance :after ((b .fir%) &key)
  (setf (datasize (out b)) '(1))
  (setf (params b)
        (list (make-instance 'param 
                :name 'coeffs :host-block b))))

(defmethod include-size-params ((b .fir%)) nil)

(defmethod finalize-types :after ((b .fir%))
  (let* ((offs (make-instance 'indexvar
                 :name 'offs :value nil
                 :host-block b :size '(1))))
    (setf (offs b) offs)
    (push offs (variables b))))

(defmethod finalize-sizes :after ((b .fir%))
  (finalize-sizes% b))

(defmethod finalize-sizes% ((b .fir%))
  (unless (> (first (datasize (prev-out (param b))))
             (1+ (cbeg b)))
    (error "FIR coeffs must contain at least ~a taps"
           (+ 2 (cbeg b))))
  (let* ((coeffs (prev-out (param b)))
         (n (- (first (datasize coeffs)) (cbeg b)))
         (type (datatype (out b)))
         (len (+ n (dsize b)))
         (buf (make-instance 'blockvar0+
                :size (list len)
                :type type :value nil
                :host-block b)))
    (setf (ssize b) n)
    (setf (state b) buf)
    (push buf (variables b))))

(defmethod lcode ((b .fir%))
  (let* ((inv (lvar (prev-out (in b))))
         (outv (lvar (out b)))
         (offs (lvar (offs b)))
         (ss (lvar (state b)))
         (cs (lvar (prev-out (param b))))
         (size (ssize b))
         (dsiz (dsize b)))
    `(progn
       (when (<= (aref ,offs 0) 0)
         (loop for i from 0 below ,size
               do (setf (aref ,ss (+ ,dsiz i))
                        (aref ,ss i)))
         (setf (aref ,offs 0) ,dsiz))
       (setf (aref ,ss (aref ,offs 0)) (aref ,inv 0))
       (loop with res = 0
             with off = (aref ,offs 0)
             for i from 0 below ,size
             for j = (+ i off)
             do (incf res (* (aref ,ss j)
                             (aref ,cs (+ ,(cbeg b) i))))
             finally (setf (aref ,outv 0) res))
       (decf (aref ,offs 0)))))

(defmethod mcode ((b .fir%) &optional (stream t))
  (let* ((onam (varname (out b)))
         (inam (varname (prev-out (in b))))
         (offnam (varname (offs b)))
         (snam (varname (state b)))
         (coeffs (prev-out (param b)))
         (cnam (varname coeffs))
         (size (ssize b))
         (dsiz (dsize b)))
    (format stream "~%if (~a<=1) ~a(~a:~a) = ~a(1:~a);"
            offnam snam (+ 1 dsiz) (+ size dsiz) snam size)
    (format stream " ~a = ~a; end" offnam (+ 1 dsiz))
    (format stream "~%~a(~a) = ~a;" snam offnam inam)
    (format stream " ~a = ~a(~a:~a+~a)'*~a(~a:end);"
            onam snam offnam offnam (1- size) cnam (1+ (cbeg b)))
    (format stream " ~a = ~a-1;" offnam offnam)))

(defmethod c-code ((b .fir%) &optional (stream t))
  (let* ((in (cref (in b)))
         (out (cref (out b)))
         (state (cref (state b)))
         (coeffs (cref (prev-out (param b))))
         (offs (cref (offs b)))
         (dsiz (dsize b))
         (len (ssize b))
         (len-1 (1- len))
         (len-1+dsiz (+ len-1 dsiz)))
    (with-c stream
      "if (|offs|<=0) {RL_1 = |len-1+dsiz|;"
      "for (RL_0=|len-1|; RL_0>=0;)"
      "{|state|[RL_1--] = |state|[RL_0--];}"
      "|offs| = |dsiz|;}"
      "|state|[|offs|] = |in|;"
      "RL_1 = |offs|; RL_2 = |(cbeg b)|; RD_0 = 0.0;"
      "for (RL_0=0; RL_0<|len|;RL_0++)"
      "{RD_0 += |state|[RL_1++]*|coeffs|[RL_2++];}"
      "|out| = RD_0; |offs| -= 1;")))

(defclass .fir (macro-block) ())

(defmethod initialize-macro-block ((b .fir) &key 
                                       coeffs arg
                                       (cbeg 0)
                                       (dsize 10))
  (let* ((f% (make-instance '.fir% :dsize dsize :cbeg cbeg)))
    (add-param b coeffs (param f%) :name 'coeffs)
    (if arg (-> arg (in f%))
        (setf (inputs b) (list (in f%))))
    (setf (outputs b) (list (out f%)))))
    
(defun .fir (&rest rest)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (apply #'make-instance '.fir :arg (car rest) (cdr rest))
    (apply #'make-instance '.fir rest)))


;;; .SFIR filter (Sparse FIR filter)
;;; --------------------------------------
;;; FIR filter with sparsely positioned taps

(defclass .sfir% (.fir%) ()
  (:default-initargs 
    :dsize 300))

(defmethod initialize-instance :after ((b .sfir%) &key)
  (setf (params b)
        (append (params b)
                (list (make-instance 'param 
                        :name 'pos :host-block b)))))

(defmethod finalize-sizes% ((b .sfir%))
  (unless (> (first (datasize (prev-out (param b 'coeffs)))) 1)
    (error "SFIR coeffs must contain at least two taps"))
  (let* ((coeffs (prev-out (param b 'coeffs)))
         (pos (prev-out (param b 'pos))))
    (unless (member (datatype pos) '(.long .short))
      (error "Index vector of ~a not of integer type" b))
    (unless (equal (datasize coeffs) (datasize pos))
      (error "Incompatibility of param sizes in ~a" b))
    (let* ((n (ssize b))
           (type (datatype (out b)))
           (len (+ n (dsize b)))
           (buf (make-instance 'blockvar0+
                  :size (list len)
                  :type type :value nil
                  :host-block b)))
      (setf (state b) buf)
      (push buf (variables b)))))

(defmethod lcode ((b .sfir%))
  (let* ((inv (lvar (prev-out (in b))))
         (outv (lvar (out b)))
         (offs (lvar (offs b)))
         (ss (lvar (state b)))
         (cs (lvar (prev-out (param b 'coeffs))))
         (is (lvar (prev-out (param b 'pos))))
         (size (ssize b))
         (csize (length cs))
         (dsiz (dsize b)))
    `(progn
       (when (<= (aref ,offs 0) 0)
         (loop for i from 0 below ,size
               do (setf (aref ,ss (+ ,dsiz i))
                        (aref ,ss i)))
         (setf (aref ,offs 0) ,dsiz))
       (setf (aref ,ss (aref ,offs 0)) (aref ,inv 0))
       (loop with res = 0
             with off = (aref ,offs 0)
             for i from 0 below ,csize
             for k = (aref ,is i)
             for j = (+ k off)
             do (incf res (* (aref ,ss j) (aref ,cs i)))
             finally (setf (aref ,outv 0) res))
       (decf (aref ,offs 0)))))

(defmethod mcode ((b .sfir%) &optional (stream t))
  (let* ((onam (varname (out b)))
         (inam (varname (prev-out (in b))))
         (offnam (varname (offs b)))
         (snam (varname (state b)))
         (coeffs (prev-out (param b 'coeffs)))
         (pos (varname (prev-out (param b 'pos))))
         (cnam (varname coeffs))
         (size (ssize b))
         (dsiz (dsize b)))
    (format stream "~%if (~a<=1) ~a(~a:~a) = ~a(1:~a);"
            offnam snam (+ 1 dsiz) (+ size dsiz) snam size)
    (format stream " ~a = ~a; end" offnam (+ 1 dsiz))
    (format stream "~%~a(~a) = ~a;" snam offnam inam)
    (format stream " ~a = ~a(~a+~a)'*~a;"
            onam snam offnam pos cnam)
    (format stream " ~a = ~a-1;" offnam offnam)))

(defmethod c-code ((b .sfir%) &optional (stream t))
  (let* ((in (cref (in b)))
         (out (cref (out b)))
         (state (cref (state b)))
         (co (prev-out (param b 'coeffs)))
         (cs (cref co))
         (is (cref (prev-out (param b 'pos))))
         (offs (cref (offs b)))
         (csiz (length (lvar co)))
         (dsiz (dsize b))
         (len (ssize b))
         (len-1 (1- len))
         (len-1+dsiz (+ len-1 dsiz)))
    (with-c stream
      "if (|offs|<=0) {RL_1 = |len-1+dsiz|;"
      "for (RL_0=|len-1|; RL_0>=0;)"
      "{|state|[RL_1--] = |state|[RL_0--];}"
      "|offs| = |dsiz|;}"
      "|state|[|offs|] = |in|;"
      "RL_1 = |offs|; RD_0 = 0.0;"
      "for (RL_0=0; RL_0<|csiz|; RL_0++)"
      "{RL_2 = |is|[RL_0] + RL_1;"
      "RD_0 += |state|[RL_2]*|cs|[RL_0];}"
      "|offs| -= 1; |out| = RD_0;")))


(defclass .sfir (macro-block) ())

(defmethod initialize-macro-block ((b .sfir) &key
                                       coeffs pos arg
                                       size (dsize 300))
  (let* ((f% (make-instance '.sfir% :size size :dsize dsize)))
    (add-param b coeffs (param f% 'coeffs) :name 'coeffs)
    (add-param b pos (param f% 'pos) :name 'pos)
    (if arg (-> arg (in f%))
        (setf (inputs b) (list (in f%))))
    (setf (outputs b) (list (out f%)))))
    
(defun .sfir (&rest rest)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (apply #'make-instance '.sfir :arg (car rest) (cdr rest))
    (apply #'make-instance '.sfir rest)))


;;; .IIR filter
;;; -----------
;;; input & output dimension 1 (scalar)
;;; Realized with periodically shifted 
;;; ring buffering and offset pointer
;;; Numerator bs size >=1, denom as size >1
;;; as[0] can be different from 1.0

(defclass .iir% (.fir%) ())
  
(defmethod initialize-instance :after ((b .iir%) &key)
  (setf (datasize (out b)) '(1))
  (let* ((as (make-instance 'param 
               :name 'as :host-block b))
         (/a0 (make-instance 'param
                :name '/a0 :host-block b))
         (bs (first (params b))))
    (setf (name bs) 'bs)
    (setf (params b) (list bs as /a0))))

(defmethod include-size-params ((b .iir%)) nil)

(defmethod finalize-sizes% ((b .iir%))
  (unless (> (first (datasize (prev-out (param b 'as)))) 1)
    (error "IIR denominator must be of length two or more"))
  (let* ((bs (prev-out (param b 'bs)))
         (as (prev-out (param b 'as)))
         (bn (first (datasize bs)))
         (an (first (datasize as)))
         (n (max bn an))
         (type (datatype (out b)))
         (len (+ n (dsize b)))
         (buf (make-instance 'blockvar0+
                :size (list len)
                :type type :value nil
                :host-block b)))
    (setf (ssize b) n)
    (setf (state b) buf)
    (push buf (variables b))))

(defmethod lcode ((b .iir%))
  (let* ((inv (lvar (prev-out (in b))))
         (outv (lvar (out b)))
         (offs (lvar (offs b)))
         (ssv (lvar (state b)))
         (bs (prev-out (param b 'bs)))
         (bsv (lvar bs))
         (as (prev-out (param b 'as)))
         (asv (lvar as))
         (/a0 (prev-out (param b'/a0)))
         (/a0v (lvar /a0))
         (sizeb (first (datasize bs)))
         (sizea (first (datasize as)))
         (size (max sizea sizeb))
         (dsiz (dsize b)))
    `(progn
       (when (<= (aref ,offs 0) 0)
         (loop for i from 0 below ,size
               do (setf (aref ,ssv (+ ,dsiz i))
                        (aref ,ssv i)))
         (setf (aref ,offs 0) ,dsiz (aref ,offs 0) ,dsiz))
       ;;;  (setf (aref ,/a0v 0) (/ 1.0d0 (aref ,asv 0)))  ;;;  ???
       (loop with res = 0
             for i from 1 below ,sizea
             for j = (+ i (aref ,offs 0))
             do (decf res (* (aref ,ssv j) (aref ,asv i)))
             finally 
             (incf res (aref ,inv 0))
             (setq res (* res (aref ,/a0v 0)))
             (setf (aref ,ssv (aref ,offs 0)) res))
       (loop with res = 0
             for i from 0 below ,sizeb
             for j = (+ i (aref ,offs 0))
             do (incf res (* (aref ,ssv j) (aref ,bsv i)))
             finally (setf (aref ,outv 0) res))
       (decf (aref ,offs 0)))))

(defmethod mcode ((b .iir%) &optional (stream t))
  (let* ((onam (varname (out b)))
         (inam (varname (prev-out (in b))))
         (offnam (varname (offs b)))
         (snam (varname (state b)))
         (/a0nam (varname (prev-out (param b '/a0))))
         (bs (prev-out (param b 'bs)))
         (as (prev-out (param b 'as)))
         (bnam (varname bs))
         (anam (varname as))
         (sizeb (first (datasize bs)))
         (sizea (first (datasize as)))
         (size (max sizea sizeb))
         (dsiz (dsize b)))
    (format stream "~%if (~a<=1) ~a(~a:~a) = ~a(1:~a);"
            offnam snam (+ 1 dsiz) (+ size dsiz) snam size)
    (format stream " ~a = ~a; end" offnam (+ 1 dsiz))
    (format stream "~%~a(~a) = (~a-~a(~a+1:~a+~a)'*~a(2:end))*~a;"
            snam offnam inam snam offnam offnam (1- sizea) anam /a0nam)
    (format stream "~%~a = ~a(~a:~a+~a)'*~a;"
            onam snam offnam offnam (1- sizeb) bnam)
    (format stream "~%~a = ~a-1;" offnam offnam)))

(defmethod c-code ((b .iir%) &optional (stream t))
  (let* ((in (cref (prev-out (in b))))
         (out (cref (out b)))
         (state (cref (state b)))
         (as (cref (prev-out (param b 'as))))
         (bs (cref (prev-out (param b 'bs))))
         (/a0 (cref (prev-out (param b'/a0))))
         (offs (cref (offs b)))
         (dsiz (dsize b))
         (aslen (first (datasize (prev-out (param b 'as)))))
         (bslen (first (datasize (prev-out (param b 'bs)))))
         (len-1 (1- (ssize b)))
         (len-1+dsiz (+ len-1 dsiz)))
    (with-c stream
      "if (|offs|<=0) {RL_1 = |len-1+dsiz|;"
      "for (RL_0=|len-1|; RL_0>=0;)"
      "{|state|[RL_1--] = |state|[RL_0--];}"
      "|offs| = |dsiz|;}"
      ;
      "RL_1 = |offs|+1; RD_0 = 0.0;"
      "for (RL_0=1; RL_0<|aslen|;)"
      "{RD_0 -= |state|[RL_1++]*|as|[RL_0++];}"
      "RD_0 += |in|; RD_0 *= |/a0|;"
      "|state|[|offs|] = RD_0;")
    ;
    (if (equal (datasize (prev-out (param b 'bs))) '(1))
      (with-c stream
        "RD_0 *= |bs|;")
      (with-c stream
        "RD_0 *= |bs|[0]; RL_1 = |offs|+1;"
        "for (RL_0=1; RL_0<|bslen|;)"
        "{RD_0 += |state|[RL_1++]*|bs|[RL_0++];}"))
    (with-c stream
      "|out| = RD_0; |offs| -= 1;")))


(defclass .iira0 (macro-group macro-block) ())

(defmethod initialize-macro-block ((b .iira0) &key)
  (let* ((p (.select-chan 0))
         (inv (.inv p)))
    (setf (outputs b) (list (out inv))
          (params b) (list (in p)))))

(defclass .iir (macro-block) ())

(defmethod initialize-macro-block ((b .iir) &key 
                                       bs as arg
                                       (dsize 10))
  (let* ((f% (make-instance '.iir% :dsize dsize))
         (bp (.p.)) (ap (.p.))
         (/a0c (make-instance '.iira0)))
    (add-param b as (param ap) :name 'as)
    (-> ap (param f% 'as))
    (add-param b bs (param bp) :name 'bs)
    (-> bp (param f% 'bs))
    (-> ap (param /a0c) (param f% '/a0))
    (if arg (-> arg (in f%))
        (setf (inputs b) (list (in f%))))
    (setf (outputs b) (list (out f%)))))
    
(defun .iir (&rest rest)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (apply #'make-instance '.iir :arg (car rest) (cdr rest))
    (apply #'make-instance '.iir rest)))


;;; .LP1 (LOWPASS OF ORDER 1):

(defclass .lp1% (dynamic-block siso-block)
  ((state :initarg :statep :accessor state)
   (gain :initform nil :accessor gain))
  (:default-initargs
    :statep t))

(defmethod initialize-instance :after ((b .lp1%) &key
                                           gainp)
  (when gainp
    (setf (gain b) 
          (make-instance 'param 
            :host-block b :name 'gain))
    (push (gain b) (params b)))
  (push (make-instance 'param 
          :host-block b :name 'a) (params b)))

(defmethod finalize-sizes :after ((b .lp1%))
  (when (state b)
    (setf (state b)
          (let* ((st (make-instance 'blockvar0
                       :size (datasize (out b)) :value nil
                       :type (datatype (out b)) :host-block b)))
            (push st (variables b))
            (setf (state b) st)))))

(defmethod lcode ((b .lp1%)) ;;; ????
  nil)

(defmethod mcode ((b .lp1%) &optional (stream t)) ;;; ????
  stream nil)

(defmethod c-code ((b .lp1%) &optional (stream t))
  (let* ((in (cref (in b)))
         (out (cref (out b)))
         (st (cref (state b)))
         (a (cref (param b 'a)))
         (g (gain b))
         (dim (datasize (out b))))
    (cond ((equal dim '(1))
           (let* ((gstr (if g (format nil "*~a" (cref g)) "")))
             (with-c stream
               "RD_0 = |a|;"
               "RD_1 = (1.0-RD_0)*|in||gstr|;"
               "RD_1 += RD_0*|st|;"
               "|out| = RD_1; |st| = RD_1;")))
          ((= (length dim) 1)
           (let* ((n (first dim))
                  (gstr (if g (format nil "*~a[RL_0]" (cref g)) "")))
             (with-c stream
               "for (RL_0=0; RL_0<|n|; RL_0++)"
               "{RD_0 = |a|[RL_0];"
               "RD_1 = (1.0-RD_0)*|in|[RL_0]|gstr|;"
               "RD_1 += RD_0*|st|[RL_0];"
               "|out|[RL_0] = RD_1;"
               "|st|[RL_0] = RD_1;}")))
          (t (error "Dimension error in ~a" b)))))


(defclass .lp1* (macro-block) ())
           
(defmethod initialize-macro-block ((b .lp1*) &key 
                                       a gain arg)
  (let* ((f% (make-instance '.lp1% :gainp gain)))
    (add-param b a (param f% 'a) :name 'a)
    (cond ((eq gain t)
           (add-param b nil (param f% 'gain) :name 'gain))
          ((eq gain nil))
          (t (add-param b gain (param f% 'gain) :name 'gain)))
    (if arg (-> arg (in f%))
        (setf (inputs b) (list (in f%))))
    (setf (outputs b) (list (out f%)))))
    
(defun .lp1* (&rest rest)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (apply #'make-instance '.lp1* :arg (car rest) (cdr rest))
    (apply #'make-instance '.lp1* rest)))


;;; MACRO-BLOCK FOR .LP1

#|
(defmethod initialize-macro-block ((b .lp1) &key
                                       arg a freq gain
                                       (delayed nil))
  (let* ((d (.d))
         (g nil)
         (add (.add))
         (mul1 (.mul))
         (mul2 (.mul))
         sub)
    (cond ((or (eq a t) (and (not a) (not freq)))
           (let* ((p (.p. :name 'a))
                  (1s (.datac 1.0d0)))
         ;    (setf (name (param p)) 'a)
             (-> p 1s) (push (param p) (params b))
             (-> p (in mul2 1))
         ;    (setq sub )
             (-> (.sub 1s p) (in mul1 1))))

          (a (setq a (.val a))
             (-> a (in mul2 1))
             (setq sub (.sub 1.0d0 a))
             (-> sub (in mul1 1)))
          ((eq freq t)
           (let* ((p (.p. :name 'freq))
                  (c (.coeff (/ (* 2.0d0 pi) (srate b)))))
        ;     (setf (name (param p)) 'freq)
             (push (param p) (params b))
             (-> p c (in mul1 1))
             (setq sub (.sub 1.0d0 c))
             (-> sub (in mul2 1))))
          (freq
           (let* ((c (.coeff (/ (cl:float (* 2.0 pi) 1.0d0) (srate b)))))
             (-> (.val freq) c (in mul1 1))
             (setq sub (.sub 1.0d0 c))
             (-> sub (in mul2 1)))))
    (-> add d mul2 (in add 1))
    
    (cond ((not gain)
           (if delayed
             (push (out d) (outputs b))
             (push (out add) (outputs b))))
          ((eq gain t)
           (setq g (.mul))
           (let* ((p (.p.)))
             (setf (name (param p)) 'gain)
             (setf (params b)
                   (append (params b) (list (param p))))
             (-> p (in g 1))
             (if delayed (-> d g) (-> add g))
             (push (out g) (outputs b))))
          (t (let* ((g (.mul)))
               (-> (.val gain) (in g 1))
               (if delayed (-> d g) (-> add g))
               (push (out g) (outputs b)))))
    ;
    (cond (arg (-> (.val arg) mul1 add))
          (t (setf (name (in mul1)) 'sigin)
             (-> mul1 add)
             (push (in mul1) (inputs b))))
    (setf (name (out b)) 'sigout)))
|#

#|
(defmethod initialize-macro-block ((b .lp1) &key
                                       arg a freq gain
                                       (delayed nil))
  (let* ((d (.d)) (sub (.sub)) (addg (.add))
         (mul1 (.mul :inputs (if gain 3 2)))
         (mul2 (.mul :inputs (if freq 3 2))))
    (setf (name (in mul1)) 'sigin)
    (if arg (-> (.val arg) mul1)
        (push (in mul1) (inputs b)))
    (-> mul1 sub d mul2 (in sub 1))
    (push (if delayed (out d) (out sub)) (outputs b))
    (when gain
      (let* ((g (.p. :name 'gain)))
        (add-param b gain (param g))
        (-> g (in mul1 2))))
|#

;;; Lowpass1 b/(1+az^-1)

(defclass .lp1ba (macro-block) ())

(defmethod initialize-macro-block ((block .lp1ba) &key
                                       arg a b)
  (let* ((d (.d)) (sub (.sub))
         (mul2 (.mul)) (mul1 (.mul))
         (pa (.p. :name 'a))
         (pb (.p. :name 'b)))
    (setf (name (in mul1)) 'sigin)
    (if arg (-> (.val arg) mul1)
        (push (in mul1) (inputs block)))
    (-> mul1 sub d mul2 (in sub 1))
    (push (out sub) (outputs block))
    (-> pa (in mul2 1))
    (-> pb (in mul1 1))
    (add-param block a (param pa))
    (add-param block b (param pb))))


;;; Lowpass2 (b+cz^{-1})/(1+bz^{-1}-cz^{-2})

(defclass .lp2bd (macro-block) ())

(defmethod initialize-macro-block ((block .lp2bd) &key
                                       arg b c)
  (let* ((d (.d)) (sub (.sub))
         (mul2 (.mul)) (mul1 (.mul))
         (pc (.p. :name 'c))
         (pb (.p. :name 'b)))
    (setf (name (in mul1)) 'sigin)
    (if arg (-> (.val arg) mul1)
        (push (in mul1) (inputs block)))
    (-> mul1 sub d mul2 (in sub 1))
    (push (out sub) (outputs block))
    (-> pc (in mul2 1))
    (-> pb (in mul1 1))
    (add-param block c (param pc))
    (add-param block b (param pb))))



#|
(defclass .lp1m (macro-block) ())

(defmethod initialize-macro-block ((b .lp1m) &key
                                       arg a gain
                                       (delayed nil))
  (let* ((d (.d)) (sub (.sub))
         (addg (.add)) (mul2 (.mul))
         (mul1 (.mul :inputs (if gain 3 2)))
         (1s (.datac 1.0d0)) (pa (.p. :name 'a)))
    (setf (name (in mul1)) 'sigin)
    (if arg (-> (.val arg) mul1)
        (push (in mul1) (inputs b)))
    (-> mul1 sub d mul2 (in sub 1))
    (push (if delayed (out d) (out sub)) (outputs b))
    (-> pa (in mul2 1))
    (-> pa addg (in mul1 1))
    (-> pa 1s (in addg 1))
    (when gain
      (let* ((pg (.p. :name 'gain)))
        (add-param b gain (param pg))
        (-> pg (in mul1 2))))
    (add-param b a (param pa))))
|#

#|
(inspect (make-instance '.lp1m))
(inspect (make-instance '.lp1m :arg 1.0))
(inspect (make-instance '.lp1m :a -0.9))
(inspect (make-instance '.lp1m :a -0.9 :gain 1.0))

(defpatch p ((lp (make-instance '.lp1m :a -0.9)))
  (-> (.var 1.0) lp (.probe "out")))

(defpatch p ((lp (make-instance '.lp1m :a -0.9 :gain 2.0)))
  (-> (.var 1.0) lp (.probe "out")))

(to-matlab p)
|#




(defun .lp1 (&rest rest)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (apply #'make-instance '.lp1 :arg (car rest) (cdr rest))
    (apply #'make-instance '.lp1 rest)))

#|
(defpatch lp ((lp (.lp1 :a 0.5)))
  (-> (.imp1 1.0) lp (.probe "out")))

(defpatch lp ((lp (.lp1 :a 0.5 :gain 0.5)))
  (-> (.imp1 1.0) lp (.probe "out")))

(defpatch lp ((lp (.lp1 :freq (* 0.5 11050.0))))
  (-> (.imp1 1.0) lp (.probe "out")))

(defpatch lp ((lp (.lp1 :a t :gain t)))
  (-> (.var 0.5) (param lp 'a))
  (-> (.var 0.5) (param lp 'gain))
  (-> (.imp1 1.0) lp (.probe "out")))

(matlab-response lp ;;; To Matlab
                 :samples (* 16 1024)
                 :outputs '("out")
                 :post "
figure(10); clf;
plot_spect(out_resp,SRATE); grid on; 
xlabel('Freq [Hz]');
ylabel('Amplitude');
title('.lp1 response');")

(inspect lp)
(c-code lp)


(inspect (.lp1 1.0 :a 0.5))
(inspect (.lp1 1.0 :a 0.5 :gain 0.7))
(inspect (.lp1 1.0 :a 0.5 :gain t))
|#


;;; .AP1 (ALLPASS, ORDER 1):

;;; .AP1 (macro-based, negative a param)
;;;  H(z) = (z^{-1}-a)/)(1-az^{-1})

(defclass .ap1- (macro-block) ())

(defmethod initialize-macro-block ((b .ap1-)
                                       &key a arg)
  (let* ((add (.add))
         (sub (.sub))
         (mul1 (.mul))
         (mul2 (.mul))
         (d (.d)) (p (.p.)))
    (add-param b a (param p) :name 'a)
    (-> d mul1 (in add 1) mul2 (in sub 1))
    (-> add d (in sub 0))
    (-> p (in mul1 1))
    (-> p (in mul2 1))
    (setf (name (in add)) 'sigin)
    (cond (arg (-> (.val arg) add))
          (t (setf (inputs b) (list (in add)))))
    (setf (name (out sub)) 'sigout)
    (setf (outputs b) (list (out sub)))))

(defun .ap1- (&rest rest)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (apply #'make-instance '.ap1- :arg (car rest) (cdr rest))
    (apply #'make-instance '.ap1- rest)))


;;; .AP1 (macro-based, positive a param)
;;;  H(z) = (a+z^{-1})/)(1+az^{-1})

(defclass .ap1 (macro-block) ())

(defmethod initialize-macro-block ((b .ap1)
                                       &key a arg)
  (let* ((add (.add))
         (sub (.sub))
         (mul1 (.mul))
         (mul2 (.mul))
         (d (.d)) (p (.p.)))
    (add-param b a (param p) :name 'a)
    (-> d mul1 (in sub 1) mul2 (in add 1))
    (-> sub d (in add 0))
    (-> p (in mul1 1))
    (-> p (in mul2 1))
    (setf (name (in sub)) 'sigin)
    (cond (arg (-> (.val arg) sub))
          (t (setf (inputs b) (list (in sub)))))
    (setf (name (out add)) 'sigout)
    (setf (outputs b) (list (out add)))))

(defun .ap1 (&rest rest)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (apply #'make-instance '.ap1 :arg (car rest) (cdr rest))
    (apply #'make-instance '.ap1 rest)))

#|
(defpatch px ((ap (.ap1 :a 0.5)))
  (-> (.imp1) ap (.probe "out")))
|#

#|
;;; .ap1% block primitive

(defclass .ap1% (siso-block) ())

(defmethod initialize-instance :after ((b .ap1%) &key)
  (setf (params b) (list (make-instance 'param :host-block b))))

(defmethod finalize-types :after ((b .ap1%))
  (let* ((out (out b))
         (var (make-instance
                'blockvar :name 'state
                :value nil :host-block b)))
    (setf (datatype var) (datatype out))
    (setf (variables b) (list var))))

(defmethod finalize-sizes :after ((b .ap1%) )
  (setf (datasize (first (variables b)))
        (datasize (out b)))
  (unless (equal (datasize (out b))
                 (datasize (prev-out (param b))))
    (error "Param-size not compatible with out-size in ~a" b)))

#|
(defmethod lcode ((b .ap1%)) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;  WWWWW
  (let* ((type (get (datatype (out b)) 'ltype)))
    (arr-op (lvar (fwd-out (link (in b))))
            (lvar (out b)) '+ type)))
|#

(defmethod lcode ((b .ap1%)) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;  WWWWW
  nil)

(defmethod mcode ((b .ap1%) &optional (stream t))
  (let* ((varname (varname (first (variables b))))
         (parname (varname (prev-out (param b))))
         (inname (varname (prev-out (in b))))
         (tempname (format nil "~aX" varname))
         (outname (varname (out b))))
    (format stream "~%~a = ~a+~a.*~a;"
            tempname inname parname varname)
    (format stream " ~a = ~a-~a.*~a;"
            outname varname tempname parname)
    (format stream " ~a = ~a;"
            varname tempname)))


;;; .AP1 (ALLPASS order 1, macro)

(defclass .ap1 (macro-block) ())

(defmethod initialize-macro-block ((b .ap1) &key
                                       arg a)
  (let* ((ap (make-instance '.ap1%)))
    (setf (outputs b) (list (out ap 0))
          (inputs b) (list (in ap 0)))
    (cond ((null a)
           (setf (params b) (list (param ap))))
          ((or (typep a 'output)
               (typep a 'basic-block))
           (-> a (param ap))
           (setf (params b) (list (param ap))))
          (t (-> (.const a :host-patch b)
                 (param ap))))))

(defun .ap1 (&rest rest) ;;; WWWWWWWWWWWWWWWWW
  (declare (dynamic-extent rest))
  (if (keywordp (car rest))
    (apply #'make-instance '.ap1 :a nil rest)
    (apply #'make-instance '.ap1 :a (car rest) (cdr rest))))
|#


;;; BIQUAD FILTERS

;;; Biquad with separate coeffs a's & b's

(defclass .biquad% (siso-block)
  ((st1 :initform nil :accessor st1)
   (st2 :initform nil :accessor st2)))

(defmethod initialize-instance :after ((b .biquad%) &key)
  (setf (params b)
        (list (make-instance 'param :host-block b :name 'a1)
              (make-instance 'param :host-block b :name 'a2)
              (make-instance 'param :host-block b :name 'b0)
              (make-instance 'param :host-block b :name 'b1)
              (make-instance 'param :host-block b :name 'b2))))

(defmethod finalize-sizes :after ((b .biquad%))
  (unless (= (length (datasize (prev-out (in b)))) 1)
    (error "Invalid datasize in ~a" b))
  (let* ((type (datatype (out b)))
         (st1 (make-instance 'blockvar
                :size (datasize (out b)) :name 'st1
                :type type :value nil
                :host-block b))
         (st2 (make-instance 'blockvar
                :size (datasize (out b)) :name 'st2
                :type type :value nil
                :host-block b)))
    (setf (st1 b) st1 (st2 b) st2)
    (setf (variables b) (list st1 st2)))
  nil)

(defmethod c-code ((b .biquad%) &optional (stream t))
  (let* ((a1 (cref (param b 'a1)))
         (a2 (cref (param b 'a2)))
      ;   (b0 (cref (param b 'b0)))
         (b1 (cref (param b 'b1)))
         (b2 (cref (param b 'b2)))
         (st1 (cref (st1 b)))
         (st2 (cref (st2 b)))
         (in (cref (in b)))
         (out (cref (out b)))
         (siz (datasize (out b))))
    (if (equal siz '(1))
      (with-c stream
        "RD_1 = |st1|; RD_2 = |st2|;"
        "RD_0 = |in| - RD_1*|a1| - RD_2*|a2|;"
        "|st2| = RD_1; |st1| = RD_0;"
        "|out| = RD_0*|b1| + RD_1*|b2|;")
      (with-c stream
        "for (RL_0=0; RL_0<|siz|; RL_0++){"
        "RD_1 = |st1|[RL_0]; RD_2 = |st2|[RL_0];"
        "RD_0 = |in|[RL_0] - RD_1*|a1|[RL_0] - RD_2*|a2|[RL_0];"
        "|st2|[RL_0] = RD_1; |st1|[RL_0] = RD_0;"
        "|out|[RL_0] = RD_0*|b1|[RL_0] + RD_1*|b2|[RL_0];}"))
    nil))

(defmethod lcode ((b .biquad%))
  (let* ((a1 (lvar (prev-out (param b 'a1))))
         (a2 (lvar (prev-out (param b 'a2))))
         (b0 (lvar (prev-out (param b 'b0))))
         (b1 (lvar (prev-out (param b 'b1))))
         (b2 (lvar (prev-out (param b 'b2))))
         (st1 (lvar (st1 b)))
         (st2 (lvar (st2 b)))
         (in (lvar (prev-out (in b))))
         (out (lvar (out b)))
         (siz (first (datasize (out b)))))
    `(loop for i from 0 below ,siz
           do (setf (aref ,out i)
                    (+ (* (aref ,b0 i)
                          (- (aref ,in i)
                             (* (aref ,st1 i) (aref ,a1 i))
                             (* (aref ,st2 i) (aref ,a2 i))))
                       (* (aref ,st1 i) (aref ,b1 i))
                       (* (aref ,st2 i) (aref ,b2 i)))))))

(defmethod mcode ((b .biquad%) &optional (stream t))
  (let* ((onam (mname (out b)))
         (innam (mname (in b)))
         (a1 (mname (param b 'a1)))
         (a2 (mname (param b 'a2)))
     ;    (b0 (mname (param b 'b0)))
         (b1 (mname (param b 'b1)))
         (b2 (mname (param b 'b2)))
         (x1 (mname (st1 b)))
         (x2 (mname (st2 b)))
         (temp "Temp"))
    (format stream "~%~a = ~a-~a.*~a-~a.*~a;"
            temp innam a2 x2 a1 x1)
    (format stream "~%~a = ~a; ~a = ~a;" 
            x2 x1 x1 temp)
    (format stream "~%~a = ~a.*~a+~a.*~a;"
            onam b1 x1 b2 x2)
    nil))


(defclass .biquad-macro (macro-block) ())

(defmethod initialize-macro-block ((b .biquad-macro) &key)
  (let* ((d1 (.d)) (d2 (.d))
         (add (.add :inputs 3))
         (sub (.sub :inputs 3))
         (ma1 (.mul)) (ma2 (.mul))
         (mb0 (.mul)) (mb1 (.mul))
         (mb2 (.mul))
         (pa1 (.p. :name 'a1)) (pa2 (.p. :name 'a2))
         (pb0 (.p. :name 'b0)) (pb1 (.p. :name 'b1))
         (pb2 (.p. :name 'b2)))
    (setf (inputs b) (list (in sub))
          (outputs b) (list (out add))
          (params b) (mapcar #'param (list pa1 pa2 pb0 pb1 pb2)))
    (-> d1 ma1 (in sub 1) d1 d2)
    (-> d2 ma2 (in sub 2) mb0 add)
    (-> d1 mb1 (in add 1))
    (-> d2 mb2 (in add 2))
    (-> pa1 (in ma1 1))
    (-> pa2 (in ma2 1))
    (-> pb0 (in mb0 1))
    (-> pb1 (in mb1 1))
    (-> pb2 (in mb2 1))))

;;; (inspect (make-instance '.biquad-macro))


(defclass .biquad (macro-block) ())

(defmethod initialize-macro-block ((b .biquad) &key
                                       a1 a2 b0 b1 b2 macro-p)
  (let* ((bq (if macro-p
               (make-instance '.biquad-macro)
               (make-instance '.biquad%))))
    (setf (outputs b) (list (out bq 0))
          (inputs b) (list (in bq 0)))
    (add-param b b2 (param bq 'b2) :name 'b2)
    (add-param b b1 (param bq 'b1) :name 'b1)
    (add-param b b0 (param bq 'b0) :name 'b0)
    (add-param b a2 (param bq 'a2) :name 'a2)
    (add-param b a1 (param bq 'a1) :name 'a1)))

(defun .biquad (&rest rest)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (apply #'make-instance '.biquad :arg (car rest) (cdr rest))
    (apply #'make-instance '.biquad rest)))

#|
(defparameter bq (.biquad))
(defparameter bq (.biquad :macro-p t))
(defparameter bq (make-instance '.biquad :a1 0.5))
(defparameter bq (make-instance '.biquad :a1 0.5 :macro-p t))
(inspect bq)

(defpatch p ((bq (.biquad :macro-p nil))
             (out (.probe "out")))
  (-> (.var 0.5) (param bq 'a1))
  (-> (.var 0.25) (param bq 'a2))
  (-> (.var 1.2) (param bq 'b0))
  (-> (.var 1.0) (param bq 'b1))
  (-> (.var 0.7) (param bq 'b2))
  (-> (.imp1) bq out)
  (defun pr () (print (value (out out)))))

(defpatch p ((bq (.biquad :macro-p t))
             (out (.probe "out")))
  (-> (.var 0.5) (param bq 'a1))
  (-> (.var 0.25) (param bq 'a2))
  (-> (.var 1.2) (param bq 'b0))
  (-> (.var 1.0) (param bq 'b1))
  (-> (.var 0.7) (param bq 'b2))
  (-> (.imp1) bq out)
  (defun pr () (print (value (out out)))))

(inspect p)

(c-code p)
(load-patch p)
(step-patch-n p 100000000 t)
(loop initially (pr)
      for i from 0 below 10
      do (step-patch p)
      do (pr))

(to-matlab p)
|#

#|
(defclass .bq-port (one-port-macro) ()
  (:default-initargs
    :domain 'electric))

(defmethod initialize-macro-block ((b .bq-port) &key)
  (let* ((pb1 (make-instance '.wa-port-block))
         (p1 (port pb1))
         (bq (.biquad :macro-p t)))
    (setf (domain p1) (domain b)
          (ports b) (list p1))
    (-> (out pb1) (in bq))
    (-> (out bq) (in pb1))
    (setf (params b) (params bq))))


    (-> (out pb1) (in pb0))
    (-> (host-block (zref p0))
        (param (host-block (zref p1)) 0))
    (-> (host-block (yref p0))
        (param (host-block (yref p1)) 0))))


(inspect (make-instance '.bq-port))



|#



;;; .bq23% biquad with FIR-part taps 2&3 only, delay-mixin
;;; 1- or 2-dim params separate

(defclass .bq23% (delay-mixin)
  ((st1 :initform nil :accessor st1)
   (st2 :initform nil :accessor st2)))

(defmethod initialize-instance :after ((b .bq23%) &key)
  (setf (params b)
        (list (make-instance 'param :host-block b :name 'a1)
              (make-instance 'param :host-block b :name 'a2)
              (make-instance 'param :host-block b :name 'b1)
              (make-instance 'param :host-block b :name 'b2))))

(defmethod finalize-sizes :after ((b .bq23%))
  (unless (= (length (datasize (prev-out (in b)))) 1)
    (error "Invalid datasize in ~a" b))
  (let* ((type (datatype (out b)))
         (st1 (make-instance 'blockvar
                :size (datasize (out b)) :name 'st1
                :type type :value nil
                :host-block b))
         (st2 (make-instance 'blockvar
                :size (datasize (out b)) :name 'st2
                :type type :value nil
                :host-block b)))
    (setf (st1 b) st1 (st2 b) st2)
    (setf (variables b) (list st1 st2)))
  nil)

(defmethod c-code ((b .bq23%) &optional (stream t))
  (let* ((a1 (cref (param b 'a1)))
         (a2 (cref (param b 'a2)))
         (b1 (cref (param b 'b1)))
         (b2 (cref (param b 'b2)))
         (st1 (cref (st1 b)))
         (st2 (cref (st2 b)))
         (in (cref (in b)))
         (out (cref (out b)))
         (siz (datasize (out b))))
    (if (equal siz '(1))
      (with-c stream
        "RD_1 = |st1|; RD_2 = |st2|;"
        "RD_0 = |in| - RD_1*|a1| - RD_2*|a2|;"
        "|st2| = RD_1; |st1| = RD_0;"
        "|out| = RD_0*|b1| + RD_1*|b2|;")
      (with-c stream
        "for (RL_0=0; RL_0<|siz|; RL_0++){"
        "RD_1 = |st1|[RL_0]; RD_2 = |st2|[RL_0];"
        "RD_0 = |in|[RL_0] - RD_1*|a1|[RL_0] - RD_2*|a2|[RL_0];"
        "|st2|[RL_0] = RD_1; |st1|[RL_0] = RD_0;"
        "|out|[RL_0] = RD_0*|b1|[RL_0] + RD_1*|b2|[RL_0];}"))
    nil))

(defmethod lcode ((b .bq23%))
  (let* ((b1 (lvar (prev-out (param b 'b1))))
         (b2 (lvar (prev-out (param b 'b2))))
         (st1 (lvar (st1 b)))
         (st2 (lvar (st2 b)))
         (out (lvar (out b)))
         (siz (first (datasize (out b)))))
    `(loop for i from 0 below ,siz
           do (setf (aref ,out i)
                    (+ (* (aref ,st1 i) (aref ,b1 i))
                       (* (aref ,st2 i) (aref ,b2 i)))))))

(defmethod first-step-p ((b .bq23%)) t)

(defmethod mcode ((b .bq23%) &optional (stream t))
  (let* ((onam (mname (out b)))
         (innam (mname (in b)))
         (a1 (mname (param b 'a1)))
         (a2 (mname (param b 'a2)))
         (b1 (mname (param b 'b1)))
         (b2 (mname (param b 'b2)))
         (x1 (mname (st1 b)))
         (x2 (mname (st2 b)))
         (temp "Temp"))
    (format stream "~%~a = ~a-~a.*~a-~a.*~a;"
            temp innam a2 x2 a1 x1)
    (format stream "~%~a = ~a; ~a = ~a;" 
            x2 x1 x1 temp)
    (format stream "~%~a = ~a.*~a+~a.*~a;"
            onam b1 x1 b2 x2)
    nil))

#|
(defparameter b (make-instance '.bq23%))
(inspect b)

(defpatch p ((bq (make-instance '.bq23%))
             (out (.probe "out")))
  (-> (.var '(0.5 0.5)) (param bq 'a1))
  (-> (.var '(0.25 0.25)) (param bq 'a2))
  (-> (.var '(1.0 1.0)) (param bq 'b1))
  (-> (.var '(0.7 0.7)) (param bq 'b2))
  (-> (.imp1 '(1.0 0.5)) bq out)
  (defun pr () (print (value (out out)))))

(defpatch p ((bq (make-instance '.bq23%))
             (out (.probe "out")))
  (-> (.var 0.5) (param bq 'a1))
  (-> (.var 0.25) (param bq 'a2))
  (-> (.var 1.0) (param bq 'b1))
  (-> (.var 0.7) (param bq 'b2))
  (-> (.imp1) bq out)
  (defun pr () (print (value (out out)))))

(inspect p)

(c-code p)
(load-patch p)
(step-patch-n p 2000000 t)
(loop initially (pr)
      for i from 0 below 10
      do (step-patch p)
      do (pr))

(to-matlab p)

(defpatch p2 ()
  (-> (.var 1.0) (.probe "out")))

(compile-patch p2)
(step-patch-n p2 100000000 t)


(defpatch p ((d1 (.d)) (d2 (.d))
             (sub (.sub :inputs 3))
             (add (.add))
             (out (.probe "out")))
  (-> (.imp1) sub d1 d2)
  (-> d1 (.coeff 0.5) (in sub 1))
  (-> d2 (.coeff 0.25) (in sub 2))
  (-> d1 (.coeff 1.0) add out)
  (-> d2 (.coeff 0.7) (in add 1))
  (defun pr () (print (at (out out)))))

(defpatch p ((out (.probe "out")))
  (-> (.imp1) out)
  (defun pr () (print (at (out out)))))
|#

;;; .bq23+ biquad with FIR-part taps 2&3 only, delay-mixin
;;; params in vector

(defclass .bq23+ (delay-mixin)
  ((st1 :initform nil :accessor st1)
   (st2 :initform nil :accessor st2)))

(defmethod initialize-instance :after ((b .bq23+) &key)
  (setf (params b)
        (list (make-instance 'param :host-block b :name 'coeff))))

(defmethod finalize-sizes :after ((b .bq23+))
  (unless (= (length (datasize (prev-out (in b)))) 1) ;;; ???
    (error "Invalid datasize in ~a" b))
  (let* ((type (datatype (out b)))
         (st1 (make-instance 'blockvar
                :size (datasize (out b)) :name 'st1
                :type type :value nil
                :host-block b))
         (st2 (make-instance 'blockvar
                :size (datasize (out b)) :name 'st2
                :type type :value nil
                :host-block b)))
    (setf (st1 b) st1 (st2 b) st2)
    (setf (variables b) (list st1 st2)))
  nil)

(defmethod c-code ((b .bq23+) &optional (stream t))
  (let* ((cs (cref (param b 'coeffs)))
         (st1 (cref (st1 b)))
         (st2 (cref (st2 b)))
         (in (cref (in b)))
         (out (cref (out b)))
         (siz (datasize (out b))))
    (if (equal siz '(1))
      (with-c stream
        "RD_1 = |st1|; RD_2 = |st2|;"
        "|out| = RD_1*|cs|[2] + RD_2*|cs|[3];"
        "RD_0 = |in| - RD_1*|cs|[0] - RD_2*|cs|[1];"
        "|st2| = RD_1; |st1| = RD_0;")
      (with-c stream
        "for (RL_0=0; RL_0<|siz|; RL_0++){"
        "RD_1 = |st1|[RL_0]; RD_2 = |st2|[RL_0];"
        "|out|[RL_0] = RD_1*|cs|[2][RL_0] + RD_2*|cs|[3][RL_0];"
        "RD_0 = |in|[RL_0] - RD_1*|cs|[0][RL_0] - RD_2*|cs|[1][RL_0];"
        "|st2|[RL_0] = RD_1; |st1|[RL_0] = RD_0;}"))
    nil))

(defmethod lcode ((b .bq23+)) ;;; ???
  nil)

(defmethod mcode ((b .bq23+) &optional (stream t))
  (let* ((onam (varname (out b)))
         (innam (varname (prev-out (in b))))
         (cs (varname (param b 'cs)))
         (x1 (varname (st1 b)))
         (x2 (varname (st2 b))))
    (format stream "~%~a = ~a(:,3).*~a+~a(:,4).*~a;"
            onam cs x1 cs x2)
    (format stream "~%~a = ~a;" x2 x1)
    (format stream "~%~a = ~a-~a(:,1).*~a-~a(:,2).*~a;"
            x1 innam cs x2 cs x1)
    nil))



;;; INTEGRATORS

;;; Impulse invariant integrator

(defclass .integ (macro-block) ())

(defmethod initialize-macro-block ((b .integ) &key
                                       arg coeff value
                                       (delayed nil))
  (let* ((kdt (.coeff (/ 1.0d0 (srate b))))
         (d (.d :value value)) (x (.x.))
         (add1 (.add)) out)
    (if coeff (-> d (.coeff coeff) x) (-> d x))
    (-> x (in add1 1))
    (-> kdt add1 d)
    (cond (arg (-> (.val arg) (in kdt)))
          (t (setf (inputs b) (list (in kdt)))))
    (cond (delayed (setq out (out d)))
          (t (setq out (out add1))))
    (setf (outputs b) (list out))))

(defun .integ (&rest rest)
  (declare (dynamic-extent rest))
  (if (not (keywordp (car rest)))
    (apply #'make-instance '.integ :arg (car rest) (cdr rest))
    (apply #'make-instance '.integ rest)))


;;; Bilinear integrator

(defclass .integb (macro-block) ())

(defmethod initialize-macro-block ((b .integb) &key
                                       arg coeff)
  (let* ((kdt (.coeff (/ 0.5 (srate b))))
         (d (.d)) (x (.x.))
         (add1 (.add))
         (add2 (.add)))
    (-> add1 d) (-> add1 add2)
    (if coeff (-> d (.coeff coeff) x) (-> d x))
    (-> x (in add1 1)) (-> x (in add2 1))
    (-> kdt add1)
    (cond (arg (-> (.val arg) (in kdt)))
          (t (setf (inputs b) (list (in kdt)))))
    (setf (outputs b) (list (out add2)))))

(defun .integb (&rest rest)
  (declare (dynamic-extent rest))
  (if (not (keywordp (car rest)))
    (apply #'make-instance '.integb :arg (car rest) (cdr rest))
    (apply #'make-instance '.integb rest)))


;;; RAMP

(defclass .ramp (macro-block) ())

(defmethod initialize-macro-block ((b .ramp) &key
                                       control value
                                       (delayed t))
  (let* ((kdt (.coeff (/ 1.0d0 (srate b))))
         (d (.d :value value)) (x (.x.))
         (add1 (.add)) out)
    (-> d x)
    (-> x (in add1 1))
    (-> kdt add1 d)
    (when control (setq control (.val control))
          (-> control (in kdt)))
    (cond (delayed (setq out (out d)))
          (t (setq out (out add1))))
    (setf (inputs b) (list (in kdt))
          (outputs b) (list out))))

(defun .ramp (&rest rest)
  (declare (dynamic-extent rest))
  (if (not (keywordp (car rest)))
    (apply #'make-instance '.ramp :control (car rest) (cdr rest))
    (apply #'make-instance '.ramp rest)))


;;; DIFFERENTIATORS

;;; Impulse invariant differentiator

(defclass .diff (macro-block) ())

(defmethod initialize-macro-block ((b .diff) &key
                                       arg coeff)
  (let* ((kdt (.coeff (* 1.0d0 (srate b))))
         (d (.d)) (x (.x.))
         (sub1 (.sub)))
    (if coeff (-> d (.coeff coeff) x) (-> d x))
    (-> x (in sub1 1))
    (-> kdt d) (-> kdt sub1)
    (cond (arg (-> (.val arg) (in kdt)))
          (t (setf (inputs b) (list (in kdt)))))
    (setf (outputs b) (list (out sub1)))))

(defun .diff (&rest rest)
  (declare (dynamic-extent rest))
  (if (not (keywordp (car rest)))
    (apply #'make-instance '.diff :arg (car rest) (cdr rest))
    (apply #'make-instance '.diff rest)))


;;; Bilinear differentiator (pole at Nyquist !!!)

(defclass .diffb (macro-block) ())

(defmethod initialize-macro-block ((b .diffb) &key
                                       arg coeff)
  (let* ((kdt (.coeff (cl:float (* 2.0 (srate b)) 1.0d0)))
         (d (.d)) (x (.x.))
         (sub1 (.sub))
         (sub2 (.sub)))
    (-> sub1 d) (-> sub1 sub2)
    (if coeff (-> d (.coeff coeff) x) (-> d x))
    (-> x (in sub1 1)) (-> x (in sub2 1))
    (-> kdt sub1)
    (cond (arg (-> (.val arg) (in kdt)))
          (t (setf (inputs b) (list (in kdt)))))
    (setf (outputs b) (list (out sub2)))))

(defun .diffb (&rest rest)
  (declare (dynamic-extent rest))
  (if (not (keywordp (car rest)))
    (apply #'make-instance '.diffb :arg (car rest) (cdr rest))
    (apply #'make-instance '.diffb rest)))


(provide :BC-filters)
