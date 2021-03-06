
(in-package :BC)

;;; OSCILLATORS AND SOURCES

;;; Impulse (non-triggerable)

(defclass .imp1 (.data) ())

(defun .imp1 (&rest rest &aux value ltype)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (setq value (car rest) rest (cdr rest))
    (setq value '(1.0d0)))
  (setq ltype (get *default-type* 'ltype))
  (multiple-value-bind (val siz b) (map.type value ltype)
    (setq b (apply #'make-instance '.imp1 :value val :var-p t rest))
    (setf (out-size b) siz (sized b) siz (typed b) *default-type*)
    (setf (var-p b) t (var-p (out b)) t)
    (setf (datatype (out b)) *default-type* (datasize (out b)) siz)
    b))

(def.c .imp1 "OUT_0 = 0.0;")

(defmethod mcode ((b .imp1) &optional (stream t))
  (format stream "~%~a(:) = 0;"
          (varname (out b)) (varname (out b))))


;;; Ramp sawtooth generator

(defclass .ramp-sawtooth (dynamic-block siso-block)
  ((limit :initarg :limit :accessor limit)
   (check-limit- :initarg :check-limit- :accessor check-limit-)
   (kd :initarg :kd :accessor kd))
  (:default-initargs
    :limit 1.0d0
    :kd nil
    :check-limit- t))

(defmethod initialize-instance :after ((b .ramp-sawtooth) &key)
  (let* ((kd (kd b)) (sr (srate b)) (limit (limit b)))
    (unless kd (setq kd (/ limit (float sr 1.0d0))))
    (unless (numberp kd) (error "Invalid kd for ~a" b))
    (setf (kd b) kd)))

(def.c .ramp-sawtooth
  (format nil "RD_0 = ~a*IN_0 + OUT_0;" (bcf (kd *b*)))
  (format nil "~%if (RD_0>~a) {RD_0 -= ~a;}"
          (bcf (limit *b*)) (bcf (limit *b*)))
  (when (check-limit- *b*)
    (format nil "~%if (RD_0<-~a) {RD_0 += ~a;}"
            (bcf (limit *b*)) (bcf (limit *b*))))
  (format nil "~%OUT_0 = RD_0;"))

(defmethod mcode ((b .ramp-sawtooth) &optional (stream t))
  (let* ((out (varname (out b)))
         (pout (varname (prev-out (in b))))
         (limit (limit b)) (kd (kd b)))
    (format stream "~%~a = ~a+~a.*~a;" out out kd pout)
    (format stream "~%~a=~a-(~a>~a).*~a;"
            out out out limit limit)
    (when (check-limit- b)
      (format stream "~%~a=~a+(~a<~a).*~a;"
              out out out 0 limit))))

(defun .ramp-sawtooth (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.ramp-sawtooth rest))


;;; Sawtooth oscillator
;;; :freq & :ampl params
;;; for :ampl, only value t makes free param input

(defclass .sawtooth-osc (macro-block) ())

(defmethod initialize-macro-block ((b .sawtooth-osc) &key
                                       freq ampl)
  (let* ((pf (make-instance '.p.))
         (ro (.ramp-sawtooth))
         pa mul)
    (-> pf ro)
    (when ampl
      (setq pa (make-instance '.p.))
      (add-param b ampl (param pa) :name 'ampl)
      (setq mul (.mul ro pa)))
    (add-param b freq (param pf) :name 'freq)
    (if ampl (push (out mul) (outputs b))
        (push (out ro) (outputs b)))
    b))

(defun .sawtooth-osc (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.sawtooth-osc rest))


;;; Sinewave oscillator
;;; :freq & :ampl params
;;; for :ampl, only value t makes free param input

(defclass .sin-osc (macro-block) ())
           
(defmethod initialize-macro-block ((b .sin-osc) &key
                                       freq ampl)
  (let* ((ro (.ramp-sawtooth :limit (* 2.0d0 pi)))
         (sin (.sin ro))
         (pf (make-instance '.p.)) pa mul)
    (-> pf ro)
    (when ampl
      (setq pa (make-instance '.p.))
      (add-param b ampl (param pa) :name 'ampl)
      (setq mul (.mul sin pa)))
    (add-param b freq (param pf) :name 'freq)
    (if ampl (push (out mul) (outputs b))
        (push (out sin) (outputs b)))
    b))

(defun .sin-osc (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.sin-osc rest))


;;; WAVETABLES

;;; .RTABLE (triggerable rreadtable = wavetable)

(defclass .rtable% (siso-block)
  ((dptr :initform nil :accessor dptr)
   (dsize :initform nil :accessor dsize)))

(defmethod initialize-instance :after ((b .rtable%) &key)
  (let* ((prm (make-instance 'param
                :name 'trig :flag t
                :host-block b))
         (ptr (make-instance 'indexvar
                :size '(1) :host-block b))
         (dsz (make-instance 'indexvar
                :size '(1) :host-block b)))
    (setf (params b) (list prm)
          (dptr b) ptr (dsize b) dsz)
    (pushnew ptr (variables b))
    (pushnew dsz (variables b)))
  nil)

(defmethod include-size-params ((b .rtable%)) nil)
(defmethod include-size-inputs ((b .rtable%)) nil)

(defmethod check-sizes ((b .rtable%)) nil)

(defmethod get-size ((b .rtable%))
  (let* ((is (get-size (in b))) s s2)
    (cond ((= (length is) 1) (setq s 1 s2 (first is)))
          (t (setq s (first is) s2 (second is))))
    (setf (lvar (dptr b)) (make-array 1 :initial-element s2)
          (lvar (dsize b)) (make-array 1 :initial-element s2))
    (list s)))

(defmethod c-code ((b .rtable%) &optional (stream t))
  (let* ((in (cref (in b)))
         (dtype  (datatype (prev-out (in b))))
         (z (get dtype 'proto))
         (out (cref (out b)))
         (osiz (datasize (out b)))
         (os1 (first osiz))
         (flag (cref (flag (param b))))
         (ptr (cref (dptr b)))
         (siz (cref (dsize b))))
    (with-c stream
      "if (|flag| != 0) {|ptr| = 0; |flag| = 0;}")
    (cond ((equal osiz '(1))
           (with-c stream
             "if (|ptr|>=|siz|) {|out| = |z|;}"
             "else {|out| = |in|[|ptr|++];}"))
          ((= (length osiz) 1)
           (with-c stream
             "if (|ptr|>=|siz|)"
             "{for (LI=0; LI<|os1|;) {|out|[LI++] = |z|;}}"
             "else {for (LI=0; LI<|os1|; LI++) "
             "{|out|[LI] = |in|[LI][|ptr|];}"
             "|ptr|++;}")))))


(defclass .rtable (macro-block) ())

(defmethod initialize-macro-block ((b .rtable) 
                                       &rest rest &key data)
  (unless data (error "Data not given to ~a" b))
  (let* ((d (.val data))
         (w (apply #'make-instance '.rtable%
                   :allow-other-keys t rest)))
    (when (equal (datasize (out d)) '(1))
      (error "Table data must be at least two samples"))
    (-> d (in w))
    (setf (params b) (list (param w 'trig))
          (outputs b) (list (out w))))
  b)

(defun .rtable (data &rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.rtable
         :data data rest))

#|
(defpatch wp ((w (make-instance '.rtable%)))
  (-> (.var 1.0) (param w 'trig))
  (-> (.var '(1.0 2.0 1.0)) (in w))
  (-> w (.probe "out")))

(defpatch wp ((w (make-instance '.rtable
                   :data '((1.0 2.0 3.0)
                           (4.0 5.0 6.0)))))
  (-> (.var 1.0) (param w 'trig))
  (-> w (.probe "out")))

(defpatch wp ((w (.rtable (.short '(1.0 2.0 1.0)))))
  (-> (.var 1.0) (param w 'trig))
  (-> w (.probe "out")))

(defpatch wp ((w (.rtable (.const '(1.0 2.0 1.0)))))
  (-> (.var 1.0) (param w 'trig))
  (-> w (.probe "out")))

(defpatch wp ((w (.rtable '(1.0 2.0 1.0) :out-type '.long)))
  (-> (.var 1.0) (param w 'trig))
  (-> w (.probe "out")))

(defpatch wp ((w (.rtable '(1.0 2.0 1.0))))
  (-> (.var 1.0) (param w 'trig))
  (-> w (.probe "out")))

(inspect wp)
(c-code wp)
(compile-patch wp)
(step-patch wp)
(step-patch-n wp 100000000 t)

(let* ((sig (qs::read-aiff :file "ccl:kaksi44.aiff"))
       (data (qs::get-list sig)))
  sig)

(defpatch k44 ((sig (qs::read-aiff
                     :file "ccl:kaksi44.aiff"))
               (data (qs::get-list sig))
               (w (.rtable (.short data))))
  (-> (.trig) (param w 'trig) (inputs (.da*))))

(inspect k44)
(c-code k44)
(compile-patch k44)
(run-patch k44)
(stop-patch k44)
(trig (elt (block-items k44) 1))

(inspect (.trig-data '(1)))
(inspect (.trig-imp1))

(defpatch trg ((tr (.trig-data '(1.0 0.0))))
  (-> tr (inputs (.da)))
  (defun pluckfun () (trig tr)))

(load-patch trg)
(run-patch trg)
(trig (elt (block-items trg) 1))
(stop-patch trg)
(pluckfun)


(defpatch trg ((tr (.trig-data '(1.0 0.0))))
  (-> tr (inputs (.da)))
  (def-pfun pluck (trg) (trig tr)))

(defpatch trg ((tr (.trig-imp1)))
  (-> tr (inputs (.da)))
  (def-pfun pluck (trg) (trig tr)))

(inspect trg)

(pluck trg)
(time (dotimes (i 100000) (pluck trg)))
|#

;;; Triggerable .RTABLE

(defclass .trig-data (macro-block) ())

(defmethod initialize-macro-block ((b .trig-data) &key
                                       data)
  (let* ((trig (.trig))
         (tab (.rtable data)))
    (-> trig (param tab 'trig))
    (push (out tab) (outputs b))))

(defmethod trig ((b .trig-data))
  (trig (find '.trig (block-items b)
              :key #'type-of)))

(defun .trig-data (data &rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.trig-data
         :data data rest))


;;; Unit impulse, triggerable

(defun .trig-imp1 (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'.trig-data '(1.0d0 0.0d0) rest))



;;; DATA ROUTING BLOCKS

;;; .GATE (signal gating)

(defclass .gate% (basic-block) ())

(defmethod initialize-instance :after ((b .gate%) &key
                                           inputs)
  (cond ((integerp inputs)
         (setf (inputs b)
               (loop for i from 0 below inputs
                     collect (make-instance 'input :host-block b))))
        ((consp inputs)
         (setf (inputs b)
               (loop for inx in inputs
                     for in = (make-instance 'input :host-block b)
                     for val = (.val inx)
                     do (-> val in)
                     collect in)))
        (t (error "Input spec error for ~a" b)))
  (setf (outputs b)
        (loop for i from 0 below (length (inputs b))
              collect (make-instance 'output :host-block b)))
  (setf (params b)
        (list (make-instance 'param
                :name 'gate :host-block b))))

(defmethod include-size-params ((b .gate%)) nil)

(defmethod finalize-types :after ((b .gate%))
  (loop for in in (inputs b)
        for out in (outputs b)
        do (unless (datatype out)
             (setf (datatype out)
                   (datatype (prev-out in))))))

(defmethod finalize-sizes :after ((b .gate%))
  (unless (equal (datasize (prev-out (param b))) '(1))
    (error "Param dimension error in ~a" b))
  (loop for in in (inputs b)
        for out in (outputs b)
        do (setf (datasize out)
                 (datasize (prev-out in)))))

(defmethod c-code ((b .gate%) &optional (stream t))
  (let* ((p (cref (param b))) d1 d2)
    (with-c stream
      "if (|p|>0){")
    (loop for input in (inputs b)
          for in = (cref input)
          for isiz = (datasize (prev-out input))
          for output in (outputs b)
          for out = (cref output)
          for osiz = (datasize output)
          do (unless (equal isiz osiz)
               (error "Datasize error in ~a" b))
          do (cond ((equal isiz '(1))
                    (with-c stream
                      "|out| = |in|;"))
                   ((= (length isiz) 1)
                    (setq d1 (first isiz))
                    (with-c stream
                      "for (RL_0=0; RL_0<|d1|; RL_0++)"
                      "{|out|[RL_0] = |in|[RL_0];}"))
                   ((= (length isiz) 2)
                    (setq d1 (first isiz) d2 (second isiz))
                    (with-c stream
                      "for (RL_0=0; RL_0<|d1|; RL_0++){"
                      "for (RL_1=0; RL_1<|d2|; RL_1++)"
                      "{|out|[RL_0][RL_2] = |in|[RL_0][RL_2];}}"))
                   (t (error "Dimensionality errir in ~a" b))))
    (format stream "}")))


(defclass .gate (macro-block) ())

(defmethod initialize-macro-block ((b .gate) &key 
                                       inputs gate)
  (let* ((g (make-instance '.gate%
              :host-patch b :inputs inputs)))
    (add-param b gate (param g 'gate) :name 'gate)
    (setf (inputs b) (inputs g)
          (outputs b) (outputs g))))

(defun .gate (inputs &rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.gate
         :inputs inputs rest))

  
#|
(inspect (make-instance '.gate% :inputs 3))
(inspect (.gate 3))

(defpatch gp ((g (.gate 2))
              (v (.var 0 :out-type '.long)))
  (-> v (param g))
  (-> (.var 1.0) (in g 0))
  (-> (.var 2.0) (in g 1))
  (-> (out g 0) (.probe "out0"))
  (-> (out g 1) (.probe "out1")))

(defpatch gp ((g (.gate 2))
              (v (.var 0 :out-type '.long)))
  (-> v (param g))
  (-> (.var'(1.0 1.5)) (in g 0))
  (-> (.var 2.0) (in g 1))
  (-> (out g 0) (.probe "out0"))
  (-> (out g 1) (.probe "out1")))

(defpatch gp ((g (.gate (list (.var 1.0) (.var 2.0))
                        :out-type '.long))
              (v (.var 0 :out-type '.long)))
  (-> v (param g))
  (-> (out g 0) (.probe "out0"))
  (-> (out g 1) (.probe "out1")))

(inspect gp)
(c-code gp)
(load-patch gp)
|#
  

(provide :BC-DSP)
