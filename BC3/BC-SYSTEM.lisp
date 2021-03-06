
(in-package :BC)


;;; C-DODE GENERATION

(defun bcf (x)
  (cond ((typep x 'float)
         (let ((str (format nil "~a" x)) pos)
           (setq pos (position #\D str))
           (unless pos (setq pos (position #\d str)))
           (unless pos (setq pos (position #\S str)))
           (unless pos (setq pos (position #\s str)))
           (unless pos (setq pos (position #\F str)))
           (unless pos (setq pos (position #\f str)))
           (when pos (setq str (replace str "e" :start1 pos)))
           str))
        (t (format nil "~a" x))))

#|
(bcf 1.2f30)
(bcf 1.2d30)
(bcf 1.2s30)

|#

(defun cformat (stream str data &optional data2)
  (when (floatp data) (setq data (cl:float data 1.0d0)))
  (format stream str data data2))

(defun cprint.data (data stream)
  ;;; makes the C data form (from Lisp array)
  ;;; to be used in declaring C variables
  (let ((dim (array-dimensions data)))
    (cond ((and (equal dim '(1)) (characterp (elt data 0))) ;;; single char ???
           (format stream "'~a'" (bcf (elt data 0))))
          ; array of chars
          ((stringp data) ;;; single string
           (format stream "\"~a\"" (bcf data)))
          ; array of strings
          ((and (arrayp data) (equal dim '(1)))
           (cformat stream "~a" (bcf (elt data 0))))
          ((and (arrayp data) (= (length dim) 1)) ;;; vector
           (cformat stream "{~a" (bcf (elt data 0)))
           (loop for i from 1 below (first dim)
                 for val = (bcf (elt data i))
                 do (cformat stream ",~a" val)
                 finally (format stream "}")))
          ((and (arrayp data) (= (length dim) 2)) ;;; 2D-arrays
           (format stream "{")
           (loop for j from 0 below (first dim)
                 do (cformat stream "~a{~a"
                            (if (/= j 0) "," "")
                            (bcf (aref data j 0)))
                 do (loop for i from 1 below (second dim)
                          for val = (bcf (aref data j i))
                          do (cformat stream ",~a" val)
                          finally (format stream "}")))
           (format stream "}"))
          (t (error "Invalid data for C-printing ~a" data)))))

(defmethod cref ((port blockvar) &rest rest)
  ;;; makes a C data referncing form (name of variable)
  (declare (ignore rest) (dynamic-extent rest))
  (varname port))

(defmethod cref ((port input) &rest rest)
  (apply #'cref (prev-out port) rest))

(defmethod cref ((port param) &rest rest)
  (apply #'cref (prev-out port) rest))


(defmethod nref ((port blockvar) &optional ind1 ind2)
  (let* ((dim (datasize port))
         (nam (varname port)))
    (cond ((equal dim '(1))
           (format nil "~a" nam))
          ((= (length dim) 1)
           (format nil "~a[~a]" nam ind1))
          (t (format nil "~a[~a][~a]" nam ind1 ind2)))))

(defmethod nref ((port input) &optional ind1 ind2)
  (nref (prev-out port) ind1 ind2))

(defmethod nref ((port param) &optional ind1 ind2)
  (nref (prev-out port) ind1 ind2))


(defmethod ccref ((x blockvar) &rest rest)
  (let* ((siz (datasize x)))
    (cond ((equal siz '(1)) (varname x))
          ((= (length siz) 1)
           (let ((a1 (first rest)))
             (when (null a1) (setq a1 (elt siz 0)))
             (format nil "~a[~a]" (varname x) a1)))
          ((= (length siz) 2)
           (let ((a1 (first rest)) (a2 (second rest)))
             (when (null a1) (setq a1 (elt siz 0)))
             (when (null a2) (setq a2 (elt siz 1)))
             (format nil "~a[~a][~a]" (varname x) a1 a2)))
          (t (error "invalid args to ccref ~a" x)))))

(defmethod csref ((x blockvar) stream &rest rest)
  (let* ((siz (datasize x)))
    (cond ((equal siz '(1))
           (format stream "~a" (varname x)))
          ((= (length siz) 1)
           (let ((a1 (first rest)))
             (when (null a1) (setq a1 (elt siz 0)))
             (format stream "~a[~a]" (varname x) a1)))
          ((= (length siz) 2)
           (let ((a1 (first rest)) (a2 (second rest)))
             (when (null a1) (setq a1 (elt siz 0)))
             (when (null a2) (setq a2 (elt siz 1)))
             (format stream "~a[~a][~a]" (varname x) a1 a2)))
          (t (error "invalid args to ccref ~a" x)))))

(defmethod ccref ((port input) &rest rest)
  (apply #'ccref (prev-out port) rest))

(defmethod ccref ((port param) &rest rest)
  (apply #'ccref (prev-out port) rest))

(defmethod cdef ((port blockvar) &optional (stream t))
  (let* ((dt (datatype port))
         (ct (get dt 'ctype))
         (var (ccref port)))
    (format stream "~%~a ~a" ct var)
    (cdefval port stream)
    (format stream ";")))

(defmethod cdefval ((port blockvar) &optional (stream t))
  (format stream " = ")
  (cprint.data (lvar port) stream))

(defmethod cdefval ((port blockvar0) &optional (stream t))
  (format stream " = {0.0}"))

(defmethod cdefval ((port blockvar0+) &optional (stream t))
  (format stream " = ")
  (cprint.data (lvar port) stream))

#|
(defmethod cdefval ((port blockvar0+) &optional (stream t))
  (format stream " = {0.0}")
  (loop with arr = (lvar port)
        for i from 0 below (length arr)
        for x = (aref arr i)
        do (when (not (= x 0.0d0))
             (format stream "; ~a[~a] = ~a"
                     (varname port) i x)
             (return nil))))
|#


;;; PSEUDO-C

(eval-when (eval compile load)
(defun parse-pseudo-c (string &optional (escape #\|))
  ;;; parsing of pseudo C with escape characters ||
  ;;; to present references to the Lisp data
  (let* ((params nil) item (pend 0) outstr)
    (with-output-to-string (str)
      (terpri str)
      (loop for i from 0 below (length string)
            for char = (elt string i)
            do (cond ((char-equal char escape)
                      (setq pend (position escape string :start (1+ i)))
                      (unless pend (error "Erroneous pseudo-c string"))
                      (setq item (read-from-string 
                                  string nil nil :start (1+ i) :end pend))
                      (unless item (setq item "|"))
                      (push item params)
                      (princ "~a" str)
                      (setq i pend))
                     (t (princ char str))))
      (setq outstr (get-output-stream-string str)))
    (list* outstr (reverse params)))))

(defmacro with-c (stream &body body)
  ;;; macro within which pseudo C expressions are used as strings
  (let* ((s (gensym))
         (forms (mapcar
                 #'(lambda (x)
                     (list* 'format s
                            (parse-pseudo-c x))) body)))
    (if (null stream)
      `(with-output-to-string (,s)
         ,@forms ,stream ,s)
      `(let ((,s ,stream))
         ,@forms nil))))

(defmethod parse-cstrn ((b basic-block) string stream &aux n)
  ;;; parsing of pseudo C with excapes to variables
  (loop for i from 0 below (length string)
        do (cond ((eql (search "IN_" string :start2 i) i)
                  (setq n (- (char-int (elt string (+ i 3))) 48))
                  (csref (prev-out (in b n)) stream "LI" "LJ")
                  (incf i 3))
                 ((eql (search "OUT_" string :start2 i) i)
                  (setq n (- (char-int (elt string (+ i 4))) 48))
                  (csref (out b n) stream "LI" "LJ")
                  (incf i 4))
                 ((eql (search "PAR_" string :start2 i) i)
                  (setq n (- (char-int (elt string (+ i 4))) 48))
                  (csref (prev-out (param b n)) stream "LI" "LJ")
                  (incf i 4))
                 (t (format stream "~a" (elt string i))))))

(defmacro def.c (name &rest strings)
  `(defmethod c-code ((b ,name) &optional (stream t))
     (let* ((dim (datasize (out b))) inbeg
            (inend (length (inputs b))))
       (cond ((equal dim '(1)) nil)
             ((= (length dim) 1)
              (format stream "~%for (LI=0; LI<~a; LI++){" (first dim)))
             ((= (length dim) 2)
              (format stream "~%for (LI=0; LI<~a; LI++){" (first dim))
              (format stream "~%for (LJ=0; LJ<~a; LJ++){" (second dim)))
             (t (error "Invalid array dimensions")))
       (terpri stream)
       (loop for sf in ',strings
             for s = (eval `(let ((*b* ,b)) *b* ,sf))
             do (cond ((eql (search "_IN*" s) 0)
                       (setq inbeg (- (char-int (elt s 4)) 48))
                       (loop for i from inbeg below inend
                             for str = (subseq s 6)
                             for rpl = (format nil "IN_~a" i)
                             for pos = (search "IN**" str)
                             for strx = (replace str rpl :start1 pos)
                             do (parse-cstrn b strx stream)))
                      (t (parse-cstrn b s stream))))
       (cond ((equal dim '(1)) nil)
             ((= (length dim) 1) (format stream "~%}"))
             ((= (length dim) 2) (format stream "~%}}")))
       nil)))


;;; C-CODE

(defmacro def.cfun (name args &body body)
  `(let ((%m (defmethod ,name ,args ,@body))
         (class (second (first ',args))))
     (pushnew ',name (get class 'cdefs))
     %m))

(defmethod c-code* ((b basic-block)
                      &optional (stream t))
  (when (equal (mrate b) 1) (setf (mrate b) nil))
  (when (equal (mrate* b) 1) (setf (mrate* b) nil))
  (when (eq (var-p b) t)
    (let* ((mr (mrate b))
           (mr* (mrate* b)))
      (format stream "~%{")
      (cond ((and (not mr) (not mr*))
             (format stream "/* ~a */" (type-of b))
             (c-code b stream))
            ((and (rationalp mr) (= (numerator mr) 1))
             (let* ((denom (denominator mr))
                    (cnt (cref (mphase b))))
               (format stream "/* ~a, mrate = ~a */" (type-of b) mr)
               (format stream "~%LC = ~a;~%if (LC==0){" cnt)
               (c-code b stream)
               (format stream "~%~a = ~a;}" cnt (1- denom))
               (format stream "~%else {~a = LC-1;}" cnt)))
            ((and (rationalp mr*) (= (numerator mr*) 1))
             (let* ((denom (denominator mr*))
                    (cnt (cref (mphase b)))
                    (out (cref (out b))))
               (format stream "/* ~a, mrate* = ~a */" (type-of b) mr)
               (format stream "~%LC = ~a;~%if (LC==0){" cnt)
               (c-code b stream)
               (format stream "~%~a = ~a;}" cnt (1- denom))
               (format stream "~%else {~a = 0.0; ~a = LC-1;}" out cnt))) ;;; NOT READY !!!!!!!!!!!!!!!!
            ((and (integerp mr))
             (format stream "/* ~a, mrate = ~a */" (type-of b) mr)
             (format stream "~%for (LC = 0; LC<~a; LC++) {" mr)
             (c-code b stream)
             (format stream "~%}"))
            (t (error "Multirate specification error")))
      (format stream "~%}")))
  nil)

(defmethod c-code* ((b fwd-block) &optional (stream t))
  stream nil)

#|
(defpatch p ()
  (-> (.var 1.0) (.neg :mrate 1/2) (.probe "out")))
(c-code p)
|#

(defmethod c-code ((b basic-block) &optional (stream t))
  stream nil)

(defmethod register-code ((p patch) &optional (stream t))
  (format stream "~%  register long LI,LJ,LC,RL_0,RL_1,RL_2,RL_3,RL_4,RL_5;")
  (format stream "~%  register double RD_0,RD_1,RD_2,RD_3,RD_4,RD_5,RD_6,RD_7,RD_8;~%")
  nil)


(defmethod body-code% ((p patch) &optional (stream t))
  (register-code p stream)
  (loop for b in (lin-schedule p) do (c-code* b stream))
  (format stream "~%return(0);}")
  nil)


(defmethod body-code ((p patch) &optional (stream t))
  (loop for b in (lin-schedule p)
        for cdefs = (get (type-of b) 'cdefs)
        do (loop for cdef in (reverse cdefs)
                 do (terpri stream)
                 do (funcall cdef b stream))
        finally (terpri stream))
  (format stream "~%extern long patch() {")
  (body-code% p stream)
  (format stream "~%~%extern void npatch(long n)")
  (format stream "~% {register long LN;")
  (format stream "~%  for (LN=0; LN<n; LN++) {patch();}}")
  (format stream "~%~%extern long paddr() {return((long)&patch);}")
  (format stream "~%~%int main(){patch();}~%")
  nil)

(defmethod variable-code ((p patch) &optional (stream t)) 
  (let* ((vars (used-variables p)))
    (loop for v in vars do (cdef v stream))
    (terpri stream)
    (loop for v in vars
        do (variable-reader-code v stream))
    (loop for v in vars
        do (variable-writer-code v stream))))

(defmethod struct-code ((p patch) &optional (stream t))
  (loop with types = nil
        for b in (lin-schedule p)
        do (when (and (var-p b) (typep b 'struct-block)
                      (not (member (type-of b) types)))
             (pushnew (type-of b) types)
             (funcall 'struct-def b stream))))

(defmethod structvars ((p patch) &optional (stream t))
  (loop for b in (lin-schedule p)
        do (structvars b stream)))

(defmethod structvars ((p basic-block) &optional (stream t))
  stream nil)

(defmethod variable-reader-code ((b t) stream) stream nil)

(defmethod variable-reader-code ((b terminal) stream) 
  (when (creader b)
    (let* ((dim (datasize b))
           (type (get (datatype b) 'ctype))
           (name (varname b))
           (rname (format nil "get_~a" name)))
      (when (var-p b)
        (format stream "~%extern ~a ~a" type rname)
        (cond ((equal dim '(1))
               (format stream "(){return(~a);}" name))
              ((= (length dim) 1)
               (format stream "(long i){return(~a[i]);}" name))
              ((= (length dim) 2)
               (format stream "(long i, long j){return(~a[i][j]);}"
                       name)))))))


(defmethod variable-writer-code ((b t) stream) stream nil)

(defmethod variable-writer-code ((b terminal) stream)
  (when (cwriter b)
    (let* ((dim (datasize b))
           (type (get (datatype b) 'ctype))
           (name (varname b))
           (rname (format nil "set_~a" name)))
      (when (var-p b)
        (format stream "~%extern ~a ~a" type rname)
        (cond ((equal dim '(1))
               (format stream "(~a x){~a = x;" type name))
              ((= (length dim) 1)
               (format stream "(~a x, long i){~a[i] = x;" type name))
              ((= (length dim) 2)
               (format stream "(~a x, long i, long j){~a[i][j] = x;"
                       type name)))
        (when (typep b 'output)
          (loop for p in (linked-params b)
                do (when (flag p)
                     (format stream " ~a = 1;"
                             (cref (flag p))))))
        (format stream "}")))))


;;; SYSTEM CONTROL

(defmethod at ((b basic-block) &rest rest)
  (let ((out (out b)))
    (cond ((not out)
           (error "Cannot read value from block ~a" b))
          (t (apply #'at out rest)))))

(defmethod at ((b terminal) &rest rest)
  (let* ((dim (datasize b))
         (rd (creader b))
         ind1 ind2)
    (unless (and rd (or (functionp rd)
                        (and (symbolp rd) (not (eq rd 't)))))
      (error "Cannot read value from variable ~a" b))
    (cond ((equal dim '(1))
           (unless (null rest)
             (error "Incorrect indexing in ~a" b))
           (funcall rd))
          ((= (length dim) 1)
           (unless (and (= (length rest) 1)
                        (>= (setq ind1 (car rest)) 0)
                        (< ind1 (car dim)))
             (error "Incorrect indexing in ~a" b))
           (funcall rd ind1))
          ((= (length dim) 2)
           (unless (and (= (length rest) 2)
                        (>= (setq ind1 (car rest)) 0)
                        (>= (setq ind2 (second rest)) 0)
                        (< ind1 (car dim))
                        (< ind2 (second dim)))
             (error "Incorrect indexing in ~a" b))
           (funcall rd ind1 ind2)))))


#|
(defmethod (setf at) (value (b basic-block) &rest rest)
  (let ((out (out b)) rd)
    (cond ((or (not out) (not (setq rd (creader out)))
               (not (symbolp rd)) (eq rd t))
           (error "Cannot write value to block ~a" b))
          (t (apply #'(setf at) value out rest)))))
|#

(defmethod (setf at) (value (b basic-block) &rest rest)
  (let ((out (out b)) wr)
    (cond ((or (not out) (not (setq wr (cwriter out)))
            ;   (symbolp wr)
               (eq wr t))
           (error "Cannot write value to block ~a" b))
          (t (apply #'(setf at) value out rest)))))

(defmethod (setf at) (value (b terminal) &rest rest)
  (let* ((dim (datasize b))
         (wr (cwriter b))
         ind1 ind2)
    (setq value (fit.value value (datatype b)))
    (unless (and wr (or (functionp wr)
                        (and (symbolp wr) (not (eq wr 't)))))
      (error "Cannot write value to variable ~a" b))
    (cond ((equal dim '(1))
           (unless (null rest)
             (error "Incorrect indexing in ~a" b))
           (funcall wr value))
          ((= (length dim) 1)
           (unless (and (= (length rest) 1)
                        (>= (setq ind1 (car rest)) 0)
                        (< ind1 (car dim)))
             (error "Incorrect indexing in ~a" b))
           (funcall wr value ind1))
          ((= (length dim) 2)
           (unless (and (= (length rest) 2)
                        (>= (setq ind1 (car rest)) 0)
                        (>= (setq ind2 (second rest)) 0)
                        (< ind1 (car dim))
                        (< ind2 (second dim)))
             (error "Incorrect indexing in ~a" b))
           (funcall wr value ind1 ind2))))
  value)

(defmethod trig ((b .trig))
  (setf (at b) 0) nil)


(defmethod fit.value (val type)
  (cond ((eq type '.double)
         (cl:float val 1.0d0))
        ((eq type '.float)
         (cl:float val 1.0s0))
        (t val)))

#|
(defmethod c-code ((p patch) &optional (stream t))
  (let* ((*read-default-float-format* 'double-float)) ;;; ???
    (variable-code p stream)
    (struct-code p stream)
    (structvars p stream)
    (body-code p stream)))

(defmethod write-patch ((p patch))
  (let* ((*read-default-float-format* 'double-float)) ;;; ???
    (write-patch% p)))
|#

(defmethod c-code ((p patch) &optional (stream t))
  (variable-code p stream)
  (struct-code p stream)
  (structvars p stream)
  (body-code p stream))

(defmethod write-patch ((p patch))
  (write-patch% p))

(defmethod compile-patch ((p patch))
  (when (eq (state p) :removed)
    (error "Cannot compile removed patch"))
  (when (eq (state p) :running)
    (error "Cannot compile running patch"))
  (write-patch% p)
  (bc-compile% p)
  (unless (state p)
    (setf (state p) :compiled))
  t)

(defmethod load-patch ((p patch) &optional (forced nil))
  (when (eq (state p) :removed)
    (error "Cannot load disposed patch"))
  (when (or (not (state p)) forced) (compile-patch p))
  (when (or (not (eq (state p) :loaded)) forced)
    (make-runtime p)
    (setf (state p) :loaded))
  t)

(defmethod step-patch ((p patch) &optional (n 1) (time nil))
  (when (eq (state p) :running)
    (error "Patch ~a running, cannot step" p))
  (unless (eq (state p) :loaded) (load-patch p))
  (let* ((fun (patchfun (runtime p))))
    (if time (time (dotimes (i n) (funcall fun)))
        (dotimes (i n) (funcall fun))))
  t)

(defmethod step-patch-n ((p patch) n &optional (time nil))
  (when (eq (state p) :running)
    (error "Patch ~a running, cannot step" p))
  (unless (< 0 n 110000000)
    (error "Invadil step-patch-n count ~a" n))
  (unless (eq (state p) :loaded) (load-patch p))
  (let* ((fun (npatchfun (runtime p))))
    (if time (time (funcall fun n))
        (funcall fun n)))
  t)

#|
(c-code p)
(write-patch p)
(time
 (compile-patch p)
 )
(load-patch p)
(step-patch p)
(step-patch p 1000000 t)

(c-code p)
(inspect p)
(funcall (creader (out (elt (block-items p) 1))) 0 0)
(funcall (creader (out (elt (block-items p) 1))) 1)
(funcall (creader (out (elt (block-items p) 1))))
(at (elt (block-items p) 1))
(at (elt (block-items p) 1) 1)
(setf (at (elt (block-items p) 2) 1) pi)

(funcall (patchfun (runtime p)))
(time (loop with fun = (patchfun (runtime p))
            for i from 0 below 1000000
            do (funcall fun)))

CLISP Mac gcc (tai cc) call:
(ext:run-shell-command "gcc koex.c -O3 -bundle -o koex")
(ext:shell "gcc koex.c -O3 -bundle -o koex")

MCL:
(bsd:system-command string)
%callsys in BC-framework
|#

;;; MATLAB INTERFACE

(defmethod init-mcode ((obj blockvar) &optional (stream t))
  (map.matlab (lvar obj) (varname obj) stream))

(defmethod init-mcode ((obj blockvar0) &optional (stream t))
  (let* ((lvar (lvar obj))
         (vnam (varname obj))
         (arrdim (array-dimensions lvar))
         (rows (first arrdim))
         (cols (if (> (length arrdim) 1)
                 (second arrdim) 1)))
    (format stream "~%~a = zeros(~a,~a);"
            vnam rows cols)))

(defmethod init-mcode :after ((obj blockvar0+) &optional (stream t))
  (loop with arr = (lvar obj)
        for i from 0 below (length arr)
        for x = (aref arr i)
        do (when (not (= x 0.0))
             (format stream " ~a(~a) = ~a;"
                     (varname obj) (1+ i) x)
             (return nil))))

(defmethod init-mcode :after ((obj indexvar) &optional (stream t))
  (format stream " ~a = ~a+1;" (varname obj) (varname obj)))

(defmethod mtl-global ((obj blockvar) &optional (stream t))
  (format stream "~%global ~a;" (varname obj)))


(defmethod to-matlab ((p patch) &key
                        (init-file "bc_init.m")
                        (step-file "bc_step.m")
                        (matdir "BC3")
                        (step-function-p nil))
  (let* ((sep (if (eq *platform* 'MCL) ":" "/")))
    (setq init-file (format nil "~a~a~a~a~a" *matlab* sep matdir sep init-file))
    (setq step-file (format nil "~a~a~a~a~a" *matlab* sep matdir sep step-file))
    (with-open-file
      (stream init-file :if-exists :supersede
              :if-does-not-exist :create :direction :output)
      (format stream "%%% BC PATCH INITIALIZATION")
      (format stream "~%global SRATE;")
      (loop for o in (used-variables p) do (mtl-global o stream))
      (format stream "~%SRATE = ~a;" (srate p))
      (loop for o in (used-variables p) do (init-mcode o stream))
      (format stream "~%clear bc_init bc_step;"))
    (with-open-file
      (stream step-file :if-exists :supersede
              :if-does-not-exist :create :direction :output)
      (when step-function-p (format stream "function [] = bc_step()"))
      (format stream "~%% BC PATCH STEP")
      (when step-function-p (format stream "~%global SRATE;"))
      (when step-function-p 
        (loop for o in (used-variables p) do (mtl-global o stream)))
      (loop for b in (lin-schedule p)
            for varp = (var-p b)
            do (when varp (mcode* b stream))))))

(defmethod mcode* ((b basic-block) &optional (stream t))
  (when (var-p b)
    (let* ((mr (mrate b))
           (mr* (mrate* b)))
      (cond ((and (not mr) (not mr*)) (mcode b stream))
            ((and (rationalp mr) (= (numerator mr) 1))
             (let* ((denom (denominator mr))
                    (cnt (varname (mphase b))))
               (format stream "~%LC = ~a;~%if (LC==1)" cnt)
               (mcode b stream)
               (format stream "~%~a = ~a;" cnt denom)
               (format stream "~%else ~a = LC-1; end" cnt)))
            ((and (rationalp mr*) (= (numerator mr*) 1))
             (let* ((denom (denominator mr*))
                    (cnt (varname (mphase b)))
                    (out (varname (out b))))
               (format stream "~%LC = ~a;~%if (LC==1)" cnt)
               (mcode b stream)
               (format stream "~%~a = ~a;" cnt denom)
               (format stream "~%else ~a(:,:) = 0.0; ~a = LC-1; ~%end" out cnt)))
            ((and (integerp mr))
             (format stream "~%for LC = 1:~a" mr)
             (mcode b stream)
             (format stream "~%end"))
            (t (error "Multirate specification error")))))
  nil)

#|
(to-matlab px)
|#


(defun map.matlab (val name &optional (stream t))
  (cond ((numberp val) (format stream "~%~a = ~a;"
                               name (cl:float val 0.0d0)))
        ((= (length (array-dimensions val)) 1)
         (format stream "~%~a = [" name)
         (loop for x across val
               do (format stream "~a;" (cl:float x 0.0d0)))
         (format stream "];"))
        (t (format stream "~%~a = [" name)
           (loop with dim = (array-dimensions val)
                 for i from 0 below (first dim)
                 do (loop for j from 0 below (second dim)
                          for x = (cl:float (aref val i j) 0.0d0)
                          do (format stream " ~a" x))
                 do (format stream "; "))
           (format stream "];"))))

(export 'to-matlab)
(import 'to-matlab :common-lisp)


(defmethod matlab-response ((p patch) &key
                               (resp-file "bc_resp.m")
                               (matdir "BC3")
                               (to-matlab t)
                               pre midpre midpost post
                               (outputs '("out"))
                               samples
                               duration)
  (when to-matlab (to-matlab p))
  (let* ((sep (if (eq *platform* 'MCL) ":" "/"))
         (srate (srate p)) len)
    (cond ((and samples (not duration)) (setq len samples))
          ((and (not samples) (not duration))
           (error "Duration or samples must be given in matlab-response"))
          (t (setq len (floor (* duration srate)))))
    (setq resp-file (format nil "~a~a~a~a~a" *matlab* sep matdir sep resp-file))
    (with-open-file
      (stream resp-file :if-exists :supersede
              :if-does-not-exist :create :direction :output)
      (format stream "%%% BC RESPONSE ANALYSIS")
      (format stream "~%bc_init;")
      (when pre (format stream "~%~a" pre))
      (loop for out in outputs
            do (format stream "
~a_resp = [~a;zeros(~a-1,1)];" out out len)
            do (when midpost (format stream "~%~a" midpost)))
      (format stream "~%for I_LOOP = 2:~a" len)
      (when midpre (format stream "~%~a" midpre))
      (format stream "~%  bc_step;")
      (loop for out in outputs
            do (format stream "~%  ~a_resp(I_LOOP) = ~a;" out out))
      (format stream "~%end")
      (when post (format stream "~%~a" post))
      nil)))


(defun data-to-matlab (data &key
                               (resp-file "bc_data.m")
                               (matdir "BC3"))
  (unless (and (consp data)
               (every #'consp data))
    (error "Invalid data spec ~a in data-to-matlab" data))
  (let* ((sep (if (eq *platform* 'MCL) ":" "/"))
         (resp-file (format nil "~a~a~a~a~a"
                            *matlab* sep matdir sep resp-file)))
    (with-open-file
      (stream resp-file :if-exists :supersede
              :if-does-not-exist :create :direction :output)
      (format stream "%%% BC DATA TO MATLAB")
      (loop for x in data
            for name = (first x)
            for dx = (second x)
            do (unless (or (symbolp name) (stringp name))
                 (error "Invalid data name ~a in data-to-matlab" name))
            do (format stream "~%~a = " name)
            do (cond ((numberp dx) (format stream "~a;" dx))
                     ((consp dx)
                      (format stream "[")
                      (loop for xx in dx
                            do (unless (numberp xx)
                                 (error "~a not a number for data-to-matlab" xx))
                            do (format stream " ~a" xx))
                      (format stream "];"))
                     (t (arrayp dx) nil))))
    nil))


(defmethod matlab-result ((p patch) &key
                             (resp-file "bc_resp.m")
                             (matdir "BC3")
                             pre post
                             (outputs '("out"))
                             samples
                             duration)
  (let* ((sep (if (eq *platform* 'MCL) ":" "/"))
         (srate (srate p)) res len)
    (cond ((and samples (not duration)) (setq len samples))
          ((and (not samples) (not duration))
           (error "Duration or samples must be given in matlab-result"))
          (t (setq len (floor (* duration srate)))))
    (loop for onam in outputs
          for r = (probe-response p :probe-name onam :samples len)
          for rn = (format nil "~a_resp" onam)
          do (push (list rn r) res))
    (data-to-matlab
     (cons (list "SRATE" (srate p)) (reverse res)))
    (setq resp-file (format nil "~a~a~a~a~a" *matlab* sep matdir sep resp-file))
    (when (probe-file resp-file)
      (delete-file resp-file))
    (with-open-file
      (stream resp-file :if-exists :supersede
              :if-does-not-exist :create :direction :output)
      (format stream "%%% BC RESPONSE ANALYSIS")
      (when pre (format stream "~%~a" pre))
      (format stream "~%~%bc_data")
      (when post (format stream "~%~a" post)))
    nil))

#|
(map.matlab 12.0 'float12)
(map.matlab #(12.0 3) 'float12)
(map.matlab #(12.0 3) 'float12)
(map.matlab
 (make-array
  '(2 3) :initial-element 10.0) 'float12)
(map.matlab
 (make-array
  '(2 3) :initial-contents '((1 2 3) (4 5 6))) 'float12)

(data-to-matlab `(("aaa" 13)))
(data-to-matlab `(("aaa" (1 2 3 4))))

(defparameter x (make-list 44100 :initial-element 2.1))
(data-to-matlab `(("aaa" ,x)))
|#

(defmethod mcode ((b t) &optional (stream t))
  stream nil)

(defmethod infix-code ((b t) op &optional (stream t))
  (let* ((ins (inputs b)))
    (format stream "~%~a = " (varname (out b)))
    (format stream "~a" (varname (prev-out (first ins))))
    (loop for inx in (cdr (inputs b))
          for vn = (varname (prev-out inx))
          do (format stream "~a~a" op vn))
    (format stream ";")))


;;; READ AND WRITE MATLAB ACII DATA

(defun read-mdata% (file &optional transpose)
  (with-open-file (str file :direction :input)
    (do (char nxtchar chars nam siz arr forms) '()
      (setq chars nil)
      (loop for x = (read-preserving-whitespace 
                     str nil nil)
            do (when (not x) (return-from read-mdata% forms))
            do (setq char (code-char (round x)))
            do (setq nxtchar (read-char str))
            do (push char chars)
            do (when (or (eql nxtchar #\Linefeed)
                         (eql nxtchar #\Return)) (return nil)))
      (setq nam (coerce (reverse chars) 'string))
      (setq siz (list (round (read str)) (round (read str))))
      (setq arr (make-array (if transpose (reverse siz) siz)))
      (loop for i from 0 below (first siz)
            do (loop for j from 0 below (second siz)
                     for x = (read str)
                     do (if transpose (setf (aref arr j i) x)
                            (setf (aref arr i j) x))))
      (setq siz (array-dimensions arr))
      (when (= (first siz) 1)
        (let ((arrx (make-array (second siz))))
          (loop for i from 0 below (second siz)
                do (setf (aref arrx i) (aref arr 0 i)))
          (setq arr arrx)))
      (push (list nam arr) forms)
      nil)))

(defun read-mdata (file &optional transpose)
  (let ((data (read-mdata% file transpose)))
    (loop for d in data
          for var = (read-from-string (first d))
          for arr = (second d)
          do (set var arr))))

#|
; (choose-file-dialog)
; (read-mdata% "Macintosh HD:Users:mak:Desktop:parfilt:aaa.bcd")
; (read-mdata "Macintosh HD:Users:mak:Desktop:parfilt:aaa.bcd")
; (read-mdata "Macintosh HD:Users:mak:Desktop:parfilt:bridgemodes.bcd")
; (read-mdata "Macintosh HD:Users:mak:Desktop:parfilt:bridgemodes.bcd" t)
; a0
; (inspect (.var am))
; (inspect (.Z2ser :a1 a1 :a2 a2 :b0 b0 :b1 b1 :b2 b2))


(defpatch koe ((z (.dual (.Z2ser :a1 a1 :a2 a2 :b0 b0 :b1 b1 :b2 b2)))
               (e (.E (.imp1 1.0) 1.0e-6)))
  (.par e z)
  (-> (.current z) (.probe "out")))


(load-patch koe)
(step-patch koe 44100 t)

(matlab-response koe
                 :samples 8000
                 :outputs '("out")
                 :post "
figure(20); clf;
%plot_sig(out_resp,SRATE); grid on; 
%plot(out_resp); grid on; 
plot_logspect(out_resp,SRATE); grid on; 
axis([80 20000 -20 20]);
")

(defpatch koe ((z (.dual (.Z2ser :a1 a1 :a2 a2 :b0 b0 :b1 b1 :b2 b2)))
               (tr (.trig-imp1))
               (e (.E (.mul tr 5.0) 1.0e-6)))
  (.par z e)
  (-> (.current z) (inputs (.da)))
  (defun trg () (trig tr))
  )

(inspect koe)
(load-patch koe)
(step-patch koe)
(run-patch koe)
(trg)
(stop-patch koe)

(defpatch koe ((z (.dual (.Z2ser :a1 a1 :a2 a2 :b0 b0 :b1 b1 :b2 b2)))
               (tr (.imp1))
               (e (.E tr 1.0e-6)))
  (.par z e)
  (-> (.current z) (.probe "out"))
  (defun trg () (trig tr))
  )

(matlab-response koe
                 :samples 8000
                 :outputs '("out")
                 :post "
figure(20); clf;
%plot_sig(out_resp,SRATE); grid on; 
%plot(out_resp); grid on; 
plot_sig(out_resp,SRATE); grid on; 
%axis([80 20000 -20 20]);
")

(defpatch koe ((z (.dual (.Z2ser :a1 a1 :a2 a2 :b0 b0 :b1 b1 :b2 b2)))
               (tr (.trig-imp1))
               (dl (.dline-n 1.0 :delay-length 269))
               (e (.E (.mul tr 10000.0) 10000.0)))
  (-> (.current z) (inputs (.da)))
  (.par e (port dl 0))
  (.par (port dl 1) z)
  (defun trg () (trig tr))
  )


(inspect (.Z2ser :a1 a1 :a2 a2 :b0 b0 :b1 b1 :b2 b2))
|#


;;; FILE I/O

(defun read-ascii-data (file &aux x res)
  (with-open-file (str (bc-file file))
    (loop for line = (read-line str nil nil)
          while line
          do (when (and (not (string-equal line ""))
                        (not (equal (position #\; line) 0)))
               (setq x (read-from-string
                        (concatenate 'string "(" line ")")))
               (push x res)))
    (reverse res)))

#|
(defparameter dx (read-ascii-data "Spatial/Kemar_0_128.txt"))
(time (defparameter dx (read-ascii-data "Spatial/Kemar_0_128.txt")))
(inspect dx)
(length dx)
(length (car dx))

(defpatch hx ((hrtf (read-ascii-data "Spatial/Kemar_0_128.txt"))
              (hvar (.var hrtf :out-type '.float)))
  (-> hvar (.probe "out")))

(inspect hx)
(c-code hx)
(time (compile-patch hx))
(step-patch-n hx 1000 t)

(datasize (out (elt (block-items hx) 1)))
(/ 360.0 (1- (first (datasize (out (elt (block-items hx) 1))))))
|#


(provide :BC-system)
