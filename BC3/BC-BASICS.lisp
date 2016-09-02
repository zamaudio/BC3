
(in-package :BC)


;;; PATCHES & BLOCKS
;;; ----------------

(eval-when (eval compile load)
  (defparameter *current-patch* nil)
  (defparameter *patches* nil)
  (defparameter *srate* 48000.0d0)
  (defparameter *default-type* '.double)
  )


(defclass block-item ()
  ((presentations :initarg :presentations :accessor presentations))
  (:default-initargs
    :presentations nil))

(defclass patch-mixin (block-item)
  ((block-items :initarg :block-items :accessor block-items)
   (lin-schedule :initform nil :accessor lin-schedule))
  (:default-initargs
    :block-items nil))

(defmethod srate ((p patch-mixin))
  (cond ((mrate p)
         (* (mrate p) (srate (host-patch p))))
        ((mrate* p)
         (* (mrate* p) (srate (host-patch p))))
        (t (srate (host-patch p)))))


(defmethod srate ((block (eql nil)))
  (let ((srate *srate*))
    srate))


;;; SIGNAL TERMINALS

(defclass terminal (block-item)
  ((host-block :initarg :host-block :accessor host-block)
   (domain :initarg :domain :accessor domain)
   (name :initarg :name :accessor name)
   (symname :initform nil :accessor symname))
  (:default-initargs
    :host-block nil
    :domain nil
    :name nil))

(defmethod top-host-block ((obj terminal))
  (let ((x (host-block obj)))
    (loop for host = (host-patch x)
          while (and host (not (typep host 'patch))
                     (or (member obj (inputs host))
                         (member obj (outputs host))
                         (member obj (params host))
                         (member obj (ports host))))
          do (setq x host))
    x))


;;; INPUTS 

(defclass input (terminal)
  ((link :initarg :link :accessor link))
  (:default-initargs
    :link nil))

(defmethod print-object ((port input) (stream t))
  (let* ((name (name port))
         (host (top-host-block port))
         (index (when host
                  (position port (inputs host)))))
    (print-unreadable-object (port stream :type t :identity t)
      (when index (format stream "~a" index))
      (when name (format stream "<~a>" name))
      (when host (format stream " in ~a" (type-of host))))))

(defmethod input ((p input) &optional index provide-p)
  (declare (ignore index provide-p))
  p)

(defmethod prev-out ((port input))
  (let* ((pout (link port)) block ppout)
    (unless pout (error "Input ~a not connected" port))
    (setq block (host-block pout))
    (unless block (error "No host block for ~a" pout))
    (setq ppout (prev-out block))
    (if ppout ppout pout)))

(defmethod prev-out ((b t)) nil)

(defmethod mname ((port input))
  (varname (prev-out port)))


;;; PARAMS

(defclass param (input)
  ((flag :initarg :flag :accessor flag))
  (:default-initargs
    :flag nil))

(defmethod initialize-instance :after ((port param) &key)
  (when (and (host-block port) (flag port))
    (let* ((fb (make-instance 'flag
                 :host-block (host-block port))))
      (pushnew fb (variables (host-block port)))
      (setf (flag port) fb))))

(defmethod print-object ((port param) (stream t))
  (let* ((name (name port))
         (host (when (host-block port) (top-host-block port)))
         (index (when host
                  (position port (params host)))))
    (print-unreadable-object (port stream :type t :identity t)
      (when index (format stream "~a" index))
      (when name (format stream "<~a>" name))
      (when host (format stream " in <~a>" (type-of host))))))

(defmethod mname ((port param))
  (varname (prev-out port)))


;;; BLOCKVARS

; (defmethod varname ((b t)) (error "No varname for ~a" b))

(defclass blockvar (terminal)
  ((lvar :initarg :value :accessor lvar) ;;; lisp-value
   (creader :initarg :creader :accessor creader)  ;;; c-reader
   (cwriter :initarg :cwriter :accessor cwriter)  ;;; c-writer
   (var-p :initarg :var-p :accessor var-p)
   (varname :initarg :varname)
   (phystype :initarg :phystype :accessor phystype)
   (datatype :initarg :type :accessor datatype)
   (datasize :initarg :size :accessor datasize)
   (trig :initarg :trig :accessor trig))
  (:default-initargs
    :value 0.0d0
    :creader nil
    :cwriter nil
    :var-p :undefined
    :varname nil
    :phystype nil
    :type nil
    :size nil
    :name 'sigout
    :trig t))

(defparameter *variable-prefix* nil)

(defmethod (setf varname) (name (b blockvar))
  (setf (slot-value b 'varname) name))
  
(defmethod varname ((b blockvar))
  (if *variable-prefix*
    (format nil "~a~a"
            *variable-prefix*
            (slot-value b 'varname))
    (slot-value b 'varname)))

(defmethod mname ((b blockvar))
  (varname b))

(defmethod value ((bv blockvar))
  (let* ((siz (datasize bv))
         (rd (creader bv)))
    (if (and rd (not (eq rd t)) bv)
      (let* ((arr (make-array siz)))
        (cond ((equal (datasize bv) '(1))
               (setf (aref arr 0)
                     (funcall 'at bv)))
              ((= (length siz) 1)
               (loop for i from 0 below (first siz)
                     for x = (funcall 'at bv i)
                     do (setf (aref arr i) x)))
              ((= (length siz) 2)
               (loop for i from 0 below (first siz)
                     do (loop for j from 0 below (second siz)
                              for x = (funcall 'at bv i j)
                              do (setf (aref arr i j) x)))))
        arr)
      (lvar bv))))
               

(defclass blockvar0 (blockvar) ()) ;;; zeros initial state

(defclass blockvar0+ (blockvar0) ()) ;;; extra nonzero

(defclass indexvar (blockvar) () ;;; .long index variable
   (:default-initargs
     :value 0 :type '.long))

(defclass flag (blockvar) () ;;; .long index variable
   (:default-initargs
     :size '(1)
     :value 0 :type '.short))


;;; OUTPUTS

(defclass output (blockvar)
  ((links :initarg :links :accessor links)
   (scheduled :initarg :scheduled :accessor scheduled))
  (:default-initargs
    :links nil
    :value 0.0d0
    :var-p :undefined
    :varname nil
    :phystype nil
    :type nil
    :size nil
    :name 'sigout
    :trig t
    :scheduled nil))

(defmethod output ((p output) &optional index error-p)
  (declare (ignore error-p))
  (when index (error "Don't use index in (output output)"))
  p)

(defmethod prev-out ((port output)) 
  (let* ((h (host-block port)))
    (cond ((typep h '.x.) (prev-out (in h)))
          ((typep h '.p.) (prev-out (param h)))
          (t port))))

(defmethod print-object ((port output) (stream t))
  (let* ((name (name port))
         (host (top-host-block port))
         (index (when host
                  (position port (outputs host)))))
    (print-unreadable-object (port stream :type t :identity t)
      (when index (format stream "~a" index))
      (when name (format stream "<~a>" name))
      (when host (format stream " in ~a" (type-of host))))))

(defmethod linked-params ((p output))
  (let* ((res nil))
    (loop for inx in (links p)
          for hx = (host-block inx)
          do (cond ((typep inx 'param)
                    (pushnew inx res))
                   ((typep hx 'fwd-block)
                    (let* ((ox (out hx))
                           (ps (linked-params ox)))
                      (dolist (x ps) (pushnew x res))))))
    res))


;;; CONNECTING OF BLOCKS

(defmethod connect ((p1 t) (p2 t))
  (error "Connection of ~a and ~a not allowed" p1 p2))

(defmethod connect ((p1 input) (p2 output))
  (when (link p1)
    (error "Input ~a already connected" p1))
  (setf (link p1) p2)
  (push p1 (links p2))
  t)

(defmethod connect ((p1 output) (p2 input))
  (connect p2 p1) t)

(defmethod connect ((p1 output) (p2 param))
  (connect p2 p1) t)

(defmethod disconnect ((p input))
  (let* ((out (link p)))
    (cond (out (setf (link p) nil)
               (setf (links out) (remove p (links out))))
          (t (error "Input ~a not linked" p)))
    nil))


;;; BLOCKS

(defparameter *current-block* nil)

(defclass basic-block (block-item)
  ((host-patch :initarg :host-patch :accessor host-patch)
   (inputs :initarg :inputs :accessor inputs)
   (params :initarg :params :accessor params)
   (ports :initarg :ports :accessor ports)
   (outputs :initarg :outputs :accessor outputs)
   (registers :initform nil :accessor registers)
   (variables :initarg :variables :accessor variables)
   (var-p :initarg :var-p :accessor var-p)
   (typed :initarg :typed :accessor typed)
   (out-type :initarg :out-type :accessor out-type)
   (default-type :initarg :default-type :accessor default-type)
   (type-model :initarg :type-model :accessor type-model)
   (sized :initarg :sized :accessor sized)
   (out-size :initarg :out-size :accessor out-size)
   (default-size :initarg :default-size :accessor default-size)
   (out-init :initarg :value :accessor out-init)
   (default-init :initarg :default-init :accessor default-init)
   (finalized :initarg :finalized :accessor finalized)
   (delayedp :initarg :delayedp :accessor delayedp)
   (scheduled :initform nil :accessor scheduled)
   (mrate :initarg :mrate :accessor mrate)
   (mrate* :initarg :mrate* :accessor mrate*)
   (mphase :initarg :mphase :accessor mphase)
   (counter :initform nil :reader counter)
   (name :initarg :name :accessor name))
  (:default-initargs
    :host-patch nil
    :inputs nil
    :params nil
    :ports nil
    :outputs nil
    :variables nil
    :var-p :undefined
    :typed nil
    :out-type nil
    :default-type *default-type*
    :type-model nil
    :value nil
    :default-init 0.0d0
    :sized nil
    :out-size nil
    :default-size '(1)
    :finalized t
    :delayedp nil
    :mrate nil
    :mrate* nil
    :mphase 0
    :name nil))

(defmethod initialize-instance :after ((block basic-block) &key)
  (cond ((host-patch block)
         (push block (block-items (host-patch block))))
        (*current-patch*
         (push block (block-items *current-patch*))
         (setf (host-patch block) *current-patch*))
        (t nil))
  (when (or (rationalp (mrate block))
            (rationalp (mrate* block)))
    (let* ((phase (mphase block))
           (val (make-array 1 :initial-element phase))
           (v (make-instance 'indexvar
                :host-block block
                :size '(1) :value val)))
      (push v (variables block))
      (setf (mphase block) v))))

(defmethod print-object ((b basic-block) (stream t))
  (let* ((name (name b)))
    (print-unreadable-object (b stream :type t :identity t)
      (when name (format stream "\"~a\"" name)))
    b))

(defmethod lcode ((b basic-block)) nil)

(defmethod input ((block basic-block) &optional (index 0) provide-p)
  (declare (ignore provide-p))
  (let* ((ins (inputs block))
         (len (length ins)) in)
    (cond ((integerp index)
           (unless (<= 0 index (1- len))
             (error "Input index ~a beyond bounds for ~a" index block))
           (elt ins index))
          (t (setq in (find index ins :key #'name
                            :test #'(lambda (x y)
                                      (string-equal (string x) (string y)))))
             (unless (and in index)
               (error "Input ~a not found for block ~a" index block))
             in))))

(defmethod in ((block basic-block) &optional (index 0) provide-p)
  (input block index provide-p))

(defmethod output ((block basic-block) &optional (index 0) (error-p t))
  (let* ((outs (outputs block))
         (len (length outs)) out)
    (cond ((integerp index)
           (unless (<= 0 index (1- len))
             (if error-p
               (error "Output index ~a beyond bounds for ~a" index block)
               (return-from output nil)))
           (elt outs index))
          (t (setq out (find index outs :key #'name
                            :test #'(lambda (x y)
                                      (string-equal (string x) (string y)))))
             (unless (and out index)
               (when error-p
                 (error "Output ~a not found for block ~a" index block)
                 (return-from output nil)))
             out))))

(defmethod out ((block basic-block) &optional (index 0))
  (output block index))

(defmethod out ((block output) &optional (index 0))
  index block)

(defmethod param ((block basic-block) &optional (index 0) (error-p t))
  (let* ((pars (params block))
         (len (length pars)) par)
    (cond ((integerp index)
           (unless (<= 0 index (1- len))
             (if error-p
               (error "Param index ~a beyond bounds for ~a" index block)
               (return-from param nil)))
           (elt pars index))
          (t (setq par (find index pars :key #'name
                             :test #'(lambda (x y)
                                      (string-equal (string x) (string y)))))
             (unless (and par index)
               (when error-p
                 (error "Param ~a not found for block ~a" index block)
                 (return-from param nil)))
             par))))

(defmethod make-param ((b basic-block) value &optional 
                         (out-type (out-type b))
                         (pname 'pval))
  (cond ((or (not value) (eq value 't))
         (setq value (.p. :name pname))
         (setf (name (param value)) pname)
         (push (param value) (params b))
         value)
        ((typep value 'data-block) value)
        (t (setq value (funcall out-type value))))
  value)

(defmethod port ((block basic-block) &optional (index 0) provide-p)
  (declare (ignore provide-p))
  (let* ((ports (ports block))
         (len (length ports)) port)
    (cond ((integerp index)
           (unless (<= 0 index (1- len))
             (error "Port index ~a beyond bounds for ~a" index block))
           (elt ports index))
          (t (setq port (find index ports :key #'name
                              :test #'(lambda (x y)
                                        (string-equal (string x) (string y)))))
             (unless (and port index)
               (error "Port ~a not found for block ~a" index block))
             port))))

(defmethod srate ((block basic-block))
  (cond ((not (host-patch block))
         *srate*)
        ((mrate block)
         (* (mrate block) (srate (host-patch block))))
        ((mrate* block)
         (* (mrate* block) (srate (host-patch block))))
        (t (srate (host-patch block)))))


;;; Special blocks

(defclass dynamic-block (basic-block) ())

(defclass struct-block (basic-block)
  ((inlinep :initarg :inline :accessor inlinep))
  (:default-initargs
    :inline t))

(defmethod struct-def ((obj struct-block) &optional (stream t))
  (struct-def (funcall 'struct obj) stream))

(defmethod structvars ((obj struct-block) &optional (stream t))
  (struct-spec (funcall 'struct obj) stream))


(defclass delayed-block (dynamic-block)
  ((out-checked :initform -1 :accessor out-checked))
  (:default-initargs
    :delayedp t))

(defmethod first-step-p ((b t)) t)
(defmethod first-step-p ((b delayed-block)) nil)

(defmethod set-non-scheduled ((block delayed-block))
  (set-scheduled block nil))


;;; Macro block

(defclass macro-block (patch-mixin basic-block)
  ((dummy :initarg :dummy :accessor dummy))
  (:default-initargs
    :dummy nil))

(defmethod initialize-instance :after ((block macro-block) 
                                           &rest rest 
                                           &key &allow-other-keys)
  (when (eq (dummy block) t)
    (setf (dummy block) (make-instance '.dummy :host-patch block)))
  (let ((*current-patch* block))
    (declare (special *current-patch*))
    (setq block (apply #'initialize-macro-block block rest)))
  nil)

(defgeneric initialize-macro-block (block &key &allow-other-keys))

(defmethod initialize-macro-block ((block t) &key &allow-other-keys)
  (error "No definition to make a macro block"))

(defmethod finalize-block :around ((b macro-block))
  (let ((*current-patch* b))
    (declare (special *current-patch*))
    (call-next-method)))

(defmethod ready-p ((block macro-block))
  (cond ((every #'(lambda (x) (eq (ready-p x) :done))
                (block-items block)) :done)
        ((every #'ready-p (block-items block)) t)
        (t nil)))

(defmethod add-param ((b macro-block) value param
                        &key name)
  (when name (setf (name param) name))
  (cond ((or (not value) (eq value 't))
         (push param (params b)))
        ((or (typep value 'basic-block)
             (typep value 'output))
         (push param (params b))
         (-> value param))
        (t (setq value (.const value))
           (-> value param)))
  param)

(defmacro def-macro-block (class params &body body)
  `(progn
     (defclass ,class (macro-block) ())
     (defmethod initialize-macro-block ((block ,class) ,@params)
       (let* ((*current-patch* block))
         (flet ((set-inputs (&rest inputs)
                  (setf (inputs block) (remove nil inputs)))
                (set-params (&rest params)
                  (setf (params block) (remove nil params)))
                (set-outputs (&rest outputs)
                  (setf (outputs block) (remove nil outputs)))
                (set-ports (&rest ports)
                  (setf (ports block) (remove nil ports))))
           ,@body)))
     (defun ,class (&rest rest)
       (apply #'make-instance ',class rest))))

(defmacro def-macro-block-nofun (class params &body body)
  `(progn
     (defclass ,class (macro-block) ())
     (defmethod initialize-macro-block ((block ,class) ,@params)
       (let* ((*current-patch* block))
         (flet ((set-inputs (&rest inputs)
                  (setf (inputs block) (remove nil inputs)))
                (set-params (&rest params)
                  (setf (params block) (remove nil params)))
                (set-outputs (&rest outputs)
                  (setf (outputs block) (remove nil outputs)))
                (set-ports (&rest ports)
                  (setf (ports block) (remove nil ports))))
           ,@body)))
     ',class))


;;; MACRO-GROUP

(defclass macro-group (macro-block) ())


;;; SISO-BLOCK (single in, single out)

(defclass siso-block (basic-block) ())

(defmethod initialize-instance :after ((b siso-block) &key
                                           (value nil) 
                                           (varname nil))
  (setf (inputs b)
        (list (make-instance 'input 
                :host-block b :name 'sigin))
        (outputs b)
        (list (make-instance 'output 
                :host-block b :value value
                :varname varname))))

(defmethod check-sizes ((b siso-block))
  (let* ((osiz (datasize (out b)))
         (isiz (datasize (link (in b)))))
    (unless (equal isiz osiz)
      (error "Data size mismatch in ~a" b))))

#| ?????
(defmethod check-sizes ((b siso-block))
  (let* ((osiz (datasize (out b)))
         (isiz (datasize (prev-out (in b)))))
    (unless (equal isiz osiz)
      (error "Data size mismatch in ~a" b))))
|#

(defmacro def.siso (name &optional (class name))
  (unless (eql (elt (string name) 0) #\.)
    (error "BC function spec name error (~a)" name))
  `(defun ,name (&rest rest &aux block)
     (declare (dynamic-extent rest))
     (cond ((or (not rest) (keywordp (car rest)))
            (apply #'make-instance ',class :value nil rest))
           (t (setq block (apply #'make-instance ',class (cdr rest)))
              (-> (.val (car rest)) block) block))))

(defclass .datac (siso-block) ())

(defmethod set-size :after ((b .datac) size)
  (declare (ignore size))
  (let* ((out (output b))
         (siz (datasize out))
         (typ (get (datatype out) 'ltype))
         (val (lvar out)))
    (setf (var-p (out b)) nil)
    (setf (lvar out) (make-array siz))
    (eval (arr-setval (lvar out) val typ))))

(defun .datac (value &rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.datac 
         :value value rest))

(defclass delay-mixin (delayed-block siso-block) ())


;;; MISO-BLOCK (multiple in, single out)

(defclass miso-block (basic-block) ()
  (:default-initargs
    :inputs 2))

(defmethod initialize-instance :after ((block miso-block) &key
                                           (value nil)
                                           (varname nil))
  (if (integerp (inputs block))
    (setf (inputs block)
          (loop for i from 0 below (inputs block)
                collect (make-instance 'input :host-block block))))
  (setf (outputs block)
        (list (make-instance 'output
                :host-block block :value value
                :varname varname))))

#|
(defmethod check-sizes ((b miso-block))
  (let* ((osiz (datasize (out b))))
    (loop for inx in (inputs b)
          for pout = (prev-out inx)
          for dsiz = (datasize pout)
          do (unless (equal dsiz osiz)
               (error "Data size mismatch in ~a" b)))))
|#

(defmethod check-sizes ((b miso-block))
  (let* ((osiz (datasize (out b))))
    (loop for inx in (inputs b)
          for pout = (prev-out inx)
          for dsiz = (datasize pout)
          do (cond ((equal dsiz osiz) nil)
                   ((equal dsiz '(1))
                    (disconnect inx)
                    (let* ((pt (find-patch b))
                           (ls (lin-schedule pt))
                           (lse (member b ls))
                           (lsb (subseq ls 0 (position b ls)))
                           (cp (funcall '.copy)))
                      (setf (lin-schedule pt)
                            (append lsb (cons cp lse)))
                      (-> (out b) (param cp))
                      (-> pout cp inx)
                      (setf (datatype (out cp))
                            (datatype pout))
                      (finalize-sizes cp)
                      (setf (lvar (out cp))
                            (make-array osiz))
                      nil))
                   (t (error "Data size mismatch in ~a" b))))))

(defmacro def.miso (name &optional (class name))
  (unless (eql (elt (string name) 0) #\.)
    (error "BC function spec name error (~a)" name))
  (let* ((args (gensym))
         (block (gensym))
         (rx (gensym)))
    `(defun ,name (&rest rest)
       (declare (dynamic-extent rest))
       (let* ((,args nil) ,block)
         (loop for ,rx = (car rest)
               while rest 
               do (cond ((not (keywordp ,rx))
                         (push (.val ,rx) ,args)
                         (setq rest (cdr rest)))
                        (t (return nil))))
         (setq ,block
               (if ,args
                 (apply #'make-instance ',class
                        :inputs (length ,args) rest)
                 (apply #'make-instance ',class rest)))
         (loop for ,rx in (reverse ,args)
               for i from 0
               do (cond ((typep ,rx 'basic-block)
                         (-> ,rx (in ,block i)))
                        ((and (symbolp ,rx) ,rx (neq ,rx t))
                         (setf (name (in ,block i)) ,rx))
                        (t nil)))
         ,block))))


;;; ZISO-BLOCK (none in, single out)

(defclass ziso-block (basic-block) ())

(defmethod initialize-instance :after ((b ziso-block) &key
                                           (value nil)
                                           (varname nil))
  (setf (outputs b)
        (list (make-instance 'output
                :host-block b :value value
                :varname varname))))

(defclass data-block (ziso-block) ()
  (:default-initargs :var-p nil))



;;; FORWARDING BLOCKS

(defclass fwd-block () ())

(defclass .x. (fwd-block siso-block) ())

(defmethod prev-out ((b .x.)) (prev-out (in b)))

(defun .x. (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.x. rest))


(defclass .p. (fwd-block ziso-block) ())

(defmethod initialize-instance :after ((b .p.) &key name)
  (setf (params b)
        (list (make-instance 'param :host-block b :name name))))

(defmethod prev-out ((b .p.)) (prev-out (param b)))

(defun .p. (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.p. rest))

(defmethod fwd-out ((out output) &aux (outx out))
  (loop for b = (host-block outx)
        do (cond ((typep b '.x.) (setq outx (link (in b))))
                 ((typep b '.p.) (setq outx (link (param b))))
                 (t (return outx)))))


(defclass .dummy (ziso-block) ())


;;; TERMINAL CHAINING


#|
(defun -> (&rest forms)
  (declare (dynamic-extent forms))
  (loop with hb and hp
        with f0 = (first forms)
        for fx in (cdr forms)
        do (cond ((listp fx)
                  (dolist (x fx) (connect (output f0) x))
                  (setq fx (host-block (first fx))))
                 (t (connect (output f0) (input fx))))
        do (when (typep fx 'input)
             (if (and (setq hb (host-block fx)) ;;; macro block processing (mk 20.8.03)
                      (typep (setq hp (host-patch hb)) 'macro-block)
                      (or (member fx (inputs hp)) (member fx (params hp))))
               (setq fx hp) (setq fx hb))
             (setq fx (when (outputs fx) (output fx))))
        do (setq f0 fx)
        finally (return f0)))
|#

(defun -> (&rest forms)
  (declare (dynamic-extent forms))
  (loop with hb and hp
        with f0 = (first forms)
        for fx in (cdr forms)
        do (cond ((listp fx)
                  (dolist (x fx) (connect (output f0) x))
                  (setq fx (host-block (first fx))))
                 (t (connect (output f0) (input fx))))
        do (when (typep fx 'input)
             (if (and (setq hb (host-block fx)) ;;; macro block processing (mk 20.8.03)
                      (typep (setq hp (host-patch hb)) 'macro-block)
                      (or (member fx (inputs hp)) (member fx (params hp))))
               (setq fx hp) (setq fx hb))
             (setq fx (when (and fx (outputs fx)) (output fx))))
        do (setq f0 fx)
        finally (return f0)))


;;; PATCHES

(defclass patch (patch-mixin)
  ((host-patch :initform nil :accessor host-patch)
   (patchname :initarg :patchname :reader patchname)
   (runtime :initform nil :accessor runtime)
   (lstepfun :initform nil :accessor lstepfun)
   (state :initform nil :accessor state)
   (variables :initform nil :accessor variables)
   (registers :initform nil :accessor registers)
   (srate :initarg :srate :accessor srate))
  (:default-initargs
    :patchname (intern (string (gensym "PATCH"))) ;;; (gentemp "PATCH")
    :srate *srate*
    :block-items nil))

(defmethod print-object ((p patch) (stream t))
  (let* ((name (patchname p)))
    (print-unreadable-object (p stream :type t :identity t)
      (format stream "'~a'" name))
    p))

(defmethod initialize-instance :after ((p patch) &key)
 ;;; (dispose-patch-named (patchname p))
  (push p *patches*))

(defmethod dispose-patch ((p patch))
  (when (eq (state p) :running)
    (funcall 'stop-patch p))
  (let* ((rt (runtime p)))
    (when rt (funcall 'dispose-runtime rt)))
  (setf (state p) :removed)
  p)

(defmethod remove-patch ((p patch))
  (dispose-patch p)
  (setq *patches* (remove p *patches*)))

(defmethod remove-named-patch (name)
  (let ((p (find name *patches* :test #'equal 
                 :key #'patchname)))
    (when p (remove-patch p))))

(defun remove-all-patches ()
  (dolist (p *patches*) (dispose-patch p))
  (setq *patches* nil))


(defmethod find-block ((p patch-mixin) name)
  (find (string name) (block-items p)
        :key #'name :test #'string-equal))

(defmethod find-block ((x symbol) name)
  (let* ((p (get x 'patch)))
    (unless p (error "No such patch ~a" p))
    (find-block p name)))

(defmethod find-patch ((b basic-block))
  (loop with p = (host-patch b)
        while (host-patch p)
        do (setq p (host-patch p))
        finally (return p)))

(defmethod set-non-scheduled ((p patch-mixin))
  (setf (lin-schedule p) nil)
  (dolist (b (block-items p)) (set-non-scheduled b)))


(defparameter *patch-inits* nil)


(defmacro patch (vars &body body)
  (let* ((name? (member 'name vars :key #'car))
         (name (if name? (second (car name?))
                   (intern (string (gensym "PATCH"))))) ;;(gentemp "PATCH")))
         (sname (string name))
         (class? (member 'class vars :key #'car))
         (class (if class? (second (car class?)) ''patch))
         (srate? (member 'srate vars :key #'car))
         (srate (if srate? (second (car srate?)) *srate*))
         (lvars (list* (list '*srate* srate)
                       `(*current-patch*
                         (make-instance ,class :patchname ',sname))
                       vars)))
    (remove-named-patch (string name))
    `(let* ,lvars
       ,(if srate? 'srate) ,(if name? 'name) ,(if class? 'class)
       (dolist (fun *patch-inits*) (funcall fun))
       ,@body
       (process-patch *current-patch*))))


(defmacro defpatch (name vars &body body)
  `(defparameter ,name
     (patch ,vars ,@body)))

(defmacro def-pfun (name (p &rest args) &body body)
  p `(defmethod ,name ((b% patch) ,@args)
       b% ,@body))

(defmethod process-patch ((p patch))
  (check-connected p)
  (finalize-block p)
  (check-connected p)
  (check-domains p)
  (schedule-patch p)
  (finalize-types p)
  (finalize-sizes p)
  (get-variables p)
  (init-values p)
  (make-lstepfun p)
  p)


#|
(defmethod process-patch ((p patch))
  (time (check-connected p))
  (time (finalize-block p))
  (time (check-connected p))
  (time (check-domains p))
  (time (schedule-patch p))
  (time (finalize-types p))
  (time (finalize-sizes p))
  (time (get-variables p))
  (time (init-values p))
  (time (make-lstepfun p))
  p)
|#


(defmethod check-connected ((pm patch))
  (loop for b in (block-items pm)
        do (check-connected b)))

(defmethod check-connected ((b block-item))
  (loop for inx in (inputs b)
        do (unless (link inx)
             (error "Input ~a of ~a not connected" inx b)))
  (loop for parx in (params b)
        do (unless (link parx)
             (error "Param ~a of ~a not connected" parx b)))
  (loop for ptx in (ports b)
        do (check-connected ptx)))

(defmethod check-connected ((p t))
  (error "Connectivity error with ~a" p))

(defmethod check-connected :after ((pm patch-mixin))
  (loop for b in (block-items pm)
        do (check-connected b)))


(defmethod finalize-block ((pm patch))
  (loop for b in (block-items pm) do (finalize-block b)))

(defmethod finalize-block ((pm patch-mixin))
  (unless (finalized pm) (error "Block ~a not finalized" pm))
  (let ((*current-patch* pm))
    (declare (special *current-patch*))
    (loop for b in (block-items pm) do (finalize-block b))))

(defmethod finalize-block ((b basic-block))
  (unless (finalized b) (error "Block ~a not finalized" b)))


(defmethod finalize-types ((pm patch-mixin))
  (loop for b in (lin-schedule pm) do (finalize-types b))
  (loop for b in (lin-schedule pm) do (check-types b)))

(defmethod finalize-types ((b basic-block))
  (unless (typed b) (set-type b (get-type b))))

(defmethod check-types ((b basic-block)) t)

(defmethod check-domains ((b basic-block)) t)

(defmethod check-domains ((pm patch-mixin))
  (loop for b in (block-items pm) do (check-domains b)))


(defparameter *typeless* nil)

(defmethod set-type ((b basic-block) type)
  (loop for o in (outputs b)
        do (unless (datatype o)
             (setf (datatype o) type)))
  (setf (typed b) type))

(defmethod get-type ((b basic-block) &aux ips)
  (when (type-model b)
    (return-from get-type (datatype (fwd-out (out (type-model b))))))
  (when (out-type b) (return-from get-type (out-type b)))
  (when (member b *typeless*)
    (return-from get-type (default-type b)))
  (let ((*typeless* (cons b *typeless*)))
    (loop for inx in (inputs b) do (push (get-type inx) ips))
    (loop for prx in (params b) do (push (get-type prx) ips))
    (if (not ips) (default-type b) (<type ips))))
  
(defmethod get-type ((obj terminal))
  (let* ((pout (link obj))
         (b (host-block pout)))
    (if (datatype pout) (datatype pout)
        (get-type b))))


(defmethod finalize-sizes ((pm patch-mixin))
  (loop for b in (lin-schedule pm) do (finalize-sizes b))
  (loop for b in (lin-schedule pm) do (check-sizes b)))

(defmethod finalize-sizes ((b basic-block))
  (unless (sized b) (set-size b (get-size b))))

(defmethod check-sizes ((b basic-block)) t)

(defparameter *sizeless* nil)

(defmethod set-size ((b basic-block) size)
  (setf (sized b) size)
  (loop for o in (outputs b)
        do (unless (datasize o)
             (setf (datasize o) size))))

(defmethod get-size ((b basic-block) &aux ips)
  (when (type-model b)
    (return-from get-size (datasize (fwd-out (out (type-model b))))))
  (when (out-size b) (return-from get-size (out-size b)))
  (when (member b *sizeless*)
    (return-from get-size (default-size b)))
  (let ((*sizeless* (cons b *sizeless*)))
    (when (include-size-inputs b)
      (loop for inx in (inputs b) do (push (get-size inx) ips)))
    (when (include-size-params b)
      (loop for prx in (params b) do (push (get-size prx) ips)))
    (if (not ips) (default-size b) (<size ips))))

(defmethod include-size-inputs ((b basic-block)) t)
(defmethod include-size-params ((b basic-block)) t)
  
(defmethod get-size ((obj terminal))
  (let* ((pout (link obj))
         (b (host-block pout)))
    (if (datasize pout) (datasize pout)
        (get-size b))))


(defmethod check-dynamic ((b basic-block))
  (loop for x in (append (inputs b) (params b))
        for pout = (prev-out x)
        for hx = (host-block pout)
        do (cond ((and (not (typep b 'dynamic-block))
                       (loop for out in (outputs b)
                             do (when (links out) (return nil))
                             finally (return t)))
                  (setf (var-p b) nil))
                 ((or (check-dynamic hx)
                      (eq (var-p pout) t))
                  (setf (var-p b) t)
                  (return t)))))

(defmethod check-dynamic ((b dynamic-block))
  (setf (var-p b) t))

(defmethod get-variables ((p patch))
  (loop for b in (lin-schedule p) do (get-variables b))
  (setf (variables p) (reverse (variables p))))

(defmethod get-variables ((b fwd-block)) nil)

(defmethod get-variables ((b data-block))
  (get-outvars b))

(defmethod get-variables ((b basic-block))
  (check-dynamic b)
  (get-outvars b))

(defmethod get-outvars ((b basic-block))
  (loop for o in (append (outputs b) (variables b))
        for type = (datatype o)
        do (unless (varname o)
             (setf (varname o) (next-varname type)))
        (initialize-value o)
        (pushnew o (variables *current-patch*))))

#|
(defmethod get-outvars ((b basic-block))
  (loop for o in (append (outputs b) (variables b))
        for type = (datatype o)
        do (when (links o)
             (unless (varname o)
               (setf (varname o) (next-varname type)))
             (initialize-value o)
             (pushnew o (variables *current-patch*)))))
|#

(defmethod used-variables ((p patch) &aux vars)
  (loop for b in (lin-schedule p)
        do (unless (or (typep b 'fwd-block) ;;; (typep b '.p.) (typep b '.x.)
                       (not (var-p b)))
             (loop for inx in (inputs b)
                   for outx = (prev-out inx)
                   do (pushnew outx vars))
             (loop for par in (params b)
                   for outx = (prev-out par)
                   do (pushnew outx vars))
             (loop for outx in (outputs b)
                   do (pushnew outx vars))
             (loop for varx in (variables b)
                   do (pushnew varx vars))))
  (reverse vars))

#|
(defmethod used-variables ((p patch) &aux vars)
  (loop for b in (lin-schedule p)
        do (unless (or (typep b 'fwd-block) ;;; (typep b '.p.) (typep b '.x.)
                       (not (var-p b)))
             (loop for inx in (inputs b)
                   for outx = (prev-out inx)
                   do (pushnew outx vars))
             (loop for par in (params b)
                   for outx = (prev-out par)
                   do (pushnew outx vars))
             (loop for outx in (outputs b)
                   do (when (links outx)
                        (pushnew outx vars)))
             (loop for varx in (variables b)
                   do (pushnew varx vars))))
  (reverse vars))
|#

(defmethod initialize-value ((obj blockvar))
  (let* ((val (lvar obj))
         (typ (datatype obj))
         (siz (datasize obj))
         (ini (out-init (host-block obj)))
         (def (default-init (host-block obj))))
  ;  (when (numberp val) (setq val (list val)))
    (cond ((arrayp val) nil)
          ((consp val)
           (let* ((tp (get typ 'ltype))
                  (v (map.type val tp))
                  (sz (array-dimensions v)))
             (setf (lvar obj) v (datasize obj) sz)))
          (ini (setf (lvar obj) (map.init ini typ siz)))
          (def (setf (lvar obj) (map.init def typ siz))))))

(defmethod init-values ((p patch))
  (loop for b in (lin-schedule p)
        for form = (lcode b)
        do (when (and form (first-step-p b))
             (eval form))))

(defmethod step-form ((p patch) &aux forms)
  (loop for b in (lin-schedule p)
        for form = (when (var-p b) (lcode b))
        do (when form (push form forms)))
  (cons 'progn (reverse forms)))

(defmethod make-lstepfun ((p patch))
  (let* ((form `#'(lambda () ,(step-form p) nil)))
    (setf (lstepfun p) (eval form))))


;;; SCHEDULING

(defparameter *schedule-result* nil)
(defparameter *schedule-linear* nil)
(defparameter *schedule-reporting* nil)
(defparameter *schedule-progress* nil)
(defparameter *scheduled* nil)
(defparameter *not-scheduled* nil)

#| ;;; USE THIS WHEN DEBUGGING
(defmethod schedule-patch ((p patch))
  (set-non-scheduled p)
  (prog* ((reportp nil)
          (*not-scheduled* nil)
          (*scheduled* nil)
          (*schedule-progress* nil)
          (loops 0) (len 0))
    (collect-non-scheduled p)
    (setq len (length *not-scheduled*))
    tag
    (setq *schedule-progress* nil)
    (schedule p) (incf loops)
    (when (and (not *schedule-progress*) *not-scheduled*)
      (error "Cannot schedule ~a" *not-scheduled*))
    (when *schedule-progress* (go tag))
    (when reportp
      (format t "Scheduling: ~a loops; ~a blocks"
              loops (* loops len)))
    (setf (lin-schedule p) (reverse *scheduled*)))
  p)

(defmethod used-schedule ((p patch) &optional all &aux ops)
  (loop for b in (lin-schedule p)
        for varp = (var-p b)
        do (when (and (not (typep b 'fwd-block))
                      (or varp all))
             (push b ops)))
  (reverse ops))

(defmethod report-schedule ((p patch) &key
                               (stream t) all)
  (let* ((ops (used-schedule p all)))
    (loop for op in ops
          for varp = (case (var-p op)
                       (:undefined "?")
                       (nil "-") (t "+"))
          do (format stream "~%~a~a ~30t( " op varp)
          do (loop for o in (outputs op)
                   for varpo = (case (var-p o)
                                 (:undefined "?")
                                 (nil "-") (t "+"))
                   do (format stream "~a~a "
                              (varname o) varpo)
                   finally (format stream ")"))
          do (format stream "~50t( ")
          do (loop for i in (inputs op)
                   for pout = (prev-out i)
                   for vpout = (case (var-p pout)
                                 (:undefined "?")
                                 (nil "-") (t "+"))
                   do (format stream "~a~a "
                              (varname pout) vpout)
                   finally (format stream ")"))
          do (format stream "~70t( ")
          do (loop for i in (params op)
                   for pout = (prev-out i)
                   for vpout = (case (var-p pout)
                                 (:undefined "?")
                                 (nil "-") (t "+"))
                   do (format stream "~a~a "
                              (varname pout) vpout)
                   finally (format stream ")~90t"))
          do (loop with b = (host-patch op)
                   while (host-patch b)
                   do (format t "~a | " b)
                   do (setq b (host-patch b))))))


(defmethod report-toplevel ((b basic-block) &key (stream t))
  (format stream "~%~%~a" b))

(defmethod report-toplevel ((p patch) &key (stream t))
  (format stream "~%~%TOPLEVEL BLOCK REPORT OF ~a" p)
  (loop for b in (reverse (block-items p))
        do (report-toplevel b :stream stream)))
|#

(defmethod set-non-scheduled ((block basic-block))
  (set-scheduled block nil))

(defun schedule-patch (p)
  (set-non-scheduled p)
  (prog* ((*not-scheduled* nil)
          (*scheduled* nil)
          (*schedule-progress* nil))
    (collect-non-scheduled p)
    tag
    (setq *schedule-progress* nil)
    (schedule p)
    (when (and (not *schedule-progress*) *not-scheduled*)
      (error "Cannot schedule ~a" *not-scheduled*))
    (when *schedule-progress* (go tag))
    (setf (lin-schedule p) (reverse *scheduled*)))
  p)

(defmethod set-scheduled ((block basic-block) &optional (value t))
  (setf (scheduled block) value)
  (dolist (out (outputs block)) (setf (scheduled out) value)))

(defmethod collect-non-scheduled ((p patch-mixin))
  (loop for b in (block-items p) do (collect-non-scheduled b)))

(defmethod collect-non-scheduled ((p t))
  (push p *not-scheduled*))

(defmethod schedule ((p patch-mixin))
  (loop for p in (block-items p) do (schedule p)))

(defmethod schedule ((block basic-block))
  (declare (special *schedule-progress*
                    *scheduled* *not-scheduled*))
  (cond ((eq (ready-p block) :done) nil)
        ((ready-p block)
         (pushnew block *scheduled*)
         (setq *not-scheduled*
               (remove block *not-scheduled*))
         (set-scheduled block :done)
         (setq *schedule-progress* t))
        (t nil)))

(defmethod next-ins ((port output) &aux res)
  (loop for inx in (links port)
        do (setq res (append res (next-ins inx))))
  res)
         
(defmethod next-ins ((port input))
  (let* ((host (host-block port)))
    (cond ((typep host '.x.)
           (next-ins (out host)))
          (t (list port)))))

(defmethod next-ins ((port param))
  (let* ((host (host-block port)))
    (cond ((typep host '.p.)
           (next-ins (out host)))
          (t (list port)))))

#|
(defmethod ready-p ((block basic-block))
  (cond ((eq (scheduled block) :done) :done)
        (t (loop with allin = (append (inputs block) (params block))
                 for inx in allin
                 for outx = (link inx)
                 do (when (and outx (not (scheduled outx)))
                      (return-from ready-p nil)))
           (loop for outx in (outputs block)
                 do (loop for inx in (links outx)
                          for host = (host-block inx)
                          do (when (and inx (typep host 'delayed-block)
                                        (member inx (inputs host))
                                        (not (scheduled host)))
                               (return-from ready-p nil))))
           t)))
|#

(defmethod ready-p ((block basic-block))
  (cond ((eq (scheduled block) :done) :done)
        (t (loop with allin = (append (inputs block) (params block))
                 for inx in allin
                 for outx = (prev-out inx)
                 do (when (and outx (not (scheduled outx)))
                      (return-from ready-p nil)))
           (loop for outx in (outputs block)
                 do (loop for inx in (next-ins outx)
                          for host = (host-block inx)
                          do (when (and inx (typep host 'delayed-block)
                                        (member inx (inputs host))
                                        (not (scheduled host)))
                               (return-from ready-p nil))))
           t)))

#|
(defmethod ready-p ((block delayed-block))
  (cond ((eq (scheduled block) :done) :done)
        (t (loop for outx in (outputs block)
                 do (loop for inx in (links outx)
                          do (when inx 
                               (let ((host (host-block inx)))
                                 (when (and host (typep host 'delayed-block)
                                            (not (scheduled (out host))))
                                   (return-from ready-p nil))))))
           (loop for inx in (params block)
                 for outx = (link inx)
                 do (when (and outx (not (scheduled outx)))
                      (return-from ready-p nil)))
          t)))
|#

(defmethod ready-p ((block delayed-block))
  (cond ((eq (scheduled block) :done) :done)
        (t (loop for outx in (outputs block)
                 do (loop for inx in (next-ins outx)
                          do (when inx 
                               (let ((host (host-block inx)))
                                 (when (and host (typep host 'delayed-block)
                                            (not (scheduled (out host))))
                                   (return-from ready-p nil))))))
           (loop for inx in (params block)
                 for outx = (prev-out inx)
                 do (when (and outx (not (scheduled outx)))
                      (return-from ready-p nil)))
          t)))



;;; VARIABLES & CONSTANTS

(defclass .data (data-block) ())

(defmethod report-toplevel :after ((b .data) &key (stream t))
  (let* ((val (lvar (prev-out (out b)))))
    (format stream ",  VALUE = ~a" val)
    (format stream "~%TO: ")
    (loop for inx in (links (out b))
          for thost = (top-host-block inx)
          do (format stream " ~a" thost))))

(defclass .vardata (.data) ())

(defmethod initialize-instance :after ((b .vardata) &key valx namx)
  (when namx (setf (varname (out b)) (string namx)))
  (let* ((dtype (if (out-type b) (out-type b) *default-type*))
         (ltype (get dtype 'ltype)))
    (multiple-value-bind (val siz) (map.type valx ltype)
      (setf (lvar (out b)) val
            (out-size b) siz
            (typed b) dtype
            (var-p (out b)) (var-p b)
            (creader (out b)) (var-p b)
            (cwriter (out b)) (var-p b)
            (datatype (out b)) dtype
            (datasize (out b)) siz))))

(defclass .var (.vardata) ()
  (:default-initargs :var-p t))

(defun .var (value &rest rest)
  (declare (dynamic-extent rest))
  (if (or (not rest) (keywordp (car rest)))
    (apply #'make-instance '.var :valx value rest)
    (apply #'make-instance '.var
           :valx value :namx (car rest) (cdr rest))))
    
(defclass .const (.vardata) ()
  (:default-initargs :var-p nil))

(defmethod print-object ((b .const) (stream t))
  (let* ((name (name b))
         (value (value (out b)))) ;; (lvar (out b))))
    (print-unreadable-object (b stream :type t :identity t)
      (when name (format stream "\"~a\"" name))
      (format stream " ~a" value))
    b))

(defun .const (value &rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.const :valx value rest))

(defclass .trig (.var) ()
  (:default-initargs 
    :var-p t :out-type '.short))

(defun .trig (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.trig :valx 0 rest))


#|
(defparameter vx (.var 1.0 :out-type '.long))
(defparameter vx (.var 1.0 "in"))
(defparameter vx (.var 1.0))
(inspect vx)

(defparameter cx (.const 1.0))
(inspect cx)
|#

(defun .val (value &rest rest)
  (declare (dynamic-extent rest))
  (cond ((typep value 'basic-block) value)
        ((eq value t) nil)
        (t (apply #'.const value rest))))


(defun next-varname (type)
  (let* ((typenumvar (get type 'typenum))
         (num (symbol-value typenumvar))
         (ctype (string (get type 'ctype)))
         (cstype (subseq ctype 0 1))
         (snam (format nil "~a_~a" cstype num))
         (name (read-from-string snam)))
    (incf (symbol-value typenumvar))
    name))


(defmacro def.type (type ctype ltype)
  (let* ((symtype (read-from-string type))
         (str1 (subseq type 1))
         (typenum (read-from-string
                   (concatenate 'string "*" str1 "-num*"))))
    `(progn
       (setf (get ',symtype 'ctype) ',ctype)
       (setf (get ',symtype 'ltype) ',ltype)
       (setf (get ',symtype 'typenum) ',typenum)
       (defparameter ,typenum 10)
       (defun ,symtype (value &rest rest)
         (multiple-value-bind (val siz b) (map.type value ',ltype)
           (setq b (apply #'make-instance '.data :value val
                          :default-init value
                          :default-type ',symtype rest))
           (setf (out-size b) siz (sized b) siz (typed b) ',symtype)
           (setf (datatype (out b)) ',symtype (datasize (out b)) siz)
           (setf (var-p b) nil (var-p (out b)) nil)
           b)))))


(def.type ".char" "char" standard-char)
(def.type ".short" "short" fixnum)
(def.type ".long" "long" fixnum)
(def.type ".float" "float" short-float)
(def.type ".double" "double" double-float)
; (def.type ".cfloat" "complex float" '(complex cl:float))
; (def.type ".dfloat" "complex double" '(complex cl:float))

(defparameter type-order
  '(.char .short .long .float .double))

(defun <type (types)
  (flet ((pos (x) (position x type-order)))
    (let* ((p (apply #'max (mapcar #'pos types))))
      (elt type-order p))))

(defun <size (sizes &aux (siz '(1)) (len 1))
  (loop for s in sizes
        for l = (length s)
        do (cond ((> l len) (setq len l) (setq siz s))
                 ((= l len) (setq siz (mapcar #'max s siz)))))
  siz)



(provide :BC-basics)
