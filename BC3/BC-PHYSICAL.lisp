
(in-package :BC)


(defparameter *domain* nil)

(defparameter *rho* 1.3d0)
(defparameter *c* 340.0d0)


;;; Physical modeling support

(defclass port-probe (macro-block)
  ((port-link :initarg :port-link :accessor port-link)
   (block-link :initarg :block-link :accessor block-link)
   (domain :initarg :domain :accessor domain))
  (:default-initargs
    :port-link nil
    :block-link nil
    :domain nil))

(defmethod check-domains ((b port-probe))
  (let* ((port (port-link b)))
    (unless (domain port)
      (check-domains (host-patch (host-block port))))
    (when (domain b)
      (unless (eq (domain port) (domain b))
        (error "Domain incompatibility of ~a" b)))))


;;; PORTS

(defclass physical-port (terminal)
  ((link :initarg :link :accessor link)
   (zref :initarg :zref :accessor zref)
   (yref :initarg :yref :accessor yref)
   (domain :initarg :domain :accessor domain)
   (input :initarg :input)
   (output :initarg :output)
   (through :initform nil :accessor through)
   (across :initform nil :accessor across)
   (power :initform nil :accessor power)
   (energy :initform nil :accessor energy)
   (through-integ :initform nil :accessor through-integ)
   (through-diff :initform nil :accessor through-diff))
  (:default-initargs
    :input (make-instance 'input)
    :output (make-instance 'output)
    :zref nil
    :yref nil
    :domain nil
    :link nil))

(defmethod port ((port physical-port) &optional (index 0) provide-p)
  (declare (ignore provide-p index))
  port)

(defmethod check-connected ((p physical-port))
  (unless (link p)
    (error "Port ~a not connected" p)))

(defmethod var-p ((p physical-port)) ;;;                              ?????
  (let* ((host (host-block (fwd-out (zref p)))))
    (if (typep host '.data) (var-p host) t)))

(defmethod print-object ((port physical-port) (stream t))
  (let* ((name (name port))
         (host (top-host-block port))
         (index (when host (position port (ports host)))))
    (print-unreadable-object (port stream :type t :identity t)
      (when index (format stream "~a" index))
      (when name (format stream "'~a'" name))
      (when host (format stream " in <~a>" (type-of host))))))


(defmethod input ((p physical-port) &optional (index 0) provide-p)
  (declare (ignore provide-p))
  (unless (= index 0)
    (error "Physical-port has only one input"))
  (slot-value p 'input))


(defmethod output ((p physical-port) &optional (index 0) error-p)
  (declare (ignore error-p))
  (unless (= index 0)
    (error "Physical-port has only one output"))
  (slot-value p 'output))


(defmethod in ((port physical-port) &optional (index 0) provide-p)
  (input port index provide-p))

(defmethod out ((port physical-port) &optional (index 0))
  (output port index))


(defclass w-port (physical-port) ()) ;;; wave-port


(defclass wa-port (w-port) ()) ;;; reflection-free (adapted) wave-port  


(defmethod connect ((p1 wa-port) (p2 w-port))
  (setf (link p1) p2 (link p2) p1)
  (connect (output p1) (input p2))
  (connect (output p2) (input p1))
  (connect (zref p1) (zref p2))
  (connect (yref p1) (yref p2))
  (setf (domain p2) (domain p1)
        (through p2) (through p1)
        (across p2) (across p1))
  nil)

(defmethod connect ((p1 w-port) (p2 wa-port))
  (connect p2 p1))

(defmethod connect-p ((p1 w-port) (p2 wa-port)) t)
(defmethod connect-p ((p2 wa-port) (p1 w-port)) t)
(defmethod connect-p ((p1 t) (p2 t)) nil)


(defclass k-port (physical-port) ()) ;;; Kirchhoff-port ????


;;; (defclass fdtd-port (k-port) ()) ;;; FDTD-port ???


;;; Wave port objects

(defclass .w-port-block (macro-block) ())

(defmethod initialize-macro-block ((b .w-port-block) &key
                                       (params-p t)
                                       (port-type 'w-port))
  (let* ((sin (.x. :name 'sigin))
         (sout (.x. :name 'sigout))
         (port (make-instance port-type :host-block b
                              :input nil :output nil)))
    (setf (zref port) (when params-p (param (.p.)))
          (yref port) (when params-p (param (.p.)))
          (slot-value port 'input) (in sin)
          (slot-value port 'output) (out sout)
          (ports b) (list port)
          (inputs b) (list (in sout))
          (outputs b) (list (out sin)))))


(defclass .wa-port-block (.w-port-block) ())

(defmethod initialize-macro-block ((b .wa-port-block) &key
                                       (port-type 'wa-port))
  (let* ((sin (.x. :name 'sigin))
         (sout (.x. :name 'sigout))
         (pz (.p. :name 'z))
         (py (.p. :name 'y))
         (port (make-instance port-type :host-block b)))
    (setf (zref port) (out pz)
          (yref port) (out py)
          (slot-value port 'input) (in sin)
          (slot-value port 'output) (out sout)
          (ports b) (list port)
          (params b) (list (param pz) (param py))
          (inputs b) (list (in sout))
          (outputs b) (list (out sin))))
  nil)


;;; Physical modeling elements (by port-count)

(defclass physical-block (block-item)
  ((ports :initarg :ports :accessor ports))
  (:default-initargs
    :ports nil))

(defmethod finalize-immittances ((block physical-block)) nil)

(defclass one-port-block (physical-block)
  ((domain :initarg :domain :accessor domain))
  (:default-initargs
    :domain *domain*))


(defclass two-port-block (physical-block)
  ((domain :initarg :domain :accessor domain))
  (:default-initargs
    :domain *domain*))

(defmethod check-domains ((b two-port-block))
  (unless (domain b)
    (error "No domain specified for ~a" b))
  (unless  (domain (port b))
    (error "No domain specified for ~a" (port b))))

(defclass three-port-block (two-port-block) ())

(defclass n-port-block (two-port-block) ())


;;; TWO-PORT LINK (No computation)

(defclass .z. (macro-block two-port-block) ()
  (:default-initargs
    :domain 'electric
    :out-type *default-type*))

(defmethod initialize-macro-block ((b .z.) &key)
  (let* ((pb0 (make-instance '.w-port-block))
         (p0 (port pb0))
         (pb1 (make-instance '.wa-port-block))
         (p1 (port pb1)))
    (setf (domain p0) (domain b)
          (domain p1) (domain b)
          (ports b) (list p0 p1))
    (-> (out pb0) (in pb1))
    (-> (out pb1) (in pb0))
    (-> (host-block (zref p0))
        (param (host-block (zref p1)) 0))
    (-> (host-block (yref p0))
        (param (host-block (yref p1)) 0))))

(defun .z. (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.z. rest))


;;; ONE-PORT COMPONENTS

(defclass one-port-macro (macro-block one-port-block)
  ((out-type :initarg :out-type :accessor out-type))
  (:default-initargs
    :out-type *default-type*
    :finalized nil
    :dummy nil))

(defmethod initialize-macro-block ((b one-port-macro) &key 
                                       value port-name
                                       (port-block-type '.wa-port-block)
                                       (port-type 'wa-port))
  (setq value (make-param b value)) ;;; otherwise constant
  (let* ((pb (make-instance port-block-type
               :port-type port-type))
         (p (port pb)))
    (setf (domain p) (domain b)
          (ports b) (list p)
          (name p) port-name)
    (process-immittance b pb value)))

(defmethod check-domains ((b one-port-macro))
  (unless (domain b)
    (error "No domain specified for ~a" b))
  (unless  (domain (port b))
    (error "No domain specified for ~a" (port b))))


(defmethod finalize-block ((b one-port-macro))
  (unless (finalized b)
    (setf (finalized b) t)))

(defmethod report-toplevel :after ((b one-port-macro) &key (stream t))
  (report-toplevel-extra b :stream stream)
  (report-toplevel (port b) :stream stream))

(defmethod report-toplevel-extra ((b one-port-macro) &key (stream t))
  stream nil)

(defmethod report-toplevel ((port w-port) &key (stream t))
  (let* ((out (lvar (prev-out (output port))))
         (in (lvar (prev-out (input port))))
         (z (lvar (prev-out (zref port))))
         (y (lvar (prev-out (yref port))))
         (ac (lvar (prev-out (across port))))
         (th (lvar (prev-out (through port))))
         )
    (format stream "~%PORT-IN = ~a" in)
    (format stream "  PORT-OUT = ~a" out)
    (format stream "~%ACROSS = ~a" ac)
    (format stream "  THROUGH = ~a" th)
    (format stream "~%ZREF = ~a" z)
    (format stream "  YREF = ~a" y)))


;;; Physical signal probes (generic versions)

(defmethod .across ((b one-port-macro) &rest rest)
  (apply #'.across (port b) rest))

(defmethod .through ((b one-port-macro) &rest rest)
  (apply #'.through (port b) rest))

(defmethod .voltage ((b one-port-block) &rest rest)
  (apply #'.voltage (port b) rest))

(defmethod .current ((b one-port-block) &rest rest)
  (apply #'.current (port b) rest))

(defmethod .force ((b one-port-block) &rest rest)
  (apply #'.force (port b) rest))

(defmethod .velocity ((b one-port-block) &rest rest)
  (apply #'.velocity (port b) rest))

(defmethod .pressure ((b one-port-block) &rest rest)
  (apply #'.pressure (port b) rest))

(defmethod .flow ((b one-port-block) &rest rest)
  (apply #'.flow (port b) rest))

(defmethod .power ((b one-port-block) &rest rest)
  (apply #'.power (port b) rest))

(defmethod .energy ((b one-port-block) &rest rest)
  (apply #'.energy (port b) rest))


;;; Z&Y parameter control for R & G

(defclass .zy-control (macro-group) ())

(defmethod initialize-macro-block ((b .zy-control) &key
                                       (z-p t))
  (let* ((p (.p.)) (inv (.inv)))
    (cond (z-p (setf (outputs b) (list (out p) (out inv))))
          (t (setf (outputs b) (list (out inv) (out p)))))
    (setf (params b) (list (param p)))
    (-> p inv)))


;;; WDF Resistance (Voltage/Current)

(defclass .R (one-port-macro) ()
  (:default-initargs
    :domain 'electric))

(defmethod process-immittance ((b .R) pb value)
  (let* ((pc (make-instance '.zy-control)))
    (-> value (param pc 0))
    (-> (out pc 0) (param pb 0))
    (-> (out pc 1) (param pb 1))
    (-> value (.datac 0.0d0) pb)))

(defun .R (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.R :value (car rest) (cdr rest))
    (apply #'make-instance '.R :value nil rest)))

;;; (.r &keys) / (.r R-param &keys)


;;; WDF Conductance (Current/Voltage)

(defclass .G (one-port-macro) ()
  (:default-initargs
    :domain 'electric))

(defmethod process-immittance ((b .G) pb value)
  (let* ((pc (make-instance '.zy-control :z-p nil)))
    (-> value (param pc 0))
    (-> (out pc 0) (param pb 0))
    (-> (out pc 1) (param pb 1))
    (-> value (.datac 0.0d0) pb)))

(defun .G (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.G :value (car rest) (cdr rest))
    (apply #'make-instance '.G :value nil rest)))


;;; Damper = mechanical resistance (Force/Velocity)

(defclass .Rm (.R) ()
  (:default-initargs
    :domain 'mechanic))

(defun .Rm (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.Rm :value (car rest) (cdr rest))
    (apply #'make-instance '.Rm :value nil rest)))

(defun .damper (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'.Rm rest))

;;; (inspect (.Rm 1.0))
;;; (inspect (.damper 1.0))

;;; Mechanical admittance (Velocity/Force)

(defclass .Gm (.G) ()
  (:default-initargs
    :domain 'mechanic))

(defun .Gm (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.Gm :value (car rest) (cdr rest))
    (apply #'make-instance '.Gm :value nil rest)))


;;; Acoustical resistance (Pressure/VolumeVelocity)

(defclass .Ra (.R) ()
  (:default-initargs
    :domain 'acoustic))

(defun .Ra (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.Ra :value (car rest) (cdr rest))
    (apply #'make-instance '.Ra :value nil rest)))

;;; (inspect (.Ra 1.0))


;;; Acoustical admittance (VolumeVelocity/Pressure)

(defclass .Ga (.G) ()
  (:default-initargs
    :domain 'acoustic))

(defun .Ga (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.Ga :value (car rest) (cdr rest))
    (apply #'make-instance '.Ga :value nil rest)))


;;; WDF Voltage source

(defclass .E (one-port-macro) ()
  (:default-initargs
    :domain 'electric))

(defmethod process-immittance ((block .E) pb value)
  (let* ((x (.x.))
         (y (.inv value))
         (c (.coeff 0.5d0)))
    (setf (inputs block) (list (in x)))
    (when (params block) (setf (name (param block 0)) 'R))
    (setf (name (in x)) 'E)
    (-> x c pb)
    (-> value (param pb 0))
    (-> y (param pb 1))))

(defmethod report-toplevel-extra ((b .E) &key (stream t))
  (let* ((val (lvar (prev-out (in b)))))
    (format stream ", E = ~a" val)))

(defun .E (&rest rest &aux val r b)
  (declare (dynamic-extent rest))
  (cond ((null rest))
        ((not (keywordp (car rest)))
         (setq val (.val (car rest)))
         (setq rest (cdr rest))
         (when (and rest (not (keywordp (car rest))))
           (setq r (car rest))
           (setq rest (cdr rest)))))
  (setq b (apply #'make-instance '.E :value r rest))
  (when val (-> val (in b)))
  b)

;;; (.E &keys) / (.E insig &keys) / (.E insig R-param &keys)
;;; (inspect (.E))


;;; WDF Fource source

(defclass .Fm (.E) ()
  (:default-initargs
    :domain 'mechanic))

(defmethod report-toplevel-extra ((b .Fm) &key (stream t))
  (let* ((val (lvar (prev-out (in b)))))
    (format stream ", F = ~a" val)))

(defun .Fm (&rest rest &aux val r b)
  (declare (dynamic-extent rest))
  (cond ((null rest))
        ((not (keywordp (car rest)))
         (setq val (.val (car rest)))
         (setq rest (cdr rest))
         (when (and rest (not (keywordp (car rest))))
           (setq r (car rest))
           (setq rest (cdr rest)))))
  (setq b (apply #'make-instance '.Fm :value r rest))
  (when val (-> val (in b)))
  b)


;;; WDF Pressure source

(defclass .Pa (.E) ()
  (:default-initargs
    :domain 'acoustic))

(defmethod report-toplevel-extra ((b .Pa) &key (stream t))
  (let* ((val (lvar (prev-out (in b)))))
    (format stream ", P = ~a" val)))

(defun .Pa (&rest rest &aux val r b)
  (declare (dynamic-extent rest))
  (cond ((null rest))
        ((not (keywordp (car rest)))
         (setq val (.val (car rest)))
         (setq rest (cdr rest))
         (when (and rest (not (keywordp (car rest))))
           (setq r (car rest))
           (setq rest (cdr rest)))))
  (setq b (apply #'make-instance '.Pa :value r rest))
  (when val (-> val (in b)))
  b)


;;; WDF Current source

(defclass .J (one-port-macro) ()
  (:default-initargs
    :domain 'electric))

(defmethod process-immittance ((block .J) pb value)
  (let* ((x (.x.))
         (y (.inv value))
         (c (.coeff 0.5d0))
         (mul (.mul)))
    (setf (inputs block) (list (in x)))
    (when (params block) (setf (name (param block 0)) 'R))
    (setf (name (in x)) 'J)
    (-> value (in mul 1))
    (-> x c mul pb)
    (-> value (param pb 0))
    (-> y (param pb 1))))

(defmethod report-toplevel-extra ((b .J) &key (stream t))
  (let* ((val (lvar (prev-out (in b)))))
    (format stream ", J = ~a" val)))

(defun .J (&rest rest &aux val r b)
  (declare (dynamic-extent rest))
  (cond ((null rest))
        ((not (keywordp (car rest)))
         (setq val (.val (car rest)))
         (setq rest (cdr rest))
         (when (and rest (not (keywordp (car rest))))
           (setq r (car rest))
           (setq rest (cdr rest)))))
  (setq b (apply #'make-instance '.J :value r rest))
  (when val (-> val (in b)))
  b)


;;; Z&Y parameter control for C

(defclass .zycl-control (macro-group) ())

(defmethod initialize-macro-block ((b .zycl-control) &key
                                       (c-p t))
  (let* ((p (.p.))
         (srate (srate b))
         (sr2/ (.datac (/ 0.5d0 srate)))
         (sr2 (.datac (* 2.0d0 srate)))
         (z (if c-p (.div sr2/ p) (.mul sr2 p)))
         (y (if c-p (.mul sr2 p) (.div sr2/ p))))
    (-> p sr2/) (-> p sr2)
    (setf (params b) (list (param p))
          (outputs b) (list (out z) (out y)))))


;;; Capacitance (I = C dU/dt)

(defclass .C (one-port-macro) ()
  (:default-initargs
    :domain 'electric))

(defmethod initialize-macro-block :after ((b .C) &key) ;;; INITIALIZE ACROSS !!!!
  (let* ((pb (host-block (port b)))
         (d (.d)))
    (when (params b) (setf (name (param b)) 'C))
    (-> (out pb) d (in pb))))

(defmethod process-immittance ((b .C) pb value)
  (let* ((pc (make-instance '.zycl-control)))
    (-> value (param pc 0))
    (-> (out pc 0) (param pb 0))
    (-> (out pc 1) (param pb 1))))

(defun .C (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.C :value (car rest) (cdr rest))
    (apply #'make-instance '.C :value nil rest)))

;;; (.C &keys) / (.C C-param &keys)


;;; Mechanical capacitance (V = Cm dF/dt)

(defclass .Cm (.C) ()
  (:default-initargs
    :domain 'mechanic))

(defun .Cm (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.Cm :value (car rest) (cdr rest))
    (apply #'make-instance '.Cm :value nil rest)))

;;; Mechanical compliance

(defun .compliance (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.Cm :value (car rest) (cdr rest))
    (apply #'make-instance '.Cm :value nil rest)))

;;; Mechanical spring of constant spring coefficient (k = coeff) -> .Cm

(defun .spring (coeff &rest rest)
  (declare (dynamic-extent rest))
  (let* ((k (/ 1.0d0 coeff)))
    (apply #'make-instance '.Cm :value k rest)))


;;; Acoustical capacitance (Va = Ca dP/dt)

(defclass .Ca (.C) ()
  (:default-initargs
    :domain 'acoustic))

(defun .Ca (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.Ca :value (car rest) (cdr rest))
    (apply #'make-instance '.Ca :value nil rest)))

;;; Cavity (constant parameters)

(defclass .cavity (macro-block)
  ((volume :initarg :volume :reader volume)
   (rho :initarg :rho :reader rho)
   (c :initarg :c :reader c))
  (:default-initargs
    :rho *rho*
    :c *c*))

(defmethod initialize-macro-block ((b .cavity) &key)
  (let* ((c (c b))
         (k (/ (volume b) (rho b) (* c c)))
         (ca (.ca k)))
    (setf (ports b) (list (port ca)))))

(defun .cavity (volume &rest rest)
  (declare (dynamic-extent rest))
    (apply #'make-instance '.cavity 
           :volume volume rest))
  

;;; Inductor (U = L dI/dt)

(defclass .L (one-port-macro) ()
  (:default-initargs
    :domain 'electric))

(defmethod initialize-macro-block :after ((b .L) &key) ;;; INITIALIZE THROUGH !!!!
  (let* ((pb (host-block (port b)))
         (d (.d-)))
    (when (params b) (setf (name (param b)) 'L))
    (-> (out pb) d (in pb))))

(defmethod process-immittance ((b .L) pb value)
  (let* ((pc (make-instance '.zycl-control :c-p nil)))
    (-> value (param pc 0))
    (-> (out pc 0) (param pb 0))
    (-> (out pc 1) (param pb 1))))

(defun .L (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.L :value (car rest) (cdr rest))
    (apply #'make-instance '.L :value nil rest)))

;;; (.L &keys) / (.L L-param &keys)


;;; Mechanical inductor (F = Lm dV/dt)

(defclass .Lm (.L) ()
  (:default-initargs
    :domain 'mechanic))

(defun .Lm (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.Lm :value (car rest) (cdr rest))
    (apply #'make-instance '.Lm :value nil rest)))

;;; Mechanical mass (like inductor)

(defclass .mass (.Lm) ())

(defun .mass (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.mass :value (car rest) (cdr rest))
    (apply #'make-instance '.mass :value nil rest)))

; (inspect (.mass 1.0))

;;; Acoustical inductor (P = La dVa/dt)

(defclass .La (.L) ()
  (:default-initargs
    :domain 'acoustic))

(defun .La (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.La :value (car rest) (cdr rest))
    (apply #'make-instance '.La :value nil rest)))


;;; ------------

;;; ROOT BLOCK

(defclass root-block (one-port-macro) ()
  (:default-initargs
    :domain 'electric))


;;; Diode

(defclass .diode (root-block)
  ((direction :initarg :direction :accessor direction))
  (:default-initargs
    :direction '+))

(defmethod initialize-macro-block ((b .diode) &key
                                       port-name)
  (let* ((pb (make-instance '.w-port-block :port-type 'w-port))
         (p (port pb)))
    (cond ((eq (direction b) '+)
           (-> (output pb) (.abs) (.neg) (input pb)))
          ((eq (direction b) '-)
           (-> (output pb) (.abs) (input pb))))
    (setf (domain p) (domain b))
    (setf (ports b) (list p)
          (name b) port-name)))

(defun .diode (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.diode rest))


;;; EXTERMAL (MATLAB / C) BLOCK

(defclass .ext-root (root-block)
  ((mfun :initarg :mfun :accessor mfun)
   (cfun :initarg :cfun :accessor cfun)
   (bfun :initarg :bfun :accessor bfun))
  (:default-initargs
    :domain 'electric
    :mfun nil
    :cfun nil
    :bfun '.extfun))

(defmethod initialize-macro-block ((b .ext-root) &key
                                       port-name)
  (let* ((pb (make-instance '.w-port-block :port-type 'w-port))
         (p (port pb))
         (f (funcall (bfun b) :mfun (mfun b) :cfun (cfun b))))
    (setf (bfun b) f)
    (-> (output pb) f (input pb))
    (setf (ports b) (list p)
          (name (port b)) port-name)))

(defun .ext-root (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.ext-root rest))


(defclass .ext-root2 (.ext-root) ()
  (:default-initargs
    :bfun '.extfun2))

(defmethod initialize-macro-block :after ((b .ext-root2) &key)
  (let* ((x (.x.)))
    (-> x (in (bfun b) 1))
    (setf (inputs b) (list (in x)))))

(defun .ext-root2 (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.ext-root2 rest))

; (inspect (.ext-root :mfun "AAA"))
; (inspect (.ext-root2 :mfun "AAA"))


;;; -------


;;; ADAPTORS

(defclass w-adaptor (macro-block) ())


;;; DUMMY ADAPTOR

(defclass .a. (w-adaptor macro-block two-port-block) ()
  (:default-initargs 
    :domain 'electric
    :out-type *default-type*))

(defmethod initialize-macro-block ((b .a.) &key portname)
  (let* ((pb0 (make-instance '.w-port-block))
         (p0 (port pb0))
         (pb1 (make-instance '.wa-port-block))
         (p1 (port pb1)))
    (setf (name p0) portname)
    (setf (domain p0) (domain b)
          (domain p1) (domain b)
          (ports b) (list p0 p1))
    (-> (out pb0) (in pb1))
    (-> (out pb1) (in pb0))
    (-> (host-block (zref p0))
        (param (host-block (zref p1)) 0))
    (-> (host-block (yref p0))
        (param (host-block (yref p1)) 0))))

(defun .a. (&optional portname)
  (make-instance '.a. :portname portname))

#|
(inspect (.a.))
(inspect (.a. 'a))

(defpatch koe ((r1 (.R (.var 1.0)))
                (e (.E (.var 1.0) 1.0))
                (a (.a. 'A)))
  (.par (port e) (port a 0))
  (.par r1 (port a 1))
  (-> (.voltage e) (.probe "out")))

(inspect koe)
(c-code koe)
|#


;;; 3-PORT PARALLEL ADAPTOR

(defclass .par3-block (macro-block) ())

(defmethod initialize-macro-block ((b .par3-block) &key)
  (let* ((in0 (.x.)) (in1 (.x.)) (in2 (.x.)) (g (.p.))
         (sub1 (.sub in0 in1))
         (mul (.mul sub1 g))
         (add1 (.add in1 mul))
         (add2 (.add in2 mul))
         (sub2 (.sub add2 sub1)))
    (setf (name (in in0)) 'a0
          (name (in in1)) 'a1
          (name (in in2)) 'a2
          (name (param g)) 'gamma
          (name (out sub2)) 'b0
          (name (out add2)) 'b1
          (name (out add1)) 'b2)
    (setf (outputs b)
          (list (out sub2) (out add2) (out add1))
          (inputs b)
          (list (in in0) (in in1) (in in2))
          (params b) (list (param g)))))

(defclass .par3-adaptor (w-adaptor) ())

(defmethod initialize-macro-block ((b .par3-adaptor) &key)
  (let* ((pb0 (make-instance '.w-port-block))
         (p0 (port pb0))
         (pb1 (make-instance '.w-port-block))
         (p1 (port pb1))
         (pb2 (make-instance '.wa-port-block))
         (p2 (port pb2))
         (parb (make-instance '.par3-block))
         (add (.add)) (inv (.inv add)) (mul (.mul)))
    (setf (ports b) (list p0 p1 p2))
    (-> (host-block (yref p0)) add)
    (-> (host-block (yref p1)) (in add 1))
    (-> (host-block (yref p0)) mul)
    (-> (out add) (param (host-block (yref p2))))
    (-> (out inv) (param (host-block (zref p2))))
    (-> inv (in mul 1) (param parb))
    (-> (out parb 0) (in pb0))
    (-> (out pb0) (in parb 0))
    (-> (out parb 1) (in pb1))
    (-> (out pb1) (in parb 1))
    (-> (out parb 2) (in pb2))
    (-> (out pb2) (in parb 2))))

(defmethod check-connected ((p wa-port))
  (when (and (not (link p)) (not (link (input p))))
    (let* ((block (host-block p))
           (patch (host-patch block)))
      (cond ((typep patch '.par3-adaptor)
             (-> (output p) (input p)))
            ((typep patch '.ser3-adaptor)
             (let ((*current-patch* patch))
               (-> (output p) (.neg) (input p)))))
      (setf (link p) p))))

(defmethod report-toplevel :after ((b .par3-adaptor) &key (stream t))
  (let* ((el0 (top-host-block (link (port b 0))))
         (pn0 (position (link (port b 0)) (ports el0)))
         (el1 (top-host-block (link (port b 1))))
         (pn1 (position (link (port b 1)) (ports el1))))
    (format stream "~%LINKS: port ~a of ~a and port ~a of ~a" pn0 el0 pn1 el1)))

(defun .par3-adaptor (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.par3-adaptor rest))


;;; 2-PORT PARALLEL ADAPTOR

(defclass .par2-block (macro-block) ())

(defmethod initialize-macro-block ((b .par2-block) &key)
  (let* ((in0 (.x.)) (in1 (.x.)) (g (.p.))
         (sub1 (.sub in0 in1))
         (mul (.mul sub1 g))
         (add1 (.add in0 mul))
         (add0 (.add in1 mul)))
    (setf (name (in in0)) 'a0
          (name (in in1)) 'a1
          (name (param g)) 'gamma
          (name (out add0)) 'b0
          (name (out add1)) 'b1)
    (setf (outputs b)
          (list (out add0) (out add1))
          (inputs b)
          (list (in in0) (in in1))
          (params b) (list (param g)))))

(defclass .par2-adaptor (w-adaptor) ())

(defmethod initialize-macro-block ((b .par2-adaptor) &key)
  (let* ((pb0 (make-instance '.w-port-block))
         (p0 (port pb0))
         (pb1 (make-instance '.w-port-block))
         (p1 (port pb1))
         (parb (make-instance '.par2-block))
         (add (.add)) (sub (.sub))
         (div (.div sub add)))
    (setf (ports b) (list p0 p1))
    (-> (host-block (yref p0)) (in add 0))
    (-> (host-block (yref p1)) (in add 1))
    (-> (host-block (yref p0)) (in sub 0))
    (-> (host-block (yref p1)) (in sub 1))
    (-> div (param parb))
    (-> (out parb 0) (in pb0))
    (-> (out pb0) (in parb 0))
    (-> (out parb 1) (in pb1))
    (-> (out pb1) (in parb 1))))

(defmethod check-domains ((b .par2-adaptor))
  (let* ((p0 (port b 0)) (p1 (port b 1)))
    (unless (domain (link p0))
      (check-domains (host-patch (host-block (link p0)))))
    (setf (domain p0) (domain (link p0)))
    (unless (domain (link p1))
      (check-domains (host-patch (host-block (link p1)))))
    (setf (domain p1) (domain (link p1)))
    (unless (eq (domain p0) (domain p1))
      (error "Domain incompatibility of ~a and ~a in ~a"
             p0 p1 b))) t)

(defmethod report-toplevel :after ((b .par2-adaptor) &key (stream t))
  (let* ((el0 (top-host-block (link (port b 0))))
         (pn0 (position (link (port b 0)) (ports el0)))
         (el1 (top-host-block (link (port b 1))))
         (pn1 (position (link (port b 1)) (ports el1))))
    (format stream "~%LINKS: port ~a of ~a and port ~a of ~a" pn0 el0 pn1 el1)))

(defun .par2-adaptor (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.par2-adaptor rest))


;;; DIRECT CONNECTION OF TWO ADAPTED PORTS

(defmethod .par2a ((p1 wa-port) (p2 wa-port))
  (setf (link p1) p2 (link p2) p1)
  (-> (output p1) (input p2))
  (-> (output p2) (input p1))
  nil)
   
(defmethod .par2a ((p1 t) (p2 t))
  (error "Cannot connect ports ~a nad ~a" p1 p2)
  nil)

(defmethod .pair ((p1 wa-port) (p2 wa-port))
  (unless (eq (domain p1) (domain p2))
    (error "Domain incompatibility of ~a and ~a" p1 p2))
  (setf (link p1) p2 (link p2) p1)
  (-> (output p1) (input p2))
  (-> (output p2) (input p1))
  nil)
   
(defmethod .pair ((p1 t) (p2 t))
  (error "Cannot connect ports ~a nad ~a" p1 p2)
  nil)


;;; 3-PORT SERIES ADAPTOR

(defclass .ser3-block (macro-block) ())

(defmethod initialize-macro-block ((b .ser3-block) &key)
  (let* ((in0 (.x.)) (in1 (.x.)) (in2 (.x.)) (g (.p.))
         (add1 (.add in0 in1))
         (sub1 (.sub add1 in2))
         (mul (.mul sub1 g))
         (sub2 (.sub in0 mul))
         (sub3 (.sub in2 sub2)))
    (setf (name (in in0)) 'a0
          (name (in in1)) 'a1
          (name (in in2)) 'a2
          (name (param g)) 'gamma
          (name (out sub2)) 'b0
          (name (out sub3)) 'b1
          (name (out add1)) 'b2)
    (setf (outputs b)
          (list (out sub2) (out sub3) (out add1))
          (inputs b)
          (list (in in0) (in in1) (in in2))
          (params b) (list (param g)))))


(defclass .ser3-adaptor (w-adaptor) ())

(defmethod initialize-macro-block ((b .ser3-adaptor) &key)
  (let* ((pb0 (make-instance '.w-port-block))
         (p0 (port pb0))
         (pb1 (make-instance '.w-port-block))
         (p1 (port pb1))
         (pb2 (make-instance '.wa-port-block))
         (p2 (port pb2))
         (parb (make-instance '.ser3-block))
         (add (.add)) (inv (.inv add)) (mul (.mul)))
    (setf (ports b) (list p0 p1 p2))
    (-> (host-block (zref p0)) add)
    (-> (host-block (zref p1)) (in add 1))
    (-> (host-block (zref p0)) mul)
    (-> (out add) (param (host-block (zref p2))))
    (-> (out inv) (param (host-block (yref p2))))
    (-> inv (in mul 1) (param parb))
    (-> (out parb 0) (in pb0))
    (-> (out pb0) (in parb 0))
    (-> (out parb 1) (in pb1))
    (-> (out pb1) (in parb 1))
    (-> (out parb 2) (in pb2))
    (-> (out pb2) (in parb 2))))

(defmethod report-toplevel :after ((b .ser3-adaptor) &key (stream t))
  (let* ((el0 (top-host-block (link (port b 0))))
         (pn0 (position (link (port b 0)) (ports el0)))
         (el1 (top-host-block (link (port b 1))))
         (pn1 (position (link (port b 1)) (ports el1))))
    (format stream "~%LINKS: port ~a of ~a and port ~a of ~a" pn0 el0 pn1 el1)))

(defun .ser3-adaptor (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.ser3-adaptor rest))



;;; DELAY LINES (.dline)

(defclass dline-block (macro-block two-port-block) ())

(defmethod report-toplevel :after ((b dline-block) &key (stream t))
  (let* ((el0 (top-host-block (link (port b 0))))
         (pn0 (position (link (port b 0)) (ports el0)))
         (el1 (top-host-block (link (port b 1))))
         (pn1 (position (link (port b 1)) (ports el1))))
    (format stream "~%LINKS: port ~a of ~a and port ~a of ~a" pn0 el0 pn1 el1)
    (format stream "~%   Port 0:")
    (report-toplevel (port b 0))
    (format stream "~%   Port 1:")
    (report-toplevel (port b 1))))


;;; .DLINE-1 (UNIT DELAY LINE)

(defclass .dline-1 (dline-block) ()
  (:default-initargs
    :domain 'electric
    :out-type *default-type*))

(defmethod initialize-macro-block ((b .dline-1) &key value)
  (setq value (make-param b value))
  (let* ((pb0 (make-instance '.wa-port-block))
         (p0 (port pb0))
         (pb1 (make-instance '.wa-port-block))
         (p1 (port pb1))
         (d0 (.d)) (d1 (.d))
         (inv (.inv)))
    (setf (domain p0) (domain b) (domain p1) (domain b)
          (ports b) (list p0 p1))
    (-> (out pb0) d0 (in pb1))
    (-> (out pb1) d1 (in pb0))
    (-> value (param (host-block (zref p0)) 0))
    (-> value (param (host-block (zref p1)) 0))
    (-> value inv)
    (-> inv (param (host-block (yref p0)) 0))
    (-> inv (param (host-block (yref p1)) 0))))

(defun .dline-1 (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.dline-1 :value (car rest) (cdr rest))
    (apply #'make-instance '.dline-1 :value nil rest)))



;;; .DLINE-N (N-LENGTH DELAY LINE)

(defclass .dline-n (dline-block)
  ((state :initform nil :accessor state)
   (dlength :initarg :delay-length :accessor dlength))
  (:default-initargs 
    :domain 'electric
    :delay-length nil
    :out-type *default-type*))

(defmethod initialize-macro-block ((b .dline-n) &key
                                       value)
  (unless (and (integerp (dlength b))
               (>= (dlength b) 1))
    (error "Invalid delay-length for ~a" b))
  (cond ((or (not value) (eq value 't))
         (setq value (.p. :name 'pval)) 
         (push (param value) (params b)))
        ((typep value 'data-block) t)
        (t (setq value (funcall (out-type b) value))))
  (let* ((pb0 (make-instance '.wa-port-block))
         (p0 (port pb0))
         (pb1 (make-instance '.wa-port-block))
         (p1 (port pb1))
         (d0 (.dn (dlength b)))
         (d1 (.dn (dlength b)))
         (inv (.inv)))
    (setf (domain p0) (domain b) (domain p1) (domain b)
          (ports b) (list p0 p1))
    (-> (out pb0) d0 (in pb1))
    (-> (out pb1) d1 (in pb0))
    (-> value (param (host-block (zref p0)) 0))
    (-> value (param (host-block (zref p1)) 0))
    (-> value inv)
    (-> inv (param (host-block (yref p0)) 0))
    (-> inv (param (host-block (yref p1)) 0))))

(defun .dline-n (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.dline-n :value (car rest) (cdr rest))
    (apply #'make-instance '.dline-n :value nil rest)))


;;; .XFORMER (transformer)
;;; ----------------------
;;; Turns ratio = 1:N 
;;; in port0 -> port 1 direction
;;; Impedance x N^2
;;; voltage x N; current / N
;;; Checked 23.04.07 mk

(defclass .xf-control (macro-group) ())

(defmethod initialize-macro-block ((b .xf-control) &key)
  (let* ((p (.p.)) (pz (.p.)) (py (.p.))
         (inv (.inv p))
         (sqr> (.mul p p pz))
         (sqr< (.mul inv inv py)))
    (setf (outputs b)
          (list (out p) (out inv)
                (out sqr>) (out sqr<)))
    (setf (params b)
          (list (param p) (param pz) (param py)))))

(defclass .xformer (macro-block two-port-block) ()
  (:default-initargs 
    :domain 'electric
    :out-type *default-type*))

(defmethod initialize-macro-block ((b .xformer) &key value)
  (setq value (make-param b value))
  (let* ((pb0 (make-instance '.w-port-block))
         (p0 (port pb0))
         (pb1 (make-instance '.wa-port-block))
         (p1 (port pb1))
         (mul0 (.mul)) (mul1 (.mul))
         (xfc (make-instance '.xf-control)))
    (setf (domain p0) (domain b)
          (domain p1) (domain b)
          (ports b) (list p0 p1))
    (-> value (param xfc 0))
    (-> (out pb0) mul0 (in pb1))
    (-> (out pb1) mul1 (in pb0))
    (-> (out xfc 0) (in mul0 1))
    (-> (out xfc 1) (in mul1 1))
    (-> (host-block (zref p0)) (param xfc 1))
    (-> (out xfc 2) (param (host-block (zref p1)) 0))
    (-> (host-block (yref p0)) (param xfc 2))
    (-> (out xfc 3) (param (host-block (yref p1)) 0))))

(defmethod report-toplevel :after ((b .xformer) &key (stream t))
  (let* ((el0 (top-host-block (link (port b 0))))
         (pn0 (position (link (port b 0)) (ports el0)))
         (el1 (top-host-block (link (port b 1))))
         (pn1 (position (link (port b 1)) (ports el1))))
    (format stream "~%LINKS: port ~a of ~a and port ~a of ~a" pn0 el0 pn1 el1)
    (format stream "~%   Port 0:")
    (report-toplevel (port b 0))
    (format stream "~%   Port 1:")
    (report-toplevel (port b 1))))

;;; (.xformer [N] [&keys])
(defun .xformer (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.xformer :value (car rest) (cdr rest))
    (apply #'make-instance '.xformer :value nil rest)))

;;; Transforming of a port impedance by 1:N
;;; (.xform obj [N] [&keys])
(defun .xform (obj &rest rest)
  (let* ((x (if (and (car rest) (not (keywordp (car rest))))
              (apply #'make-instance '.xformer :value (car rest) (cdr rest))
              (apply #'make-instance '.xformer :value nil rest))))
    (connect (port obj) (port x 0))
    (port x 1)))


;;; XFORMER/, parameter inverted
;;; ----------------------------
;;; Turns ratio = N:1 
;;; in port0 -> port 1 direction:
;;; Impedance / N^2
;;; voltage / N; current x N
;;; Checked 23.04.07 mk

(defclass .xformer/ (.xformer) ())

(defmethod initialize-macro-block ((b .xformer/) &key value)
  (setq value (make-param b value))
  (let* ((pb0 (make-instance '.w-port-block))
         (p0 (port pb0))
         (pb1 (make-instance '.wa-port-block))
         (p1 (port pb1))
         (mul0 (.mul)) (mul1 (.mul))
         (xfc (make-instance '.xf-control)))
    (setf (domain p0) (domain b)
          (domain p1) (domain b)
          (ports b) (list p0 p1))
    (-> value (param xfc 0))
    (-> (out pb0) mul0 (in pb1))
    (-> (out pb1) mul1 (in pb0))
    (-> (out xfc 1) (in mul0 1))
    (-> (out xfc 0) (in mul1 1))
    (-> (host-block (zref p0)) (param xfc 2))
    (-> (out xfc 3) (param (host-block (zref p1)) 0))
    (-> (host-block (yref p0)) (param xfc 1))
    (-> (out xfc 2) (param (host-block (yref p1)) 0))))

;;; (.xformer/ [N] [&keys])
(defun .xformer/ (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.xformer/ :value (car rest) (cdr rest))
    (apply #'make-instance '.xformer/ :value nil rest)))

;;; Transforming of a port impedance by N:1
;;; (.xform/ obj [N] [&keys])
(defun .xform/ (obj &rest rest)
  (let* ((x (if (and (car rest) (not (keywordp (car rest))))
              (apply #'make-instance '.xformer/ :value (car rest) (cdr rest))
              (apply #'make-instance '.xformer/ :value nil rest))))
    (connect (port obj) (port x 0))
    (port x 1)))


#|
(inspect (.xformer))
(inspect (.xformer 2.0))
(inspect (.xformer/ 2.0))

(defparameter koe
  (patch ((r (.r 1.0))
          (e (.e 1.0 1.0)))
    (.par e (.xform/ r 2.0))
    (-> (.across r) (.probe "vr"))
    (-> (.across e) (.probe "ve"))))

(defparameter koe
  (patch ((r (.r 1.0))
          (e (.e 1.0 1.0)))
    (.par e (.xform r 2.0))
    (-> (.across r) (.probe "vr"))
    (-> (.across e) (.probe "ve"))))

(inspect koe)
|#


;;; TRANSFORMER_CONTROLLED REACTANCES

(defclass .Cx (root-block) ())

(defmethod initialize-macro-block ((b .Cx) &key value coeff)
  (let* ((c (.C))
         (xf (.xformer/))
         (sq (.sqrt*)))
    (connect (port c) (port xf 0))
    (setf (ports b) (list (port xf 1)))
    (setq value (make-param b value (out-type b) 'rc))
    (setq coeff (make-param b coeff (out-type b) 'c0))
    (-> coeff sq (param xf))))
         
(defun .Cx (&rest rest)
  (declare (dynamic-extent rest))
  (let* ((val (when (and (not (keywordp (car rest))) 
                         (car rest)) (car rest)))
         (coe (when (and (not (keywordp (car (cdr rest))))
                         (car (cdr rest))) (car (cdr rest)))))
    (apply #'make-instance '.Cx
           :value val :coeff coe rest)))

#|
(inspect (.Cx 0.0 1.0))
(inspect (.Cx t 1.0))  ???
(inspect (.Cx 1.0 t))  ???
(inspect (.Cx))
|#


;;; .GYRATOR
;;; --------
;;; Checked 24.04.07 mk

(defclass .gyrator (macro-block two-port-block) ()
  (:default-initargs
    :domain 'electric
    :out-type *default-type*))

(defmethod initialize-macro-block ((b .gyrator) &key)
  (let* ((pb0 (make-instance '.w-port-block))
         (p0 (port pb0))
         (pb1 (make-instance '.wa-port-block))
         (p1 (port pb1)))
    (setf (domain p0) (domain b)
          (domain p1) (domain b)
          (ports b) (list p0 p1))
    (-> (out pb0) (.neg) (in pb1))
    (-> (out pb1) (in pb0))
    (-> (host-block (zref p0))
        (param (host-block (zref p1)) 0))
    (-> (host-block (yref p0))
        (param (host-block (yref p1)) 0))))

(defmethod report-toplevel :after ((b .gyrator) &key (stream t))
  (let* ((el0 (top-host-block (link (port b 0))))
         (pn0 (position (link (port b 0)) (ports el0)))
         (el1 (top-host-block (link (port b 1))))
         (pn1 (position (link (port b 1)) (ports el1))))
    (format stream "~%LINKS: port ~a of ~a and port ~a of ~a" pn0 el0 pn1 el1)
    (format stream "~%   Port 0:")
    (report-toplevel (port b 0))
    (format stream "~%   Port 1:")
    (report-toplevel (port b 1))))

(defun .gyrator (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.gyrator rest))

(defun .gyrate (obj &rest rest)
  (let ((x (apply #'.gyrator rest)))
    (connect (port obj) (port x 0))
    (port x 1)))

#|
(inspect (.gyrator))

(defparameter koe
  (patch ((r (.r 4.0))
          (e (.e 1.0 1.0)))
    (.par e (.gyrate r))
    (-> (.across r) (.probe "outr"))
    (-> (.across e) (.probe "oute"))))

(defparameter koe
  (patch ((r (.r (.var 4.0)))
          (e (.e (.var 1.0) 1.0)))
    (.par e (.gyrate r))
    (-> (.across r) (.probe "outr"))
    (-> (.across e) (.probe "oute"))))

(inspect koe)
(c-code koe)

(defparameter koe
  (patch ((rx 0.5)
          (r (.r rx))
          (e (.e 1.0 1.0)))
    (.par e (.xform (.gyrate r) (/ rx)))
    (-> (.across r) (.probe "outr"))
    (-> (.across e) (.probe "oute"))))
|#

;;; .DUALIZER
;;; ---------
;;; Z <-> Y, U <-> I
;;; Reciprocal impedance
;;; Checked 24.04.07 mk

(defclass .dualizer (macro-block two-port-block) ()
  (:default-initargs
    :domain 'electric
    :out-type *default-type*))

(defmethod initialize-macro-block ((b .dualizer) &key)
  (let* ((g (.gyrator :domain (domain b)))
         (pg1 (port g 1))
         (x (.xformer :domain (domain b))))
    (-> (yref pg1) (param x 0))
    (connect (port g 1) (port x 0))  
    (setf (ports b) (list (port g 0) (port x 1)))))

(defun .dualizer (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.dualizer rest))

;;; Mapping of port to dual
(defun .dual (obj &rest rest)
  (let ((x (apply #'.dualizer rest)))
    (connect (port obj) (port x 0))
    (port x 1)))

#|
(inspect (.dualizer))

(defparameter koe ;;; ???
  (patch ((r (.r 4))
          (e (.e 1.0 1.0)))
    (.par e (.dual r))
    (-> (.across r) (.probe "outr"))
    (-> (.across e) (.probe "oute"))))

(defparameter koe
  (patch ((r (.r 4))
          (e (.e 1.0 1.0)))
    (.par2 e (.dual r))
    (-> (.across r) (.probe "outr"))
    (-> (.across e) (.probe "oute"))))

(inspect koe)
(load-patch koe)
|#


;;; .C-MUTATOR

(defclass .c-mutator (macro-block two-port-block) ()
  (:default-initargs
    :domain 'electric
    :out-type *default-type*))

(defmethod initialize-macro-block ((b .c-mutator) &key)
  (let* ((pb0 (make-instance '.w-port-block))
         (p0 (port pb0))
         (pb1 (make-instance '.wa-port-block))
         (p1 (port pb1))
         (sub (.sub))
         (add1 (.add))
         (add2 (.add))
         (d (.d)))
    (setf (domain p0) (domain b)
          (domain p1) (domain b)
          (ports b) (list p0 p1))
    (-> (out pb0) sub d add2 (in pb0))
    (-> (out pb0) add1 (in pb1))
    (-> d (in add1 1))
    (-> (out pb1) (in add2 1))
    (-> (out pb1) (in sub 1))
    (-> (host-block (zref p0))
        (param (host-block (zref p1)) 0))
    (-> (host-block (yref p0))
        (param (host-block (yref p1)) 0))))

(defmethod report-toplevel :after ((b .c-mutator) &key (stream t))
  (let* ((el0 (top-host-block (link (port b 0))))
         (pn0 (position (link (port b 0)) (ports el0)))
         (el1 (top-host-block (link (port b 1))))
         (pn1 (position (link (port b 1)) (ports el1))))
    (format stream "~%LINKS: port ~a of ~a and port ~a of ~a" pn0 el0 pn1 el1)
    (format stream "~%   Port 0:")
    (report-toplevel (port b 0))
    (format stream "~%   Port 1:")
    (report-toplevel (port b 1))))

(defun .c-mutator (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.c-mutator rest))

(defun .c-mutate (obj &rest rest)
  (let ((x (apply #'.c-mutator rest)))
    (connect (port obj) (port x 0))
    (port x 1)))


#|
(defparameter cm (.c-mutator))
(inspect cm)
(defpatch xx ()
  (.par (.c-mutate (.r 1.0)) (.r 1.0)))

(defpatch xx ((cm (.c-mutate (.r 1.0))))
  (-> (output (port cm)) (input (port cm))))
(c-code xx)
(compile-patch xx)
|#


;;; .C* (Mutator-based capacitor)

(defclass .c-mut-control (macro-group) ())

(defmethod initialize-macro-block ((b .c-mut-control) &key)
  (let* ((p (.p.)) (pc (.p.))
         (div (.div))
         (sr2 (.const (* 2.0 (srate b))))
         (mul (.mul :inputs 3)))
    (setf (name (param p)) 'c)
    (-> p mul)
    (-> (.sub 1.0d0 mul) (in div 0))
    (-> (.add 1.0d0 mul) (in div 1))
    (-> pc (in mul 1))
    (-> sr2 (in mul 2))
    (setf (outputs b) (list (out div)))
    (setf (params b)
          (list (param p) (param pc)))))


(defclass .C* (root-block macro-block) ()
  (:default-initargs
    :domain 'electric))

(defmethod initialize-macro-block ((b .C*) &key value)
  (setq value (make-param b value))
  (let* ((mul (.mul))
         (cmc (make-instance '.c-mut-control))
         (cmut (.c-mutator))
         (cmp (port cmut 1)))
    (-> value (param cmc 0))
    (-> (zref cmp) (param cmc 1))
    (-> (output cmp) mul (input cmp))
    (-> cmc (in mul 1))
    (push (port cmut 0) (ports b))))

(defun .C* (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.C* :value (car rest) (cdr rest))
    (apply #'make-instance '.C* :value nil rest)))


;;; .L-MUTATOR

(defclass .L-mutator (.c-mutator) ())

(defmethod initialize-macro-block ((b .l-mutator) &key)
  (let* ((pb0 (make-instance '.w-port-block))
         (p0 (port pb0))
         (pb1 (make-instance '.wa-port-block))
         (p1 (port pb1))
         (sub (.sub))
         (add1 (.add))
         (add2 (.add))
         (d (.d-)))
    (setf (domain p0) (domain b)
          (domain p1) (domain b)
          (ports b) (list p0 p1))
    (-> (out pb0) sub d add2 (in pb0))
    (-> (out pb0) add1 (in pb1))
    (-> d (in add1 1))
    (-> (out pb1) (in add2 1))
    (-> (out pb1) (in sub 1))
    (-> (host-block (zref p0))
        (param (host-block (zref p1)) 0))
    (-> (host-block (yref p0))
        (param (host-block (yref p1)) 0))))

(defun .l-mutator (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.l-mutator rest))

(defun .l-mutate (obj &rest rest)
  (let ((x (apply #'.l-mutator rest)))
    (connect (port obj) (port x 0))
    (port x 1)))


;;; .L* (Mutator-based inductor)

;;; ????


;;; Resistance with I-port (.w-port)

;;; Z&Y parameter control for R*

(defclass .zy*-control (macro-group) ())

(defmethod initialize-macro-block ((b .zy*-control) &key
                                       (z-p t))
  (let* ((p (.p.)) (inv (.inv)))
    (cond (z-p (setf (outputs b) (list (out p) (out inv))))
          (t (setf (outputs b) (list (out inv) (out p)))))
    (setf (params b) (list (param p)))
    (-> p inv)))


(defclass .R* (one-port-macro) ()
  (:default-initargs
    :domain 'electric))

(defmethod initialize-macro-block ((b .R*) &key 
                                       value port-name
                                       (port-block-type '.w-port-block)
                                       (port-type 'w-port))
  (setq value (make-param b value)) ;;; otherwise constant
  (let* ((pb (make-instance port-block-type
               :port-type port-type))
         (p (port pb)))
    (setf (domain p) (domain b)
          (ports b) (list p)
          (name p) port-name)
    (process-immittance b pb value)))

(defmethod process-immittance ((b .R*) pb value)

  value nil)

#|
  (let* ((pc (make-instance '.zy*-control)))
    (-> value (param pc 0))
    (-> (out pc 0) (param pb 0))
    (-> (out pc 1) (param pb 1))
    (-> value (.datac 0.0d0) pb)))
|#

(defun .R* (&rest rest)
  (declare (dynamic-extent rest))
  (if (and (car rest) (not (keywordp (car rest))))
    (apply #'make-instance '.R* :value (car rest) (cdr rest))
    (apply #'make-instance '.R* :value nil rest)))


; (inspect (.R*))


;;; CONNECTING WDF MODELS

;;; Parallel connection

(defun .par% (&rest rest)
  (let ((len (length rest)) par)
    (cond ((zerop len) (error "No port block to connect"))
          ((= len 1) (port (first rest)))
          (t (setq par (make-instance '.par3-adaptor))
             (connect (port par 0) (port (first rest)))
             (connect (port par 1) (port (second rest)))
             (unless (eq (domain (port par 0))
                         (domain (port par 1)))
               (error "Port incompatibility in ~a" par))
             (setf (domain (port par 2)) (domain (port par 0)))
             (setq rest (cons (port par 2) (cddr rest)))
             (apply #'.par% rest)))))

(defun .par (&rest rest)
  (declare (dynamic-extent rest))
  (let* (ws was px)
    (loop for x in rest
          for p = (port x)
          do (if (typep p 'wa-port)
               (push p was) (push p ws)))
    (when (> (length ws) 1)
      (error "Can't .par more than 1 w-port"))
    (setq px (apply #'.par% was))
    (cond ((not ws) px)
          (t (connect px (car ws)) nil))))

(defun .par* (&rest rest)
  (declare (dynamic-extent rest))
  (.par2 (car rest)
         (apply #'.par% (reverse (cdr rest)))))

(defmethod .par2 ((p0 wa-port) (p1 wa-port))
  (let* ((ada (make-instance '.par2-adaptor)))
    (connect p0 (port ada 0))
    (connect p1 (port ada 1)))
  nil)

(defmethod .par2 ((b one-port-block) (p1 wa-port))
  (.par2 (port b) p1))

(defmethod .par2 ((p1 wa-port) (b one-port-block))
  (.par2 (port b) p1))

(defmethod .par2 ((b1 one-port-block) (b2 one-port-block))
  (.par2 (port b1) (port b2)))

#|
(defparameter koe
  (patch ((r (.r 1.0))
          (e (.e 1.0 9.0)))
    (.par2 e r)
    (-> (.across r) (.probe "out"))))

(funcall (lstepfun koe))
(report-toplevel koe)
(inspect koe)
|#

;;; Port connectivity

(defmethod <=> ((p0 t) (p1 t))
  (error "Cannot connect ~a and ~a" p0 p1))

(defmethod <=> ((p0 wa-port) (p1 w-port))
  (connect p0 p1))

(defmethod <=> ((p0 w-port) (p1 wa-port))
  (connect p0 p1))

(defmethod <=> ((b0 one-port-block) (p1 w-port))
  (connect (port b0) p1))

(defmethod <=> ((p1 w-port) (b0 one-port-block))
  (connect (port b0) p1))

(defmethod <=> ((p0 wa-port) (p1 wa-port))
  (.par2 p0 p1))


;;; Series connection

(defun .ser% (&rest rest)
  (let ((len (length rest)) ser)
    (cond ((zerop len) (error "No port block to connect"))
          ((= len 1) (port (first rest)))         
          (t (setq ser (make-instance '.ser3-adaptor))
             (connect (port ser 0) (port (first rest)))
             (connect (port ser 1) (port (second rest)))
             (unless (eq (domain (port ser 0))
                         (domain (port ser 1)))
               (error "Port incompatibility in ~a" ser))
             (setf (domain (port ser 2)) (domain (port ser 0)))
             (setq rest (cons (port ser 2) (cddr rest)))
             (apply #'.ser% rest)))))

(defun .ser (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'.ser% (reverse rest)))

(defun .ser (&rest rest)
  (declare (dynamic-extent rest))
  (let* (ws was px)
    (loop for x in rest
          for p = (port x)
          do (if (typep p 'wa-port)
               (push p was) (push p ws)))
    (when (> (length ws) 1)
      (error "Can't .ser more than one w-port"))
    (setq px (apply #'.ser% was))
    (cond ((not ws) px)
          (t (connect px (car ws)) nil))))

#|
(defparameter r1 (.r 1.0))
(defparameter r2 (.r 2.0))
(inspect (.ser r1 r2))

(defparameter pat
  (patch ((c (.c 0.01 :name 'r))
          (l (.l 0.01 :name 'l))
          (e (.e 1.0 0.1)))
    (.ser e c l)
    (-> (.voltage l) (.probe "out"))
    (-> (.current l) (.probe "out2"))))

(to-matlab pat)
(report-schedule pat)

(defparameter pat
  (patch ((c (.c 0.01 :name 'r))
          (l (.l 0.01 :name 'l))
          (e (.e 1.0 0.1)))
    (.root (.diode :direction '+) (.ser e c l))
    (-> (.across l) (.probe 'acrl))))

(to-matlab pat)

(defparameter pat
  (patch ((r (.r 1.0))
          (e (.e 1.0 1.0)))
    (.root (.diode :direction '+) (.ser e r))
    (-> (.across r) (.probe "out"))))

(funcall (lstepfun pat))
(inspect pat)

(defparameter pat
  (patch ((r (.r 1.0))
          (e (.e 1.0 1.0))
          (x (.ext-root :mfun "bc_extfun")))
    (.root x (.ser e r))
    (-> (.across r) (.probe "out"))))
|#

(defmethod connect-root ((p1 wa-port) (p2 w-port))
  (setf (link p1) p2 (link p2) p1)
  (connect (output p1) (input p2))
  (connect (output p2) (input p1))
  (connect (zref p1) (zref p2))
  (connect (yref p1) (yref p2))
  (setf ;;; (domain p2) (domain p1) ;;; !!!
        (through p2) (through p1)
        (across p2) (across p1))
  nil)

(defmethod .root ((w w-port) (wa wa-port))
  (connect-root wa w)
  nil)

(defmethod .root ((b root-block) (wa wa-port))
  (.root (port b) wa)
  nil)


;;; PHYSICAL SIGNAL PROBES

;;; Across

(defclass .wa-across (port-probe) ())

(defmethod initialize-macro-block ((b .wa-across) &key)
  (let* ((add (.add))
         (p (port-link b))
         (win (out (host-block (input p))))
         (wout (output p)))
    (-> win (in add 0))
    (-> wout (in add 1))
    (setf (outputs b) (list (out add)))))

(defmethod .across ((p wa-port) &rest rest)
  (cond ((across p) (across p))
        (t (setf (across p)
                 (apply #'make-instance '.wa-across
                        :port-link p :block-link (top-host-block p)
                        rest)))))

(defmethod .across ((p w-port) &rest rest)
  (apply #'.across (link p) rest))

(defmethod .across ((b t) &rest rest)
  (declare (ignore rest))
  (error "Across not available in ~a" b))

;;; Voltage

(defclass .wa-voltage (.wa-across) ()
  (:default-initargs
    :domain 'electric))
  
(defmethod .voltage ((p wa-port) &rest rest)
  (cond ((across p) (across p))
        (t (setf (across p)
                 (apply #'make-instance '.wa-voltage
                        :port-link p :block-link (top-host-block p)
                        rest)))))

(defmethod .voltage ((p w-port) &rest rest)
  (apply #'.voltage (link p) rest))

(defmethod .voltage ((b t) &rest rest)
  (declare (ignore rest))
  (error "Voltage not available in ~a" b))

;;; Force

(defclass .wa-force (.wa-across) ()
  (:default-initargs
    :domain 'mechanic))
  
(defmethod .force ((p wa-port) &rest rest)
  (cond ((across p) (across p))
        (t (setf (across p)
                 (apply #'make-instance '.wa-force
                        :port-link p :block-link (top-host-block p)
                        rest)))))

(defmethod .force ((p w-port) &rest rest)
  (apply #'.force (link p) rest))

(defmethod .force ((b t) &rest rest)
  (declare (ignore rest))
  (error "Force not available in ~a" b))

;;; Pressure

(defclass .wa-pressure (.wa-across) ()
  (:default-initargs
    :domain 'acoustic))
  
(defmethod .pressure ((p wa-port) &rest rest)
  (cond ((across p) (across p))
        (t (setf (across p)
                 (apply #'make-instance '.wa-pressure
                        :port-link p :block-link (top-host-block p)
                        rest)))))

(defmethod .pressure ((p w-port) &rest rest)
  (apply #'.pressure (link p) rest))

(defmethod .pressure ((b t) &rest rest)
  (declare (ignore rest))
  (error "Pressure not available in ~a" b))


;;; Through

(defclass .wa-through (port-probe) ())

(defmethod initialize-macro-block ((b .wa-through) &key)
  (let* ((sub (.sub))
         (mul (.mul))
         (p (port-link b))
         (win (out (host-block (input p))))
         (wout (output p))
         (yref (yref p)))
    (-> win (in sub 0) (in mul 0))
    (-> wout (in sub 1))
    (-> yref (in mul 1))
    (setf (outputs b) (list (out mul)))))

(defmethod .through ((p wa-port) &rest rest)
  (cond ((through p) (through p))
        (t (setf (through p)
                 (apply #'make-instance '.wa-through
                        :port-link p :block-link (top-host-block p)
                        rest)))))

(defmethod .through ((b t) &rest rest)
  (declare (ignore rest))
  (error "Through-variable not available in ~a" b))

;;; Current

(defclass .wa-current (.wa-through) ()
  (:default-initargs
    :domain 'electric))

(defmethod .current ((p wa-port) &rest rest)
  (cond ((through p) (through p))
        (t (setf (through p)
                 (apply #'make-instance '.wa-current
                        :port-link p :block-link (top-host-block p)
                        rest)))))

(defmethod .current ((p w-port) &rest rest)
  (apply #'.current (link p) rest))

(defmethod .current ((b t) &rest rest)
  (declare (ignore rest))
  (error "Current not available in ~a" b))

;;; Velocity

(defclass .wa-velocity (.wa-through) ()
  (:default-initargs
    :domain 'mechanic))

(defmethod .velocity ((p wa-port) &rest rest)
  (cond ((through p) (through p))
        (t (setf (through p)
                 (apply #'make-instance '.wa-velocity
                        :port-link p :block-link (top-host-block p)
                        rest)))))

(defmethod .velocity ((p w-port) &rest rest)
  (apply #'.velocity (link p) rest))

(defmethod .velocity ((b t) &rest rest)
  (declare (ignore rest))
  (error "Velocity not available in ~a" b))


;;; Flow (volume velocity)

(defclass .wa-flow (.wa-through) ()
  (:default-initargs
    :domain 'acoustic))

(defmethod .flow ((p wa-port) &rest rest)
  (cond ((through p) (through p))
        (t (setf (through p)
                 (apply #'make-instance '.wa-flow
                        :port-link p :block-link (top-host-block p)
                        rest)))))

(defmethod .flow ((p w-port) &rest rest)
  (apply #'.flow (link p) rest))

(defmethod .flow ((b t) &rest rest)
  (declare (ignore rest))
  (error "Flow not available in ~a" b))

;;; Power

(defclass .wa-power (port-probe) ())

(defmethod initialize-macro-block ((b .wa-power) &key ac th)
  (let* ((mul (.mul)))
    (-> ac (in mul 0))
    (-> th (in mul 1))
    (setf (outputs b) (list (out mul)))))

(defmethod .power ((p wa-port) &rest rest)
  (let* ((th (through p))
         (ac (across p)))
    (unless th (setq th (.through p)))
    (unless ac (setq ac (.across p)))
    (setf (power p)
          (apply #'make-instance '.wa-power
                 :port-link p :block-link (top-host-block p)
                 :ac ac :th th rest))))

(defmethod .power ((p w-port) &rest rest)
  (apply #'.power (link p) rest))

(defmethod .power ((b t) &rest rest)
  (declare (ignore rest))
  (error "Power not available in ~a" b))

;;; Energy

(defclass .wa-energy (port-probe) ())

(defmethod initialize-macro-block ((b .wa-energy) &key pwr)
  (let* ((integ (.integb pwr)))
    (setf (outputs b) (list (out integ)))))

(defmethod .energy ((p wa-port) &rest rest)
  (let* ((pwr (power p)))
    (unless pwr (setq pwr (.power p)))
    (setf (energy p)
          (apply #'make-instance '.wa-energy
                 :port-link p :block-link (top-host-block p)
                 :pwr pwr rest))))

(defmethod .energy ((p w-port) &rest rest)
  (apply #'.energy (link p) rest))

(defmethod .energy ((b t) &rest rest)
  (declare (ignore rest))
  (error "Energy not available in ~a" b))

(provide :BC-physical)
