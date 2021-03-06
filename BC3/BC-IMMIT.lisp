
(in-package :BC)


;;; CONSOLIDATED BLOCKS AND MODAL DECOMPOSITION

;;; Parallel and series RCL connections

;;; .RL series connection (macro-block, not fully optimized)

(defclass .RLserp (macro-group) ())

(defmethod initialize-macro-block ((b .RLserp) &key)
  (let* ((r (.p. :name 'r))
         (l (.p. :name 'l))
         (2sr (.datac (* 2.0d0 (srate b))))
         (1s (.datac 1.0d0))
         (rl (.mul l 2sr))
         (rp (.add r rl))
         (/rp (.inv rp))
         (c (.div r rp))
         (1-c (.sub 1s c)))
    (-> l (in 2sr) (in 1s))
    (setf (outputs b)
          (list (out 1-c) (out c)
                (out rp) (out /rp)))
    (setf (params b)
          (list (param r) (param l)))))

(defclass .RLser (macro-block one-port-block) ()
  (:default-initargs
    :domain 'electric))

(defmethod initialize-macro-block ((b .RLser) &key r l)
  (let* ((pb (make-instance '.wa-port-block))
         (port (port pb))
         (rlsp (make-instance '.RLserp))
         (lp1 (make-instance '.lp1ba 
                :b (out rlsp 0) :a (out rlsp 1))))
    (add-param b l (param rlsp 'l) :name 'l)
    (add-param b r (param rlsp 'r) :name 'r)
    (-> (out rlsp 2) (param pb 'z))
    (-> (out rlsp 3) (param pb 'y))
    (-> (out pb) lp1 (.d-) (in pb))
    (setf (domain port) (domain b))
    (setf (ports b) (list port))))

(defun .RLser (&rest rest)
  (declare (dynamic-extent rest))
  (apply 'make-instance '.RLser rest))


;;; .RL parallel connection (macro-block, not fully optimized)

(defclass .RLparp (macro-group macro-block) ())

(defmethod initialize-macro-block ((b .RLparp) &key)
  (let* ((r (.p. :name 'r))
         (l (.p. :name 'l))
         (2sr (.datac (* 2.0d0 (srate b))))
         (1s (.datac 1.0d0))
         (rl (.mul l 2sr))
         (rp+ (.add r rl))
         (rp* (.mul r rl))
         (rp (.div rp* rp+))
         (/rp (.inv rp))
         (c- (.neg (.div rp r)))
         (1-c (.add 1s c-)))
    (-> l (in 2sr) (in 1s))
    (setf (outputs b)
          (list (out 1-c) (out c-)
                (out rp) (out /rp)))
    (setf (params b)
          (list (param r) (param l)))))

(defclass .RLpar (macro-block one-port-block) ()
  (:default-initargs
    :domain 'electric))

(defmethod initialize-macro-block ((b .RLpar) &key r l)
  (let* ((pb (make-instance '.wa-port-block))
         (port (port pb))
         (rlsp (make-instance '.RLparp))
         (lp1 (make-instance '.lp1ba 
                :b (out rlsp 0) :a (out rlsp 1))))
    (add-param b l (param rlsp 'l) :name 'l)
    (add-param b r (param rlsp 'r) :name 'r)
    (-> (out rlsp 2) (param pb 'z))
    (-> (out rlsp 3) (param pb 'y))
    (-> (out pb) lp1 (.d-) (in pb))
    (setf (domain port) (domain b))
    (setf (ports b) (list port))))

(defun .RLpar (&rest rest)
  (declare (dynamic-extent rest))
  (apply 'make-instance '.RLpar rest))


;;; .RC series connection (macro-block, not fully optimized)

(defclass .RCserp (macro-group macro-block) ())

(defmethod initialize-macro-block ((b .RCserp) &key)
  (let* ((r (.p. :name 'r))
         (cp (.p. :name 'c))
         (2sr (.datac (/ 1.0d0 2.0d0 (srate b))))
         (rc (.div 2sr cp))
         (rp (.add r rc))
         (/rp (.inv rp))
         (c- (.neg (.div r rp)))
         (1s (.datac 1.0d0))
         (1-c (.add 1s c-)))
    (-> r (in 2sr)) (-> r (in 1s))
    (setf (outputs b)
          (list (out 1-c) (out c-)
                (out rp) (out /rp)))
    (setf (params b)
          (list (param r) (param cp)))))

(defclass .RCser (macro-block one-port-block) ()
  (:default-initargs
    :domain 'electric))

(defmethod initialize-macro-block ((b .RCser) &key r c)
  (let* ((pb (make-instance '.wa-port-block))
         (port (port pb))
         (rlsp (make-instance '.RCserp))
         (lp1 (make-instance '.lp1ba 
                :b (out rlsp 0) :a (out rlsp 1))))
    (add-param b c (param rlsp 'c) :name 'c)
    (add-param b r (param rlsp 'r) :name 'r)
    (-> (out rlsp 2) (param pb 'z))
    (-> (out rlsp 3) (param pb 'y))
    (-> (out pb) lp1 (.d) (in pb))
    (setf (domain port) (domain b))
    (setf (ports b) (list port))))

(defun .RCser (&rest rest)
  (declare (dynamic-extent rest))
  (apply 'make-instance '.RCser rest))


;;; .RC parallel connection (macro-block, not fully optimized)

(defclass .RCparp (macro-group macro-block) ())

(defmethod initialize-macro-block ((b .RCparp) &key)
  (let* ((r (.p. :name 'r))
         (cp (.p. :name 'c))
         (2sr (.datac (/ 1.0d0 2.0d0 (srate b))))
         (rc (.div 2sr cp))
         (rp+ (.add r rc))
         (rp* (.mul r rc))
         (rp (.div rp* rp+))
         (/rp (.inv rp))
         (c (.div rp r))
         (1s (.datac 1.0d0))
         (1-c (.sub 1s c)))
    (-> r (in 2sr)) (-> r (in 1s))
    (setf (outputs b)
          (list (out 1-c) (out c)
                (out rp) (out /rp)))
    (setf (params b)
          (list (param r) (param cp)))))

(defclass .RCpar (macro-block one-port-block) ()
  (:default-initargs
    :domain 'electric))

(defmethod initialize-macro-block ((b .RCpar) &key r c)
  (let* ((pb (make-instance '.wa-port-block))
         (port (port pb))
         (rlsp (make-instance '.RCparp))
         (lp1 (make-instance '.lp1ba 
                :b (out rlsp 0) :a (out rlsp 1))))
    (add-param b c (param rlsp 'c) :name 'c)
    (add-param b r (param rlsp 'r) :name 'r)
    (-> (out rlsp 2) (param pb 'z))
    (-> (out rlsp 3) (param pb 'y))
    (-> (out pb) lp1 (.d) (in pb))
    (setf (domain port) (domain b))
    (setf (ports b) (list port))))

(defun .RCpar (&rest rest)
  (declare (dynamic-extent rest))
  (apply 'make-instance '.RCpar rest))


;;; .LC series connection (macro-block, not fully optimized)

(defclass .LCserp (macro-group macro-block) ())

(defmethod initialize-macro-block ((b .LCserp) &key)
  (let* ((cr (.p. :name 'c))
         (l (.p. :name 'l))
         (2sr (.datac (* 2.0d0 (srate b))))
         (rl (.mul l 2sr))
         (/2sr (.datac (/ 1.0d0 2.0d0 (srate b))))
         (rc (.div /2sr cr))
         (rp (.add rc rl))
         (/rp (.inv rp))
         (c (.mul (.sub rc rl) /rp)))
    (-> l (in 2sr)) (-> l (in /2sr))
    (setf (outputs b)
          (list (out c) (out rp) (out /rp)))
    (setf (params b)
          (list (param l) (param cr)))))

(defclass .LCser (macro-block one-port-block) ()
  (:default-initargs
    :domain 'electric))

(defmethod initialize-macro-block ((b .LCser) &key l c)
  (let* ((pb (make-instance '.wa-port-block))
         (port (port pb))
         (rlsp (make-instance '.LCserp))
         (lp1 (make-instance '.ap1 
                :a (out rlsp 0))))
    (add-param b l (param rlsp 'l) :name 'l)
    (add-param b c (param rlsp 'c) :name 'c)
    (-> (out rlsp 1) (param pb 'z))
    (-> (out rlsp 2) (param pb 'y))
    (-> (out pb) lp1 (.d) (in pb))
    (setf (domain port) (domain b))
    (setf (ports b) (list port))))

(defun .LCser (&rest rest)
  (declare (dynamic-extent rest))
  (apply 'make-instance '.LCser rest))


;;; .LC parallel connection (macro-block, not fully optimized)

(defclass .LCparp (macro-group macro-block) ())

(defmethod initialize-macro-block ((b .LCparp) &key)
  (let* ((cr (.p. :name 'c))
         (l (.p. :name 'l))
         (2sr (.datac (* 2.0d0 (srate b))))
         (rl (.mul l 2sr))
         (/2sr (.datac (/ 1.0d0 2.0d0 (srate b))))
         (rc (.div /2sr cr))
         (rp+ (.add rc rl))
         (rp+/ (.inv rp+))
         (rp (.mul (.mul rc rl) rp+/))
         (/rp (.inv rp))
         (c (.mul (.sub rc rl) rp+/)))
    (-> l (in 2sr)) (-> l (in /2sr))
    (setf (outputs b)
          (list (out c) (out rp) (out /rp)))
    (setf (params b)
          (list (param l) (param cr)))))

(defclass .LCpar (macro-block one-port-block) ()
  (:default-initargs
    :domain 'electric))

(defmethod initialize-macro-block ((b .LCpar) &key l c)
  (let* ((pb (make-instance '.wa-port-block))
         (port (port pb))
         (rlsp (make-instance '.LCparp))
         (lp1 (make-instance '.ap1 
                :a (out rlsp 0))))
    (add-param b l (param rlsp 'l) :name 'l)
    (add-param b c (param rlsp 'c) :name 'c)
    (-> (out rlsp 1) (param pb 'z))
    (-> (out rlsp 2) (param pb 'y))
    (-> (out pb) lp1 (.d-) (in pb))
    (setf (domain port) (domain b))
    (setf (ports b) (list port))))

(defun .LCpar (&rest rest)
  (declare (dynamic-extent rest))
  (apply 'make-instance '.LCpar rest))


;;; .LCR series connection (macro-block, not fully optimized)

(defclass .LCRserp (macro-group macro-block) ())

(defmethod initialize-macro-block ((b .LCRserp) &key)
  (let* ((cr (.p. :name 'c)) ; C
         (l (.p. :name 'l))  ; L
         (r (.p. :name 'r))  ; R
         (2sr (.datac (* 2.0d0 (srate b))))
         (rl (.mul l 2sr))   ; RpL
         (/2sr (.datac (/ 1.0d0 2.0d0 (srate b))))
         (rc (.div /2sr cr)) ; RpC
         (rp (.add rc rl r)) ; Rp
         (/rp (.inv rp))     ; Gp = 1/Rp
         (c (.mul (.sub rc rl) /rp)) ; c-coeff
         (d- (.neg (.mul r /rp)))  ; -d
         (1s (.datac 1.0d0))   ; ones
         (1-d (.add 1s d-))) ; 1-d
    (-> l (in 2sr)) (-> l (in /2sr)) (-> l (in 1s))
    (setf (outputs b)
          (list (out c) (out d-) (out 1-d)
                (out rp) (out /rp)))
    (setf (params b)
          (list (param l) (param cr) (param r)))))

(defclass .LCRser (macro-block one-port-block) ()
  (:default-initargs
    :domain 'electric))

(defmethod initialize-macro-block ((b .LCRser) &key l c r)
  (let* ((pb (make-instance '.wa-port-block))
         (port (port pb))
         (rlsp (make-instance '.LCRserp))
         (d1 (.d)) (d2 (.d))
         (a1 (.mul d1 (out rlsp 0)))
         (a2 (.mul d2 (out rlsp 1)))
         (sub1 (.sub (out pb) a1 a2))
         (b0 (.mul sub1 (out rlsp 0)))
         (b1 (.mul d1 (out rlsp 2)))
         (add2 (.add b0 b1)))
    (add-param b l (param rlsp 'l) :name 'l)
    (add-param b c (param rlsp 'c) :name 'c)
    (add-param b r (param rlsp 'r) :name 'r)
    (-> (out rlsp 3) (param pb 'z))
    (-> (out rlsp 4) (param pb 'y))
    (-> sub1 d1 d2)
    (-> add2 (.d) (in pb))
    (setf (domain port) (domain b))
    (setf (ports b) (list port))))

(defun .LCRser (&rest rest)
  (declare (dynamic-extent rest))
  (apply 'make-instance '.LCRser rest))


;;; .LCR parallel connection (macro-block, not fully optimized)

(defclass .LCRparp (macro-group macro-block) ())

(defmethod initialize-macro-block ((b .LCRparp) &key)
  (let* ((cr (.p. :name 'c))
         (l (.p. :name 'l))
         (2sr (.datac (* 2.0d0 (srate b))))
         (rl (.mul l 2sr))
         (/2sr (.datac (/ 1.0d0 2.0d0 (srate b))))
         (rc (.div /2sr cr))
         (rp+ (.add rc rl))
         (rp+/ (.inv rp+))
         (rp (.mul (.mul rc rl) rp+/))
         (/rp (.inv rp))
         (c (.mul (.sub rc rl) rp+/)))
    (-> l (in 2sr)) (-> l (in /2sr))
    (setf (outputs b)
          (list (out c) (out rp) (out /rp)))
    (setf (params b)
          (list (param l) (param cr)))))

(defclass .LCRpar (macro-block one-port-block) ()
  (:default-initargs
    :domain 'electric))

(defmethod initialize-macro-block ((b .LCRpar) &key l c)
  (let* ((pb (make-instance '.wa-port-block))
         (port (port pb))
         (rlsp (make-instance '.LCRparp))
         (lp1 (make-instance '.ap1 
                :a (out rlsp 0))))
    (add-param b l (param rlsp 'l) :name 'l)
    (add-param b c (param rlsp 'c) :name 'c)
    (-> (out rlsp 1) (param pb 'z))
    (-> (out rlsp 2) (param pb 'y))
    (-> (out pb) lp1 (.d-) (in pb))
    (setf (domain port) (domain b))
    (setf (ports b) (list port))))

(defun .LCRpar (&rest rest)
  (declare (dynamic-extent rest))
  (apply 'make-instance '.LCRpar rest))


;;; ----

;;; .Z2sub Impedance subfilter, 2nd order

(defclass .Z2sub (macro-block)
  ((b0p :initform nil :accessor b0p)))

(defmethod initialize-macro-block ((b .Z2sub) &key
                                       arg a1 a2 
                                       (b0 t) b1 b2)
  (let* ((a1p (when a1 (.p. :name 'a1)))
         (a2p (when a2 (.p. :name 'a2)))
         (b0p (if b0 (.p. :name 'b0)
                  (error "b0 param not given in ~a" b)))
         (b1p (when b1 (.p. :name 'b1)))
         (b2p (when b2 (.p. :name 'b2)))
         (d1 (when (or a1 a2 b1 b2) (.d)))
         (d2 (when (or a2 b2) (.d)))
         (a1s (when a1p (.mul d1 a1p)))
         (a2s (when a2p (.mul d2 a2p)))
         (as- (cond ((and a1s a2s)
                     (.neg (.add a1s a2s)))
                    (a2s (.neg a2s))
                    (a1s (.neg a1s))))
         (b0s (if as- (.mul as- b0p)
                  (let ((x (.datac 0.0d0)))
                    (-> b0p x) x)))
         (b1s (when b1p (.mul d1 b1p)))
         (b2s (when b2p (.mul d2 b2p)))
         (bs (cond ((and b1s b2s)
                    (.add b0s b1s b2s))
                   (b1s (.add b0s b1s))
                   (b2s (.add b0s b2s))
                   (t b0s)))
         (as (when as- (.add)))
         (in (if as as b0s)))
    (setf (b0p b) b0p)
    (when b2p (add-param b b2 (param b2p)))
    (when b1p (add-param b b1 (param b1p)))
    (add-param b b0 (param b0p))
    (when a2p (add-param b a2 (param a2p)))
    (when a1p (add-param b a1 (param a1p)))
    (setf (name (in in)) 'sigin)
    (when as (-> as- (in as 1)))
    (when d1 (-> in d1))
    (when d2 (-> d1 d2))
    (if arg (-> (.val arg) (in in))
        (setf (inputs b) (list (in in))))
    (setf (outputs b) (list (out bs)))))

(defun .Z2sub (&rest rest)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (apply #'make-instance '.z2sub :arg (car rest) (cdr rest))
    (apply #'make-instance '.z2sub rest)))


;;; Combined impedance subfilter, 2nd order

(defclass .z2sersub (macro-block)
  ((b0p :initform nil :accessor b0p)))

(defmethod initialize-macro-block ((b .z2sersub) &rest rest &key)
  (let* ((copy (.copy))
         (sub (apply #'make-instance '.z2sub :arg copy rest))
         (b0p (b0p sub))
         (sum (.sum sub))
         (sumb (.sum b0p)))
    (setf (host-patch sub) b)
    (setf (b0p b) sumb)
    (-> b0p (param copy))
    (setf (inputs b) (list (in copy))
          (outputs b) (list (out sum))
          (params b) (params sub))))


;;; Impedance subfilter wrapper

(defclass .zwrapper (macro-block one-port-block) ()
  (:default-initargs
    :domain 'electric))

(defmethod initialize-macro-block ((b .zwrapper) &key
                                       subfilt rp delay-p)
  (setf (host-patch subfilt) b)
  (let* ((pb (make-instance '.wa-port-block))
         (port (port pb))
         (sub (.sub))
         (mul (.mul :inputs 3))
         (b0 (if rp rp (b0p subfilt)))
         (/b0 (.inv b0))
         (c05 (.datac 0.5d0)))
    (setf (domain port) (domain b))
    (setf (params b) (params subfilt))
    (-> b0 c05)
    (-> (out pb) sub (in subfilt))
    (if delay-p (-> subfilt (.d) mul)
        (-> subfilt mul))
    (-> b0 (param pb 'z))
    (-> /b0 (param pb 'y))
    (-> c05 (in mul 1))
    (-> /b0 (in mul 2))
    (-> mul (in pb))
    (-> mul (in sub 1))
    (setf (ports b) (list port))))


;;; .Z2 Second order impedance given in rational form
;;;  Can be vectorized also

(defclass .Z2 (macro-block one-port-block) ()
  (:default-initargs
    :domain 'electric))

(defmethod initialize-macro-block ((b .Z2) &rest rest &key)
  (let* ((sub (apply #'make-instance '.z2sub :arg nil rest))
         (wrap (make-instance '.zwrapper :subfilt sub)))
    (setf (outputs b) (outputs wrap)
          (inputs b) (inputs wrap)
          (params b) (params wrap)
          (ports b) (ports wrap))))

(defun .Z2 (&rest rest)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (apply #'make-instance '.z2 :arg (car rest) (cdr rest))
    (apply #'make-instance '.z2 rest)))


;;; .Y2 Second order admittance given in rational form
;;;  Can be vectorized also

(defun .Y2 (&rest rest)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (.dual (apply #'make-instance '.z2 :arg (car rest) (cdr rest)))
    (.dual (apply #'make-instance '.z2 rest))))


;;; .Z2ser Second order impedances in series

(defclass .Z2ser (macro-block one-port-block) ()
  (:default-initargs
    :domain 'electric))

(defmethod initialize-macro-block ((b .Z2ser) &rest rest &key)
  (let* ((sub (apply #'make-instance '.z2sersub :arg nil rest))
         (wrap (make-instance '.zwrapper :subfilt sub)))
    (setf (outputs b) (outputs wrap)
          (inputs b) (inputs wrap)
          (params b) (params wrap)
          (ports b) (ports wrap))))

(defun .Z2ser (&rest rest)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (apply #'make-instance '.z2ser :arg (car rest) (cdr rest))
    (apply #'make-instance '.z2ser rest)))


;;; .Y2par Second order admittance in parallel

(defun .Y2par (&rest rest)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (.dual (apply #'make-instance '.z2ser :arg (car rest) (cdr rest)))
    (.dual (apply #'make-instance '.z2ser rest))))

#|
(defpatch koe ((zw (.z2sub :a1 -0.3 :a2 0.9 
                           :b0 1.0 :b1 0.0 :b2 0.0))
               (out (.probe "out")))
  (-> (.imp1) zw out)
  (defun pr () (float (at (out out)) 1.0)))
(inspect koe)

(inspect
 (make-instance '.zwrapper :subfilt (.z2sub) ))

;;; Vectorized case WWWW ***** WWW 
(defpatch koe ((zw (make-instance '.zwrapper
                     :subfilt
                     (.z2sub :a1 '(0.0 0.0) :a2 '(0.9 0.9) 
                             :b0 '(1.0 1.0) :b1 '(0.0 0.0)
                             :b2 '(-1.0 -1.0))))
               (e (.E (.imp1 '(1.0 1.0)) '(1000.0 1000.0)))
               (out (.probe "out")))
  (.par e zw)
  (-> (.voltage zw) out)
  (defun pr () (float (at (out out) 0) 1.0)))

;;; ser case
(defpatch koe ((zw (make-instance '.zwrapper
                     :subfilt
                     (make-instance '.z2sersub
                       :a1 '(1.53500182080508 -0.58631852275456)
                       :a2 '(0.9 0.9) 
                       :b0 '(1.0 1.0) :b1 '(0.0 0.0)
                       :b2 '(-1.0 -1.0))))
               (e (.E (.imp1 1.0) 1000.0))
               (out (.probe "out")))
  (.par e zw)
  (-> (.voltage zw) out)
  (defun pr () (float (at (out out)) 1.0)))

(defpatch koe ((zw (.z2ser
                    :a1 '(1.53500182080508 -0.58631852275456)
                    :a2 '(0.9 0.9) 
                    :b0 '(1.0 1.0) :b1 '(0.0 0.0)
                    :b2 '(-1.0 -1.0)))
               (e (.E (.imp1 1.0) 1000.0))
               (out (.probe "out")))
  (.par e zw)
  (-> (.voltage zw) out)
  (defun pr () (float (at (out out)) 1.0)))

;;; Scalar case
(defpatch koe ((zw (make-instance '.zwrapper
                     :subfilt
                     (.z2sub :a1 0.0 :a2 0.9 
                             :b0 1.0 :b1 0.0 :b2 -1.0)))
               (e (.E (.imp1) 1000.0))
               (out (.probe "out")))
  (.par e zw)
  (-> (.voltage zw) out)
  (defun pr () (float (at (out out)) 1.0)))

;;; basic case
(defpatch koe ((zw (.Z2 :a1 -0.3 :a2 0.9 
                        :b0 1.0 :b1 0.0 :b2 -1.0))
               (e (.E (.imp1) 1000.0))
               (out (.probe "out")))
  (.par* e zw)
  (-> (.voltage zw) out)
  (defun pr () (float (at (out out)) 1.0)))

(defpatch koe ((zw (.Y2 :a1 -0.3 :a2 0.9 
                        :b0 1.0 :b1 0.0 :b2 0.0))
               (e (.E (.imp1) 1000.0))
               (out (.probe "out")))
  (.par* e zw)
  (-> (.voltage zw) out)
  (defun pr () (float (at (out out)) 1.0)))

(inspect koe)

(load-patch koe)

(loop with res = nil and sig
      for i from 0 below 400
      do (push (pr) res)
      do (step-patch koe)
      finally 
      (setq res (nreverse res))
      (setq sig (qs::put-list 'qs::signal res))
      (setf (qs::scaler sig) (/ (srate koe)))
      (qs::show sig))

(step-patch-n koe 1000000 t)

(inspect
 (make-instance '.zwrapper
   :sumb0-p t
   :subfilt
   (make-instance '.z2sersub
     :a1 '(0.0 0.0) :a2 '(0.9 0.9) 
     :b0 '(1.0 1.0) :b1 '(0.0 0.0)
     :b2 '(-1.0 -1.0))))
|#


;;; .MODE-MAP3 

(defclass .mode-map3 (macro-group macro-block) ())

(defmethod initialize-macro-block ((b .mode-map3) &key
                                       F Q A)
  (let* ((/srate (/ 1.0d0 (srate b)))
         (pval (.p. :name 'A))
         (pQ (.p. :name 'Q))
         (pfreq (.p. :name 'F))
         (c2 (.datac -2.0d0))
         (c1 (.datac 1.0d0))
         (c05 (.datac 0.5d0))
         (c (.datac (* 2.0d0 (float pi 1.0d0) /srate))))
    (-> pfreq c) (-> pfreq c2)
    (-> pQ c05) (-> pfreq c1)
    (add-param b A (param pval) :name 'A)
    (add-param b Q (param pq) :name 'Q)
    (add-param b F (param pfreq) :name 'F)
    (let* ((omeg (.mul c pfreq))
           (rk (.sub c1 (.mul c05 (.div omeg pQ))))
           (a1 (.mul c2 rk (.cos omeg)))
           (a2 (.sqr rk))
           (b0 (.mul (.sub c1 rk) pval))
           (b2 (.neg b0)))
      (setf (name (out a1)) 'a1
            (name (out a2)) 'a2
            (name (out b0)) 'b0
            (name (out b2)) 'b2)
      (setf (outputs b)
            (list (out b0) (out b2)
                  (out a1) (out a2))))))


;;; .ZFQ-modes Modal impedances in series

(defclass .ZFQ-modes (macro-block one-port-block) ()
  (:default-initargs
    :domain 'electric))

(defmethod initialize-macro-block ((b .ZFQ-modes) &key
                                       Z F Q)
  (let* ((mmap (make-instance '.mode-map3 :F F :Q Q :A Z))
         (sub (make-instance '.z2sersub
                :arg nil :b0 (out mmap 'b0) :b2 (out mmap 'b2)
                :a1 (out mmap 'a1) :a2 (out mmap 'a2)))
         (wrap (make-instance '.zwrapper :subfilt sub)))
    (setf (outputs b) (outputs wrap)
          (inputs b) (inputs wrap)
          (params b) (params mmap)
          (ports b) (ports wrap))))

(defun .ZFQ-modes (&rest rest)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (apply #'make-instance '.ZFQ-modes
           :arg (car rest) (cdr rest))
    (apply #'make-instance '.ZFQ-modes rest)))


;;; .YFQ-modes Modal admittances in parallel

(defun .YFQ-modes (&rest rest &key Y &allow-other-keys)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (.dual (apply #'make-instance '.ZFQ-modes
                  :arg (car rest) (cdr rest) :Z Y))
    (.dual (apply #'make-instance '.ZFQ-modes
                  :Z Y rest))))


#|

(inspect (make-instance '.mode-map3))
(inspect (make-instance '.mode-map3 :f 1000.0))


(defpatch pp ((mp3 (make-instance '.mode-map3)))
  (-> (.var 1000.0) (param mp3 'F))
  (-> (.const 2000.0) (param mp3 'Q))
  (-> (.const 3000.0) (param mp3 'A))
  (-> (out mp3 'b0) (.probe "b0"))
  (-> (out mp3 'a1) (.probe "a1"))
  (-> (out mp3 'a2) (.probe "a2")))

(defpatch pp ((mp3 (make-instance '.mode-map3))
              (p (.var '(1000.0 2000.0))))
  (-> p (param mp3 'F))
  (-> p (param mp3 'Q))
  (-> p (param mp3 'A))
  (-> (out mp3 'b0) (.probe "b0"))
  (-> (out mp3 'a1) (.probe "a1"))
  (-> (out mp3 'a2) (.probe "a2")))

(inspect pp)
(c-code pp)
(load-patch pp)
|#


;;; .Zpoly

(defclass .firpoly (macro-block)
  ((b0p :initform nil :accessor b0p)))

(defmethod initialize-macro-block ((b .firpoly) &key
                                       (dsize 10) coeffs)
  (let* ((f% (make-instance '.fir
               :coeffs coeffs :dsize dsize :cbeg 1))
         (f%% (find '.fir% (block-items f%) :key #'type-of))
         (bs (prev-out (param f%% 'coeffs)))
         (b0 (.select-chan 0))
         (d (.d)))
    (-> bs b0) (-> f% d)
    (setf (b0p b) b0)
    (setf (outputs b) (list (out d))
          (inputs b) (inputs f%)
          (params b) (params f%))))


(defclass .Zpoly (macro-block one-port-block) ()
  (:default-initargs
    :domain 'electric))

(defmethod initialize-macro-block ((b .Zpoly) &key
                                       (dsize 10)
                                       coeffs)
  (let* ((f% (make-instance '.firpoly
               :coeffs coeffs :dsize dsize))
         (wrap (make-instance '.zwrapper :subfilt f%)))
    (setf (outputs b) (outputs wrap)
          (inputs b) (inputs wrap)
          (params b) (params wrap)
          (ports b) (ports wrap))))

(defun .Zpoly (&rest rest)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (apply #'make-instance '.Zpoly :coeffs (car rest) (cdr rest))
    (apply #'make-instance '.Zpoly rest)))

(defun .Ypoly (&rest rest)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (.dual (apply #'make-instance '.Zpoly :coeffs (car rest) (cdr rest)))
    (.dual (apply #'make-instance '.Zpoly rest))))
    
#|
(inspect (make-instance '.firpoly :coeffs '(1.0 2.0 3.0)))
(inspect (make-instance '.firpoly :coeffs (.const '(1.0 2.0 3.0))))

(defpatch koe ((zp (.Zpoly '(1.0 0.0 -1.0)))
               (e (.E (.imp1 1.0) 1000.0))
               (out (.probe "out")))
  (.par e zp)
  (-> (.voltage zp) out)
  (defun pr () (float (at (out out)) 1.0)))

(defpatch koe ((zp (.Zpoly (.const '(1.0 0.0 -1.0))))
               (e (.E (.imp1 1.0) 1000.0))
               (out (.probe "out")))
  (.par e zp)
  (-> (.voltage zp) out)
  (defun pr () (float (at (out out)) 1.0)))

(defpatch koe ((yp (.Ypoly '(1.0 0.0 -1.0)))
               (e (.E (.imp1 1.0) 0.001))
               (out (.probe "out")))
  (.par e yp)
  (-> (.current yp) out)
  (defun pr () (float (at (out out)) 1.0)))
  
(defpatch koe ((zp (.Zpoly (.var '(1.0 -1.0 0.0))))
               (e (.E (.imp1 1.0) 1000.0))
               (out (.probe "out")))
  (.par e zp)
  (-> (.voltage zp) out)
  (defun pr () (float (at (out out)) 1.0)))

(matlab-response koe ;;; To Matlab
                 :samples 4000
                 :outputs '("out")
                 :post "
figure(10); clf; subplot(2,1,1);
plot_sig(out_resp,SRATE); grid on; 
xlabel('Time [s]');
ylabel('Amplitude');
subplot(2,1,2);
plot_spect(out_resp,SRATE); grid on;
")
|#



#|
(defclass .iirpoly (macro-block)
  ((b0p :initform nil :accessor b0p)))

(defmethod initialize-macro-block ((b .iirpoly) &key
                                       as bs)
  (let* ((ndel (make-instance '.ndelay+))
         (bdots (make-instance '.dot+1))
         (adots (make-instance '.dot+1))
         (pa (.p. :name 'as))
         (pb (.p. :name 'bs))
         (a0 (.select-chan 0))
         (b0 (.select-chan 0))
         (mula (.mul))
         (mulb (.mul))
         (sub1 (.sub))
         (subo (.sub)))
    (add-param b bs (param pb))
    (add-param b as (param pa))
    (-> pa a0 (in mula 1))
    (-> pb b0 (in mulb 1))
    (-> pa (param ndel 0))
    (-> pb (param ndel 1))
    (-> pa (param adots))
    (-> pb (param bdots))
    (-> ndel bdots)
    (-> ndel adots (in sub1 1))
    (-> adots mulb (in subo 1))
    (-> sub1 mula (in ndel))
    (-> bdots subo)
    (setf (outputs b) (list (out subo))
          (inputs b) (list (in sub1)))))
|#

;;; .Zratio

(defclass .iirpoly% (macro-block) ())

(defmethod initialize-macro-block ((b .iirpoly%) &key
                                       (dsize 10) as bs)
  (let* ((f% (make-instance '.iir
               :as as :bs bs :dsize dsize))
         (d (.d)))
    (-> f% d)
    (setf (outputs b) (list (out d))
          (inputs b) (inputs f%)
          (params b) (params f%))))
    
#|
; (inspect (make-instance '.iirpoly))

(defpatch koe ((ip (make-instance '.iirpoly
                     :bs (.pack (.var 1.0) (.var 2.0) (.var 3.0))
                     :as '(1.0 0.5 0.2 0.1))))
  (-> (.var 1.0) ip (.probe "out")))

(defpatch koe ((ip (make-instance '.iirpoly
                     :bs '(1.0 1.0 1.0 1.0)
                     :as '(1.0 0.5 0.2 0.1))))
  (-> (.var 1.0) ip (.probe "out")))

(c-code koe)
(load-patch koe)
(inspect koe)
|#


(defclass .Zratio% (macro-block one-port-block) ()
  (:default-initargs
    :domain 'electric))

(defmethod initialize-macro-block ((b .Zratio%) &key
                                       (dsize 10)
                                       as bs rp)
  (let* ((f% (make-instance '.iirpoly%
               :as as :bs bs :dsize dsize))
         (wrap (make-instance '.zwrapper
                 :subfilt f% :rp rp)))
    (setf (outputs b) (outputs wrap)
          (inputs b) (inputs wrap)
          (params b) (params wrap)
          (ports b) (ports wrap))))

(defun .Zratio (&rest rest)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (apply #'make-instance '.Zratio% :coeffs (car rest) (cdr rest))
    (apply #'make-instance '.Zratio% rest)))

(defun .Yratio (&rest rest)
  (declare (dynamic-extent rest))
  (if (and rest (not (keywordp (car rest))))
    (.dual (apply #'make-instance '.Zratio% :coeffs (car rest) (cdr rest)))
    (.dual (apply #'make-instance '.Zratio% rest))))

#|
(defpatch koe ((z (.Zratio :as '(2.0 1.0 0.5)
                           :bs '(1.0 2.0 1.0)
                           :rp (.const 1.0))))
  (.par (.R 1.0) z))
(load-patch koe)
(inspect koe)
|#


;;; ------

(defclass .R1+R2//C (macro-block) ())

(defmethod initialize-macro-block ((b .R1+R2//C) &key
                                       R1 R2 C)
  (let* ((R1p (.p. :name 'R1))
         (R2p (.p. :name 'R2))
         (Cp (.p. :name 'C))
         (/fs2 (/ 0.5d0 (srate b)))
         (RCp (.div /fs2 Cp))
         (R1R2 (.mul R1p R2p))
         (R1RC (.mul R1p RCp))
         (R2RC (.mul R2p RCp))
         (b0 (.add R1R2 R1RC R2RC))
         (b1 (.add R1RC R2RC (.neg R1R2)))
         (a0 (.add RCp R2p))
         (a1 (.sub RCp R2p)))
    (add-param b C (param Cp))
    (add-param b R2 (param R2p))
    (add-param b R1 (param R1p))
    (setf (name (out b0)) 'b0
          (name (out b1)) 'b1
          (name (out a0)) 'a0
          (name (out a1)) 'a1)
    (setf (outputs b)
          (list (out b0) (out b1)
                (out a0) (out a1)))))


#|
(inspect (make-instance '.R1+R2//C))
|#


#|
I THINK THESE ARE NOT REALLY NEEDED 

(defclass .Zser (macro-block) ()
  (:default-initargs
    :domain 'electric))

(defclass .Zpar (macro-block) ()
  (:default-initargs
    :domain 'electric))
|#

#|


;;; OTHER ACOUSTIC COMPONENTS

;;; Acoustic radiation impedance (simple, constant parameters)

(defclass .Za-rad (macro-block)
  ((area :initform nil :reader area)
   (rho :initarg :rho :reader rho)
   (c :initarg :c :reader c))
  (:default-initargs
    :rho *rho*
    :c *c*))

(defmethod initialize-macro-block ((b .Za-rad) &key 
                                       (end-factor 0.85d0)
                                       diameter
                                       radius
                                       area)
  (cond (area t)
        (diameter (setq area (* 1/4 (cl:float pi 1.0d0) diameter diameter)))
        (radius (setq area (* pi diameter diameter))))
  (setf (slot-value b 'area) area)
  (let* ((R (.Ra (/ (* (rho b) (c b)) area)))
         (L (.La (/ end-factor (rho b) (sqrt (* (cl:float pi 1.0d0) area)))))
         (port (.par R L)))
    (setf (ports b) (list port))))

(defun .Za-rad (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.Za-rad rest))

#|
(inspect (.Za-rad :diameter 0.007 :end-factor 0.6))
|#


;;; Acoustic tube ??????????????????????

(defclass .tube (macro-block)
  ((area :initform nil :reader area)
   (rho :initarg :rho :reader rho)
   (c :initarg :c :reader c)
   (delay :initform nil :reader delay))
  (:default-initargs
    :rho *rho*
    :c *c*))

#|
(defmethod initialize-macro-block ((b .tube) &key 
                                       (end-factor 0.85d0)
                                       diameter radius area
                                       length delay
                                       &aux (rate (srate b)))
  (cond (area t)
        (diameter (setq area (* 1/4 (cl:float pi 1.0d0) diameter diameter)))
        (radius (setq area (* pi diameter diameter))))
  (setf (slot-value b 'area) area)
  (cond (length t)
        (delay (setq length 
  
|#

#|
(def-macro-block .ear_canal (&key (length 25.0d-3)
                                    (diam 7d-3)
                                    (c 343.0d0) (rho 1.20d0)
                                    (area (* 1/4 pi diam diam))
                                    (report-p nil))
  (let* ((rate (srate block))
         (dlen (round (* rate (/ length c))))
         (length* (/ (* dlen c) rate))
         (z (/ (* rho c) area))
         (tube (.dline-n z :delay-length dlen :domain 'acoustic)))
    (when report-p
      (format t "Given length: ~a, used length: ~a, error: ~a \%"
              length length* (* 100.0 (- (/ length* length) 1.0d0))))
    (set-ports (port tube 0) (port tube 1))))
|#







;;; .bq23% biquad with FIR-part taps 2&3 only, delay-mixin
;;; 1- or 2-dim params, separate scalars/vectors

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


;;; .bq% (biquad skeleton)
;;; 1- or 2-dim params, scalars/vectors
;;; mode: normal/par/ser/modal/modpar/modser

(defclass .bq% (basic-block)
  ((mode :initarg :mode :accessor mode)
   (dim :initform nil :accessor dim))
  (:default-initargs
    :mode 'normal))

(defmethod initialize-instance :after ((b .bq%) &key)
  (setf (params b)
        (list (make-instance 'param :host-block b :name 'a1)
              (make-instance 'param :host-block b :name 'a2)
              (make-instance 'param :host-block b :name 'b0)
              (make-instance 'param :host-block b :name 'b1)
              (make-instance 'param :host-block b :name 'b2))
        (outputs b)
        (list (make-instance 'output :host-block b :name 'sig)
              (make-instance 'output :host-block b :name 'out0)
              (make-instance 'output :host-block b :name 'out1))
        (inputs b)
        (list (make-instance 'input :host-block b :name 'sig)
              (make-instance 'input :host-block b :name 'in1)
              (make-instance 'input :host-block b :name 'in2))))

; (inspect (make-instance '.bq%))
; (inspect (make-instance '.bq% :mode 'normal))

(defmethod finalize-sizes :after ((b .bq%))
  (cond ((member (mode b) '(par ser modpar modser))
         (unless (equal (datasize (prev-out (in b 'sig))) '(1))
           (error "Series or parallel biquad ~a not with scalar input" b)))
        ((member (mode b) '(normal modal))
         (unless (equal (datasize (prev-out (in b 'sig)))
                        (datasize (prev-out (param b 'b0))))
           (error "Data dimension error in biquad ~a" b)))
        (t (error "Invalid mode spec in biquad ~a" b)))
  (setf (dim b) (first (datasize (prev-out (param b 'b0)))))
  nil)

(defmethod set-size :after ((b .bq%) size)
  (setf (datasize (out b 0))
        (datasize (prev-out (in b 'sig)))))


(defmethod c-code ((b .bq%) &optional (stream t))
  (let* ((?m (member (mode b) '(modal modpar modser)))
         (?1 (not (member (mode b) '(normal modal))))
         (a1 (nref (param b 'a1) 'RL_0))
         (a2 (nref (param b 'a2) 'RL_0))
         (b0 (nref (param b 'b0) 'RL_0))
         (b1 (nref (param b 'b1) 'RL_0))
         (b2 (nref (param b 'b2) 'RL_0))
         (in (if ?1 (cref (in b 'sig) )
                 (nref (in b 'sig) 'RL_0)))
         (in1 (nref (in b 'in1) 'RL_0))
         (in2 (nref (in b 'in2) 'RL_0))
         (out (nref (out b 'sig) 'RL_0))
         (out0 (nref (out b 'out0) 'RL_0))
         (out1 (nref (out b 'out1) 'RL_0))
         (siz (dim b)))
    (when (> siz 1)
      (when (member (mode b) '(ser modser))
        (with-c stream "|out| = |in|;"))
      (when (member (mode b) '(par modpar))
        (with-c stream "|out| = 0.0;"))
      (with-c stream
        "for (RL_0=0; RL_0<|siz|; RL_0++){"))
    (with-c stream
      "RD_1 = |in1|; RD_2 = |in2|;")
    (cond ((and ?m (eq (mode b) 'modser))
           (with-c stream
             "RD_0 = RD_1*|a1| + RD_2*|a2|;"
             "|out0| = |out| - RD_0; |out1| = RD_1;"
             "|out| = RD_1*|b1| + RD_2*|b2| - RL_0*|b0|;"))
          ((and ?m (eq (mode b) 'modpar))
           (with-c stream
             "RD_0 = RD_1*|a1| + RD_2*|a2|;"
             "|out0| = |in| - RD_0; |out1| = RD_1;"
             "|out| += RD_1*|b1| + RD_2*|b2| - RL_0*|b0|;"))
          (?m (with-c stream
                "RD_0 = RD_1*|a1| + RD_2*|a2|;"
                "|out0| = |in| - RD_0; |out1| = RD_1;"
                "|out| = RD_1*|b1| + RD_2*|b2| - RL_0*|b0|;"))
          ((and (not ?m) (eq (mode b) 'ser))
           (with-c stream
             "RD_0 = |out| - RD_1*|a1| - RD_2*|a2|;"
             "|out0| = RD_0; |out1| = RD_1;"
             "|out| = RD_1*|b1| + RD_2*|b2| + RD_0*|b0|;"))
          ((and (not ?m) (eq (mode b) 'par))
           (with-c stream
             "RD_0 = |in| - RD_1*|a1| - RD_2*|a2|;"
             "|out0| = RD_0; |out1| = RD_1;"
             "|out| += RD_1*|b1| + RD_2*|b2| + RD_0*|b0|;"))
          ((not ?m)
           (with-c stream
             "RD_0 = |in| - RD_1*|a1| - RD_2*|a2|;"
             "|out0| = RD_0; |out1| = RD_1;"
             "|out| = RD_1*|b1| + RD_2*|b2| - RL_0*|b0|;")))
    (if (> siz 1) (format stream "}"))
    nil))


(defmethod lcode ((b .bq%))
  (let* ((?m (member (mode b) '(modal modpar modser)))
         (?1 (not (member (mode b) '(normal modal))))
         (b0 (lvar (prev-out (param b 'b0))))
         (b1 (lvar (prev-out (param b 'b1))))
         (b2 (lvar (prev-out (param b 'b2))))
         (a1 (lvar (prev-out (param b 'a1))))
         (a2 (lvar (prev-out (param b 'a2))))
         (in (lvar (prev-out (in b 'sig))))
         (in1 (lvar (prev-out (in b 'in1))))
         (in2 (lvar (prev-out (in b 'in2))))
         (out (lvar (out b 'sig)))
         (out0 (lvar (out b 'out0)))
         (out1 (lvar (out b 'out1)))
         (indi (if ?1 0 'i))
         (indo (if ?1 0 'i))
         (siz (dim b)))
    `(loop initially (setf (aref ,out 0) 0.0d0)
           for i from 0 below ,siz
           for temp = (+ (* (aref ,in1 i) (aref ,a1 i))
                         (* (aref ,in2 i) (aref ,a2 i)))
           do (setf (aref ,out1 i) (aref ,in1 i)
                    (aref ,out0 i) (- (aref ,in ,indi) temp))
           do (if ,?m
                (setf (aref ,out ,indo)
                      (+ (* (aref ,in1 i) (aref ,b1 i))
                         (* (aref ,in2 i) (aref ,b2 i))
                         (- (* temp (aref ,b0 i)))))
                (setf (aref ,out ,indo)
                      (+ (* (aref ,in1 i) (aref ,b1 i))
                         (* (aref ,in2 i) (aref ,b2 i))
                         (* (aref ,out0 i) (aref ,b0 i))))))))


(defmethod mcode ((b .bq%) &optional (stream t)) ;;; ???
  (let* ((onam (mname (out b)))
         (innam (mname (in b)))
         (a1 (mname (param b 'a1)))
         (a2 (mname (param b 'a2)))
         (b0 (mname (param b 'b0)))
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


;;; .bq (regular biquad filter)
;;; with coeffs a1, a2, b0, b1, b2
;;; can be scalars or vectors of same size

(defclass .bq (macro-block) ())

(defmethod initialize-macro-block ((b .bq) &key mode
                                       a1 a2 b0 b1 b2)
  (let* ((bq (make-instance '.bq% :mode mode)))
    (setf (inputs b) (list (in bq 'sig))
          (outputs b) (list (out bq 'sig)))
    (-> (out bq 'out0) (.d) (in bq 'in1))
    (-> (out bq 'out1) (.d) (in bq 'in2))
    (add-param b b2 (param bq 'b2) :name 'b2)
    (add-param b b1 (param bq 'b1) :name 'b1)
    (add-param b b0 (param bq 'b0) :name 'b0)
    (add-param b a2 (param bq 'a2) :name 'a2)
    (add-param b a1 (param bq 'a1) :name 'a1)))

(defun .bq (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.bq :mode 'normal rest))

(defun .bq-ser (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.bq :mode 'ser rest))
        
(defun .bq-par (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.bq :mode 'par rest))

|#



; (inspect (.bq))

#|
(defpatch koe ((bq (make-instance '.bq%))
               (out (.probe "out")))
  (-> (.var 0.5) (param bq 'a1))
  (-> (.var 0.25) (param bq 'a2))
  (-> (.var 1.2) (param bq 'b0))
  (-> (.var 1.0) (param bq 'b1))
  (-> (.var 0.7) (param bq 'b2))
  (-> (.imp1) (in bq 'sig))
  (-> (out bq 'out0) (.d) (in bq 'in1))
  (-> (out bq 'out1) (.d) (in bq 'in2))
  (-> (out bq 'sig) out)
  (defun pr () (print (value (out out)))))

(defpatch koe ((bq (.bq))
               (out (.probe "out")))
  (-> (.var 0.5) (param bq 'a1))
  (-> (.var 0.25) (param bq 'a2))
  (-> (.var 1.2) (param bq 'b0))
  (-> (.var 1.0) (param bq 'b1))
  (-> (.var 0.7) (param bq 'b2))
  (-> (.imp1) (in bq 'sig))
  (-> (out bq 'sig) out)
  (defun pr () (print (value (out out)))))

(defpatch koe ((bq (.bq))
               (out (.probe "out")))
  (-> (.var '(0.5 0.5)) (param bq 'a1))
  (-> (.var '(0.25 0.25)) (param bq 'a2))
  (-> (.var '(1.2 1.2)) (param bq 'b0))
  (-> (.var '(1.0 1.0)) (param bq 'b1))
  (-> (.var '(0.7 0.7)) (param bq 'b2))
  (-> (.imp1 '(1.0 1.0)) (in bq 'sig))
  (-> (out bq 'sig) out)
  (defun pr () (print (value (out out)))))

(defpatch koe ((bq (.bq :a1 0.5 :a2 0.25 :b0 1.2 :b1 1.0 :b2 0.7))
               (out (.probe "out")))
  (-> (.imp1) (in bq 'sig))
  (-> (out bq 'sig) out)
  (defun pr () (print (value (out out)))))

(inspect koe)
(c-code koe)
(load-patch koe)

(loop initially (pr)
      for i from 0 below 10
      do (step-patch koe)
      do (pr))

(c-code (elt (block-items koe) 5))
(nref (in (elt (block-items koe) 5) 'sig) 'RL_0)
(nref (out (elt (block-items koe) 5) 'sig) 'RL_0)
(nref (param (elt (block-items koe) 5) 'a1) 'RL_0)

(defpatch koe ((bq (.bq-ser))
               (out (.probe "out")))
  (-> (.var '(0.5 0.5)) (param bq 'a1))
  (-> (.var '(0.25 0.25)) (param bq 'a2))
  (-> (.var '(1.2 1.2)) (param bq 'b0))
  (-> (.var '(1.0 1.0)) (param bq 'b1))
  (-> (.var '(0.7 0.7)) (param bq 'b2))
  (-> (.imp1 1.0) (in bq 'sig))
  (-> (out bq 'sig) out)
  (defun pr () (print (value (out out)))))
|#

;;;; =======================


(provide :BC-immit)
