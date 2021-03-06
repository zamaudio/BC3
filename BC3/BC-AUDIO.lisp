
(in-package :bc)


;;; AUDIO & PA-SERVER

(defparameter *audio* nil)
  
(defmethod get-patchlink ((pa .pa_server) n)
  (unless (< -1 n (patchcount pa))
    (error "Invalid patchlink index ~a" n))
  (get_link n))

#|
(defparameter pas (make-instance '.pa_server))
(inspect pas)
(get-patchlink pas 0)
(set-patchlink pas 33)
(release-patchlink pas 33)
(patchlinks-p pas)
|#

(defmethod set-patchlink ((block .pa_server) address)
  (let* ((siz (patchcount block)))
    (loop for i from 0 below siz
          for x = (get-patchlink block i)
          do (when (= x 0)
               (set_link i address)
               (return i))
          finally (error "AUDIO linklist full")))
  address)

(defmethod release-patchlink ((block .pa_server) address)
  (let* ((siz (patchcount block)))
    (loop for i from 0 below siz
          for x = (get-patchlink block i)
          do (when (= x address)
               (set_link i 0) (return i))
          finally (error "Address not in AUDIO linklist"))))

(defmethod remove-patchlink ((block .pa_server) index)
  (get-patchlink block index)
  (set_link index 0))

(defmethod patchlinks-p ((block .pa_server))
  (loop with siz = (patchcount block)
        for i from 0 below siz
        do (when (/= (get-patchlink block i) 0)
             (return-from patchlinks-p t))))

(defmethod pa-open-stream ((pa .pa_server))
  (unless (zerop (pa_Open (cl::float (srate pa) 0.0d0)))
    (error "Cannot open pa-audio stream"))
  (setf (state pa) :loaded)
  t)

;;; (pa-open-stream pas)

(defmethod pa-start-stream ((pa .pa_server))
  (unless (zerop (pa_Start))
    (error "Cannot start pa-audio client"))
  (setf (state pa) :running)
  t)

;;; (pa-start-stream pas)

(defmethod pa-stop-stream ((pa .pa_server))
  (unless (zerop (pa_Stop))
    (error "Cannot stop pa-audio client"))
  (setf (state pa) :loaded)
  t)

;;; (pa-stop-stream pas)

(defmethod pa-close-stream ((pa .pa_server))
  (unless (zerop (pa_Close))
    (error "Cannot close pa-audio client"))
  (setf (state pa) nil)
  t)

;;; (pa-close-stream pas)

(defmethod start-server ((pa .pa_server) &optional srate)
  (when srate (setf (srate pa) (cl::float srate 0.0d0)))
  (when (eq (state pa) nil) (pa-open-stream pa))
  (cond ((eq (state pa) :running)
         (return-from start-server nil))
        ((eq (state pa) :loaded) (pa-start-stream pa))
        (t (error "Cannot start audio server")))
  t)
  
;;; (start-server pas)

(defmethod stop-server ((pa .pa_server))
  (cond ((eq (state pa) :running)
         (pa-stop-stream pa))
        (t nil))
  t)

;;; (stop-server pas)


;;; AUDIO PATCH

(defun make-audio ()
  (progn
    (setq *audio* (make-instance '.pa_server))
    (when *AUDIO-SERVER-ALWAYS-ON* (start-server *audio*))
    *audio*))

;;; (inspect *AUDIO*)
;;; (make-audio)
;;; (start-server *AUDIO*)
;;; (stop-server *AUDIO*)

(defun start-audio (&optional srate)
  (unless *AUDIO* (make-audio))
  (start-server *AUDIO* srate))

(defun stop-audio () (stop-server *AUDIO*))

(defun restart-audio (&optional srate)
  (let ((audio *AUDIO*))
    (unless audio
      (error "No audio available for restarting"))
    (when (eq (state audio) :running)
      (stop-server audio))
    (when (eq (state audio) :loaded)
      (pa-close-stream audio))
    (start-server audio srate)))


;;; DA-CONVERTER

(defclass .da (basic-block)
  ((channels :initarg :channels :accessor channels)
   (scaler :initarg :scaler :accessor scaler)
   (server :initarg :server :accessor server))
  (:default-initargs
    :server nil
    :scaler nil
    :outputs nil
    :channels 2))

(defmethod initialize-instance :after ((block .da) &key)
  (unless *AUDIO* (error "No audio server for ~a" block))
  (let* ((dasiz 16))
    (unless (>= dasiz (channels block))
      (error "Not enough channels in audio server for ~a" block))
    (setf (inputs block)
          (loop for i from 0 below (channels block)
                for in = (make-instance 'input :host-block block)
                do (cond ((= i 0) (setf (name in) 'left))
                         ((= i 1) (setf (name in) 'right)))
                collect in)))
  (setf (server block) *AUDIO*)
  nil)

(defmethod get-variables ((b .da))
  (setf (var-p b) t)
  (get-outvars b))

(def.cfun make-bufref ((b .da) &optional (stream t))
  (let* ((addr (get_da)))
    (format stream "double *DAV = ~a;~%" addr)))

(defmethod c-code ((block .da) &optional (stream t))
  (let* ((ins (inputs block))
         (in# (length ins))
         (sc (if (scaler block)
               (format nil "~a*" (scaler block)) "")))
    (loop for i from 0 below in#
          for in = (cref (elt ins i))
          do (with-c stream
               "DAV[|i|] += |sc||in|;"))))

(defun .da (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.da rest))

(defun .da* (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.da
         :scaler (/ 1.0d0 32768.0d0) rest))


;;; AD-CONVERTER

(defclass .ad (basic-block)
  ((channels :initarg :channels :accessor channels)
   (server :initarg :server :accessor server))
  (:default-initargs
    :server nil
    :var-p t
    :channels 2))

(defmethod initialize-instance :after ((block .ad) &key)
  (unless *AUDIO* (error "No audio server for ~a" block))
  (let* ((adsiz 16))
    (unless (>= adsiz (channels block))
      (error "Not enough channels in audio server for ~a" block))
    (setf (outputs block)
          (loop for i from 0 below (channels block)
                for out = (make-instance 'output :host-block block
                                         :type '.double)
                do (cond ((= i 0) (setf (name out) 'left))
                         ((= i 1) (setf (name out) 'right)))
                collect out)))
  (setf (server block) *AUDIO*)
  nil)

(def.cfun make-bufref ((b .ad) &optional (stream t))
  (let* ((addr (get_ad)))
    (format stream "double *ADV = ~a;~%" addr)))

(defmethod c-code* :before ((b .ad) &optional (stream t))
  stream (setf (var-p b) t))

(defmethod c-code ((block .ad) &optional (stream t))
  (let* ((outs (outputs block))
         (out# (length outs)))
    (loop for i from 0 below out#
          for out = (cref (elt outs i))
          do (with-c stream "|out| = ADV[|i|];"))))

(defun .ad (&rest rest)
  (declare (dynamic-extent rest))
  (apply #'make-instance '.ad rest))


;;; PATCH RUN CONTROL

(defmethod run-patch ((p patch)) ;;; srate control ???
  (unless (and (boundp '*AUDIO*))
    (error "No audio installed, cannot run patch"))
  (when (eq (state p) :running)
    (return-from run-patch nil))
  (unless (eq (state p) :loaded) (load-patch p))
  (let* ((paddr (patchaddr (runtime p)))
         (audio *AUDIO*))
    (when (symbolp paddr) (setq paddr (funcall paddr)))
    (unless (member (state audio) '(:loaded :running))
      (pa-open-stream audio))
    (cond ((and (eq (state audio) :running)
                (patchlinks-p audio)
                (/= (srate p) (srate audio)))
           (error "Audio reserverd for another sample rate"))
          ((and (eq (state audio) :running)
                (not (patchlinks-p audio))
                (/= (srate p) (srate audio)))
           (restart-audio (srate p)))
          ((and (eq (state audio) :loaded)
                (/= (srate p) (srate audio)))
           (restart-audio (srate p)))
          ((eq (state audio) :loaded) (start-audio))
          (t nil))
    (set-patchlink audio paddr)
    (setf (state p) :running)
    nil))

(defmethod stop-patch ((p patch))
  (when (eq (state p) :running)
    (let* ((paddr (patchaddr (runtime p)))
           (audio *AUDIO*))
      (when (symbolp paddr) (setq paddr (funcall paddr)))
      (release-patchlink audio paddr)
      (setf (state p) :loaded)
      (when (and (not (patchlinks-p audio))
                 (not *AUDIO-SERVER-ALWAYS-ON*))
        (stop-audio))))
  nil)


(provide :BC-audio)

