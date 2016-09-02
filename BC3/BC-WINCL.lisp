
(in-package :bc)


;;;CLISP for Windows

;;; (defparameter *BC-lib* nil)
;;; (defparameter *BC-patch-dir* "patches")

(defclass .pa_server () ;;; (basic-block)
  ((pa_device :initform nil :accessor pa_device)
   (bufsize :initarg :bufsize :accessor bufsize)
   (maxin# :initform nil :accessor maxin#)
   (maxout# :initform nil :accessor maxout#)
   (srate :initarg :srate :accessor srate)
   (state :initform nil :accessor state)
   (patchcount :initarg :patchcount :accessor patchcount))
  (:default-initargs
    :srate *srate*
    :bufsize 128
    :patchcount 32))

(defmethod initialize-instance :after ((pa .pa_server) &key)
  (let* ()
    (setf (pa_device pa) nil
          (maxin# pa) 2
          (maxout# pa) 2)
    (pa_Init (bufsize pa) (maxin# pa) (maxout# pa))))


(defclass pa_deviceinfo ()
  ((structversion :reader structversion)
   (name :reader name)
   (maxInputChannels :reader maxInputChannels)
   (maxOutputChannels :reader maxOutputChannels)
   (samplerates :reader samplerates)))

(defclass pa_device ()
  ((name :initarg :name :accessor name)
   (indev :initform nil :accessor indev)
   (inblock :initform nil :accessor inblock)
   (outdev :initform nil :accessor outdev)
   (outblock :initform nil :accessor outblock)))

(ffi:def-c-struct pa-deviceinfo
  (structversion ffi:int)
  (name ffi:c-string)
  (hostapi ffi:int)
  (maxInputChannels ffi:int)
  (maxOutputChannels ffi:int)
  (x ffi:double-float)
  (y ffi:double-float)
  (z ffi:double-float)
  (w ffi:double-float)
  (srate ffi:double-float))

(defun get_pa_devices ()
  (let* ((devicecount (pa_countdevices)))
    (loop for i from 0 below devicecount
          for devptr = (pa_getdeviceinfo i)
          for structversion = (slot-value devptr 'structversion)
          for name = (slot-value devptr 'name)
          for maxinputs = (slot-value devptr 'maxInputChannels)
          for maxoutputs = (slot-value devptr 'maxOutputChannels)
          for sratecount = 1
          for srate = (slot-value devptr 'srate)
          for info = (make-instance 'pa_deviceinfo)
          do (setf (slot-value info 'structversion) structversion)
          do (setf (slot-value info 'name) name)
          do (setf (slot-value info 'maxInputChannels) maxinputs)
          do (setf (slot-value info 'maxOutputChannels) maxoutputs)
          do (setf (slot-value info 'samplerates) (list srate))
          collect info)))

(defun parse_pa_devs (devinfos)
  (let (devs dx)
    (loop for dinfo in devinfos
          for n = (name dinfo)
          for in# = (maxinputchannels dinfo)
          for out# = (maxoutputchannels dinfo)
          do (setq dx nil)
          do (cond ((and (> in# 0) (> out# 0))
                    ;;; if name there already => error
                    (setq dx (make-instance 'pa_device))
                    (setf (indev dx) dinfo)
                    (setf (outdev dx) dinfo)
                    (setf (name dx) n)
                    (push dx devs))
                   ((> in# 0)
                    (setq dx (find dinfo devs 
                                   :test #'(lambda (x y) (string-equal (name x) y))
                                   :key #'name))
                    (when dx (setf (indev dx) dinfo))
                    (unless dx 
                      (setq dx (make-instance 'pa_device :name n))
                      (push dx devs))
                    (setf (indev dx) dinfo))
                   ((> out# 0)
                    (setq dx (find dinfo devs 
                                   :test #'(lambda (x y) (string-equal (name x) y))
                                   :key #'name))
                    (when dx (setf (outdev dx) dinfo))
                    (unless dx 
                      (setq dx (make-instance 'pa_device :name n))
                      (push dx devs)))))
    (nreverse devs)))

(defun updated_pa_devs ()
  (pa_termin) (pa_termin)
  (parse_pa_devs (get_pa_devices)))

(defparameter *pa_devices*  nil)

(defun pa_devices ()
  (unless *pa_devices*
    (pa_initialize)
    (pa_termin)
    (pa_initialize)
    (setq *pa_devices*
          (parse_pa_devs
           (get_pa_devices))))
  *pa_devices*)


(ffi:def-call-out Pa_Initialize
   (:name "Pa_Initialize")
   (:arguments)
   (:language :stdc)
   (:library "portaudio_x86.dll")
   (:return-type ffi:int))

(ffi:def-call-out Pa_GetDeviceInfo
   (:name "Pa_GetDeviceInfo")
   (:arguments (n ffi:int))
   (:language :stdc)
   (:library "portaudio_x86.dll")
   (:return-type (ffi:c-ptr pa-deviceinfo)))

(ffi:def-call-out Pa_GetDefaultInputDeviceID
   (:name "Pa_GetDefaultInputDevice")
   (:arguments)
   (:language :stdc)
   (:library "portaudio_x86.dll")
   (:return-type ffi:int))

(ffi:def-call-out Pa_CountDevices
   (:name "Pa_GetDeviceCount")
   (:arguments)
   (:language :stdc)
   (:library "portaudio_x86.dll")
   (:return-type ffi:int))

(ffi:def-call-out Pa_Init
   (:name "pa_Init")
   (:arguments
     (nframes ffi:long)
     (adsize ffi:long)
     (dasize ffi:long))
   (:language :stdc)
   (:library "bc3.dll")
   (:return-type ffi:int))

(ffi:def-call-out Pa_Open
   (:name "pa_OpenStream")
   (:arguments
     (srate ffi:double-float))
   (:language :stdc)
   (:library "bc3.dll")
   (:return-type ffi:int))

(ffi:def-call-out Pa_Start
   (:name "pa_StartStream")
   (:arguments)
   (:language :stdc)
   (:library "bc3.dll")
   (:return-type ffi:int))

(ffi:def-call-out Pa_Stop
   (:name "pa_StopStream")
   (:arguments)
   (:language :stdc)
   (:library "bc3.dll")
   (:return-type ffi:int))

(ffi:def-call-out Pa_Close
   (:name "pa_CloseStream")
   (:arguments)
   (:language :stdc)
   (:library "bc3.dll")
   (:return-type ffi:int))

(ffi:def-call-out Pa_Termin
   (:name "pa_Terminate")
   (:arguments)
   (:language :stdc)
   (:library "bc3.dll")
   (:return-type ffi:int))

(ffi:def-call-out get_link
   (:name "get_link")
   (:arguments (n ffi:long))
   (:language :stdc)
   (:library "bc3.dll")
   (:return-type ffi:int))

(ffi:def-call-out set_link
   (:name "set_link")
   (:arguments
     (n ffi:long)
     (addr ffi:long))
   (:language :stdc)
   (:library "bc3.dll")
   (:return-type ffi:int))

(ffi:def-call-out get_ad
   (:name "get_ad")
   (:arguments)
   (:language :stdc)
   (:library "bc3.dll")
   (:return-type ffi:long))

(ffi:def-call-out get_da
   (:name "get_da")
   (:arguments)
   (:language :stdc)
   (:library "bc3.dll")
   (:return-type ffi:long))

#|
;;; (pa_Init 256 2 2)
;;; (pa_Open 44100.0d0)
;;; (pa_Start)
;;; (pa_Stop)
;;; (pa_Close)
;;; (pa_Termin)
|#

(defun %syscall (string)
  (ext:shell string))

(defparameter *BC-patch-dir* "BC3patches/")

(defun write-patch% (p)
  (let* ((dirpath *BC-patch-dir*)
         (name (patchname p))
         (dst (format nil "~a~a/~a.c" dirpath name name)))
    (when (probe-file dst) (delete-file dst))
    (ensure-directories-exist dst)
    (with-open-file
      (str dst :direction :output
           :if-exists :overwrite
           :if-does-not-exist :create)
      (funcall 'c-code p str))))

(defun bc-compile% (p &optional (dir *BC-patch-dir*))
  (let* ((file (patchname p))
         (dirpath dir)
         (src (format nil "~a~a/~a.c" dirpath file file))
         (dst (format nil "~a~a/~a" dirpath file file))
         (str (format nil "~a -w -O3 -shared ~a -o ~a.dll" *MinGWgcc* src dst)))
    (unless (%syscall str)
      (print str t)
      (error "Cannot compile patch ~a" p))
    nil))

(defun link-main% (p) p nil)

(defparameter *type-mapper*
  '((.char . ffi:char)
    (.short . ffi:short)
    (.long . ffi:long)
    (.float . ffi:single-float)
    (.double . ffi:double-float)))

(defun type.map (var)
  (cdr (assoc (datatype var) *type-mapper*)))

#|
(defmethod c-address ((v terminal))
  (let* ((b (host-block v))
         (p (find-patch b))
         (r (runtime p)))
    (fli:pointer-address
     (fli:make-pointer :symbol-name (string (varname v))
                       :module (patchname p)))))
|#

(defmethod reader-access ((b t)) nil)

(defmethod writer-access ((b t)) nil)

(defmethod reader-access ((b terminal))
  (when (creader b)
    (let* ((dim (datasize b))
           (type (type.map b))
           (name (varname b))
           (rname (format nil "get_~a" name))
           (funame (gensym))
           (p (find-patch (host-block b)))
	   (lname (bundle (runtime p)))
           (pname (string (patchname p)))
           (args (cond ((equal dim '(1)) nil)
                       ((= (length dim) 1) '((x ffi:long)))
                       ((= (length dim) 2)
                        '((x ffi:long) (y ffi:long))))))
      (when (var-p b)
        (eval
         `(ffi:def-call-out ,funame 
            (:name ,rname)
            (:arguments ,@args)
	    (:language :stdc)
            (:library ,lname)
	    (:return-type ,type)))
        (setf (creader b) funame)))))

(defmethod writer-access ((b terminal))
  (when (cwriter b)
    (let* ((dim (datasize b))
           (type (type.map b))
           (name (varname b))
           (rname (format nil "set_~a" name))
           (funame (gensym))
           (p (find-patch (host-block b)))
	   (lname (bundle (runtime p)))
           (pname (string (patchname p)))
           (args (cond ((equal dim '(1)) nil)
                       ((= (length dim) 1) '((x ffi:long)))
                       ((= (length dim) 2)
                        '((x ffi:long) (y ffi:long))))))
      (push (list 'val type) args)
      (when (var-p b)
        (eval
	  `(ffi:def-call-out ,funame 
            (:name ,rname)
            (:arguments ,@args)
	    (:language :stdc)
            (:library ,lname)
	    (:return-type nil)))
        (setf (cwriter b) funame)))))

(defclass runtime ()
  ((patchfun :initarg :patchfun :accessor patchfun)
   (npatchfun :initarg :npatchfun :accessor npatchfun)
   (patchaddr :initarg :patchaddr :accessor patchaddr)
   (bundle :initarg :bundle :accessor bundle)
   (host-patch :initarg :patch :accessor host-patch))
  (:default-initargs
    :patch nil
    :patchfun nil
    :npatchfun nil
    :patchaddr nil
    :bundle nil))

(defmethod stop-patch ((r runtime))
  nil)

(defun make-runtime (p)
  (let* ((pname (string (patchname p)))
         (rname (format nil "~a~a/~a.dll" *BC-patch-dir* pname pname))
         (bundle rname)
         (funame (gensym))
	 (nfuname (gensym))
         (gpname (gensym)))
    (eval
      `(ffi:def-call-out ,funame 
          (:name "patch")
          (:arguments)
	  (:language :stdc)
          (:library ,rname)
	  (:return-type ffi:int)))
    (eval
      `(ffi:def-call-out ,nfuname 
          (:name "npatch")
          (:arguments (n ffi:long))
	  (:language :stdc)
          (:library ,rname)
	  (:return-type nil)))
    (eval
      `(ffi:def-call-out ,gpname 
          (:name "paddr")
          (:arguments)
	  (:language :stdc)
          (:library ,rname)
	  (:return-type ffi:long)))
    (setf (runtime p)
          (make-instance 'runtime
            :bundle bundle :patchfun funame
	    :npatchfun nfuname
            :patchaddr gpname :patch p))
    (loop for b in (used-variables p)
          do (reader-access b))
    (loop for b in (used-variables p)
          do (writer-access b))
    (runtime p)))

(defmethod dispose-runtime ((r runtime))
  (when (bundle r) (ffi:close-foreign-library (bundle r)))
  (let ((hostp (host-patch r)))
    (setf (bundle r) nil (patchaddr r) nil
          (patchfun r) nil 
	  (npatcfun r) nil
	  (host-patch r) nil)
    (setf (runtime hostp) nil)))


(defun dispose-pointer (ptr)
  (ffi:foreign-free ptr))

(defun pointer-address (ptr)
  (ffi:foreign-address ptr))


(provide :BC-WINCLISP)

