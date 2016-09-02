
(in-package :bc)

#|
(make-audio)
(compile-patch *AUDIO*)
(load-patch *AUDIO*)
(inspect *AUDIO*)
(c-code *AUDIO*)
(start-audio 44100.0)
(start-audio)
(stop-audio)
(restart-audio)

(pa_server *AUDIO*)
(block-items *AUDIO*)
(patchlinks-p *AUDIO*)
(restart-audio)
(at (first (variables (elt (block-items *AUDIO*) 0))) 0)
(setf (at (first (variables (elt (block-items *AUDIO*) 0))) 0) 0)
(get-callback (pa_server *AUDIO*))
(set-patchlink *AUDIO* 12)
(release-patchlink *AUDIO* 12)

(inspect (ad-vector *AUDIO*))
(c-address (ad-vector *AUDIO*))

(remove-all-patches)
*patches*

(defparameter fun
  (cl-user::make.caller
   (bundle (runtime *AUDIO*))
   "getCallback" () :unsigned-fullword))
(funcall fun)
|#


#|
(defparameter dax
  (make-instance '.da))
(inspect dax)

(defpatch dp ((x (.var 0.0))
              (da (.da)))
  (-> x (in da 0))
  (-> x (in da 1)))

(inspect dp)
(c-code dp)
(compile-patch dp)
(load-patch dp)
(run-patch dp)
(stop-patch dp)
|#


#|
(defparameter adx
  (make-instance '.ad))
(inspect adx)

(defpatch dp ((ad (.ad)))
  (-> (out ad 0) (.probe "left"))
  (-> (out ad 1) (.probe "right")))

(inspect dp)
(c-code dp)
(compile-patch dp)
(load-patch dp)
(run-patch dp)
(stop-patch dp)

(defpatch dp ((ad (.ad))
              (da (.da)))
  (-> (out ad 0) (in da 0))
  (-> (out ad 1) (in da 1)))

(defpatch so ((srate 48000.0)
              (so (.sin-osc :freq 800.0 :ampl 0.9))
              (da (.da)))
  (-> so (inputs da)))

(defpatch so ((so (.sin-osc :freq 800.0 :ampl 0.9))
              (da (.da)))
 ; (-> so (.probe "out"))
  (-> so (inputs da)))

(defpatch sb ()
  (-> (.sin-osc :freq 700.0 :ampl 0.5) (inputs (.da))))

(inspect so)
(compile-patch so)
(load-patch so)
(run-patch so)
(stop-patch so)
(c-code so)
(step-patch-n so 10000000 t)

(run-patch sb)
(stop-patch sb)

(Pa_GetDefaultInputDeviceID)

(defparameter ptr
  (%int-to-ptr 85139720))

(defparameter ptr
  (%int-to-ptr 85139592))

(%get-double-float ptr)

 (defpatch so ((so (.sin-osc :freq 800.0 :ampl 0.9))
              (da (.probe "out")))
   (-> so (inputs da)))

(load-patch so)

(step-patch so)

(describe (elt (pa_devices) 2))

(describe (elt (pa_devices) 0))
(describe (indev (elt (pa_devices) 0)))
|#


