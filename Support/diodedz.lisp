(defclass .diode (root-block)

(defmethod initialize-macro-block ((b .diode) &key port-name)

(defun .diode (&rest rest)

Do I put the non-linear models inside the defun section? or the
defmethod section?
Or do I need a new section?

This is what I came up with so far:
    (lambda (vrd (.voltage pb)))
    (lambda (rdp (.var 2.7e+3)))
    (lambda (rdm (.var 1.0e+11)))
    (lambda (rd (cond ((> vrd 0.0)
                (cond (((eq (direction b) '+) rdp)
                      (t rdm)))
               (t
                (cond (((eq (direction b) '+) rdm)
                      (t rdp))))))))
     (-> (output pb) (.R rd))
