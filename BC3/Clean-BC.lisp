
(in-package :BC)

; BC-files

(defun copy-file (src dst)
  (when (probe-file dst) (delete-file dst))
  (with-open-file (s src :direction :input)
    (with-open-file (d dst :direction :output
                       :if-does-not-exist :create)
      (loop for c = (read-char s nil nil)
            while c
            do (write-char c d)))))

(loop with src = (format nil "/Users/mak/BC3MACLW/BC3/")
      with dst = (format nil "/Users/mak/BC3MACLW/BC3_/")
      for f in BC-files
      for srcf = (format nil "~a~a.lisp" src f)
      for dstf = (format nil "~a~a.lisp" dst f)
      do (copy-file srcf dstf)
      finally
      (copy-file (format nil "~a~a.lisp" src "BC3_files")
                 (format nil "~a~a.lisp" dst "BC3_files")))

