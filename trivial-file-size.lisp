;;;; trivial-file-size.lisp

(in-package #:trivial-file-size)

;;; "trivial-file-size" goes here. Hacks and glory await!

(deftype file-size ()
  '(or null (integer 0 *)))

(defun file-size-from-stream (file)
  (with-open-file (in file
                      :direction :input
                      :element-type '(unsigned-byte 8))
    (file-length in)))

(declaim
 (ftype
  (function ((or string pathname))
            file-size)
  file-size-in-octets))

(defun file-size-in-octets (file)
  (multiple-value-bind (path namestring)
      (etypecase file
        (string (values (ensure-pathname file :want-pathname t)
                        file))
        (pathname (values file
                          (native-namestring file))))
    (declare (ignorable path namestring))
    (handler-case
        (progn
          #+sbcl (sb-posix:stat-size (sb-posix:stat path))
          #+cmucl (nth-value 8 (unix:unix-stat namestring))
          #+ccl (ccl:file-data-size path)
          #+clisp (os:file-stat-size (os:file-stat path))
          #+allegro (excl.osi:stat-size (excl.osi:stat path))

          #-(or sbcl cmucl ccl clisp allegro)
          (file-size-from-stream file))
      (error ()
        nil))))
