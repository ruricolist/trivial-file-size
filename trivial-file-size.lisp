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
  "Return the size of FILE in octets.
Whenever possible, get the size from the file's metadata.

Some platforms (e.g. ABCL) may return 0 when the file does not exist."
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
          #+gcl (nth 1 (sys:stat namestring))

          ;; According to trivial-features LispWorks pushes :unix to
          ;; `*features*`.
          #+(and lispworks unix)
          (sys:file-stat-size (sys:get-file-stat namestring))

          #+abcl
          (let* ((class (java:jclass "java.io.File"))
                 (method (java:jmethod class "length"))
                 (file (java:jnew class namestring)))
            (java:jcall method file))

          #-(or sbcl cmucl ccl clisp allegro abcl gcl
                (and lispworks unix))
          (file-size-from-stream file))
      (error ()
        nil))))
