;;;;; cl-minterm - wafer thin raw terminal 

(in-package #:cl-user)
(defpackage #:cl-minterm
  (:use #:common-lisp #:cffi)
  (:export #:with-rawterm))
(in-package #:cl-minterm)

(defcstruct termios
  (c-iflag :unsigned-int) (c-oflag :unsigned-int)
  (c-clfag :unsigned-int) (c-lflag :unsigned-int)
  (c-cc :unsigned-char :count 20)
  (c-ispeed :int) (c-ospeed :int))

(defcfun "cfmakeraw" :void
         (termios (:pointer (:struct termios))))
(defcfun "tcgetattr" :int (fd :int) 
         (termios (:pointer (:struct termios))))
(defcfun "tcsetattr" :int (fd :int) (action :int)
         (termios (:pointer (:struct termios))))

(defmacro with-rawterm (fd action &body body)
  `(with-foreign-objects ((orig '(:struct termios))
                          (raw '(:struct termios)))
     (tcgetattr ,fd orig)
     ; TODO how copy struct so don't need 2nd call here?
     (tcgetattr ,fd raw)
     (cfmakeraw raw)
     (tcsetattr ,fd ,action raw)
     (unwind-protect
       (progn ,@body)
       (tcsetattr ,fd ,action orig))))
