;;;;; cl-minterm - sets the terminal to raw mode and provides a few
;;;;; ANSI/XTerm Control Sequence related utility functions

(in-package #:cl-user)
(defpackage #:cl-minterm
  (:use #:common-lisp #:cffi)
  (:export #:+alt-screen+ #:+clear-screen+ #:+clear-right+ #:+hide-cursor+
           #:+hide-pointer+ #:+show-cursor+ #:+term-norm+ #:+unalt-screen+
           #:at #:at-col #:bgcolor #:color #:grey #:norm
           #:emit-at #:emit-at-col #:emit-at-row #:with-rawterm
           #:getch #:restore-screen #:setup-screen #:term-size))
(in-package #:cl-minterm)

; this may not be portable
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

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name
     (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

; ANSI or XTerm Control Sequences - https://invisible-island.net/xterm/
(define-constant +alt-screen+   #.(format nil "~C[?1049h" #\Esc))
(define-constant +clear-screen+ #.(format nil "~C[1;1H~C[2J" #\Esc #\Esc))
(define-constant +clear-line+   #.(format nil "~C[2K"   #\Esc))
(define-constant +clear-right+  #.(format nil "~C[K"    #\Esc))
(define-constant +hide-cursor+  #.(format nil "~C[?25l" #\Esc))
(define-constant +hide-pointer+ #.(format nil "~C[>2p"  #\Esc))
(define-constant +show-cursor+  #.(format nil "~C[?25h" #\Esc))
(define-constant +term-norm+    #.(format nil "~C[m"    #\Esc))
(define-constant +unalt-screen+ #.(format nil "~C[?1049l" #\Esc))

(defmacro at (col row)
  `(format t "~C[~d;~dH" #\Esc ,row ,col))
(defmacro at-col (col)
  `(format t "~C[~dG" #\Esc ,col))
; olden terminals may not support such colors. TERM=xterm-256color or
; similar is likely required, in addition to ISO-8613-3 support
(defmacro bgcolor (r g b)
  `(format t "~C[48;2;~d;~d;~dm" #\Esc ,r ,g ,b))
(defmacro color (r g b &optional (style 0))
  `(format t "~C[~d;38;2;~d;~d;~dm" #\Esc ,style ,r ,g ,b))
(defmacro grey (v &optional (style 0))
  `(color ,v ,v ,v ,style))
(defmacro norm ()
  `(princ +term-norm+))

(defmacro emit-at (col row string)
  `(progn (at ,col ,row)
          (princ ,string)))
(defmacro emit-at-col (col string)
  `(progn (at-col ,col)
          (princ ,string)))
(defmacro emit-at-row (row string)
  `(progn (at 1 ,row)
          (princ +clear-right+)
          (princ ,string)))

(defmacro with-rawterm (fd action &body body)
  `(with-foreign-objects ((orig '(:struct termios))
                          (raw '(:struct termios)))
     (tcgetattr ,fd orig)
     (tcgetattr ,fd raw)
     (cfmakeraw raw)
     (tcsetattr ,fd ,action raw)
     (unwind-protect
       (progn ,@body)
       (tcsetattr ,fd ,action orig))))

; handle <C-c> that SBCL otherwise does unwanted things with
(defun getch ()
  (let ((key))
    (handler-case
     (restart-case (setf key (read-char))
       (control-c nil (setf key 3)))
     #+SBCL
     (sb-sys:interactive-interrupt nil (setf key 3)))
    key))

; for term-size
(defun read-int-to (end)
  (let ((str
         (make-array 2 :fill-pointer 0 :adjustable t :element-type
                     'standard-char)))
    (loop :do (let ((ch (read-char)))
                (if (char= ch end)
                    (loop-finish)
                    (vector-push-extend ch str))))
    (parse-integer str)))

(defun restore-screen (&rest more)
  (at-col 1)
  (dolist (x (append (list +term-norm+ +unalt-screen+ +show-cursor+) more))
    (princ x)))

(defun setup-screen (&rest more)
  (dolist (x (append (list +term-norm+ +alt-screen+ +clear-screen+) more))
    (princ x)))

; \e[18t in, \e[8;R;Ct out (if all goes well)
(defun term-size ()
  (format t "~C[18t" #\Esc)
  (force-output)
  (dolist (ch '(#\Esc #\[ #\8 #\;))
    (unless (char= (read-char) ch) (error "invalid CSI response")))
  (values (read-int-to #\;) (read-int-to #\t)))
