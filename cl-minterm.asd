(asdf:defsystem #:cl-minterm
  :description "minimal raw terminal plus some xterm control sequences"
  :author "Jeremy Mates <jmates@cpan.org>"
  :license "BSD"
  :version "0.0.3"
  :serial t
  :depends-on (#:cffi)
  :components ((:file "minterm")))
