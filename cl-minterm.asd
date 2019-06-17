(asdf:defsystem #:cl-minterm
  :description "minimal raw terminal"
  :author "Jeremy Mates <jmates@cpan.org>"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi)
  :components ((:file "minterm")))
