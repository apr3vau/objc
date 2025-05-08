(asdf:defsystem objc
  :author "The Calendrical System"
  :license "0BSD"
  :depends-on (:alexandria :anaphora :closer-mop :cffi :cffi-libffi)
  :components ((:file "objc-raw")
               (:file "objc")))
