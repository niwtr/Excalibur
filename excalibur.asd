(in-package :cl-user)
(defpackage excalibur-asd
  (:use :cl :asdf)
  (:export #:*excalibur-version))
(in-package :excalibur-asd)
(defparameter *excalibur-version* "0.4")

(defsystem #:excalibur
  :serial t
  :version #.*excalibur-version*
  :author "NiwTR 'Lexicall' niwtr@icloud.com"
  :license "Public Domain"
  :components ((:file "package")
               (:file "excalibur")))
;;EOF
