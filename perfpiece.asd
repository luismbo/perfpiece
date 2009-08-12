;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem perfpiece
  :description "CL:TIME on steroids."
  :author "Luis Oliveira <loliveira@common-lisp.net>"
  :licence "MIT"
  :depends-on (alexandria cffi)
  :serial t
  :components ((:file "perfpiece")))
