#-asdf3.1 (error "docker requires ASDF 3.1")

(defsystem "docker"
  :name "Common Lisp Docker Library"
  :description "A Common Lisp client library for the Docker Remote API"
  :version "0.0.1"
  :license "MIT"
  :class :package-inferred-system
  :depends-on (:docker/all))
