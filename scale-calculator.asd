;;;; scale-calculator.asd

(in-package :asdf)

(defsystem "scale-calculator"
    :description "scale-calculator: calculate scale notes"
    :version "0.1"
    :author "Philippe Mechai <philippe.mechai@gmail.com>"
    :licence "MIT"
    :serial t
    :components ((:file "package")
                 (:file "constants")
		 (:file "note")
		 (:file "scale")
		 (:file "scale-defs")
		 (:file "abc-export"))


