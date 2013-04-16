(in-package :asdf)

(defsystem :cl-framenet
  :depends-on (:xmls :cl-network :s-dot)
  :components 
  ((:file "package")
   (:file "tree" :depends-on ("package"))
   (:file "configuration" :depends-on ("package" "tree"))
   (:file "sem-type" :depends-on ("configuration"))
   (:file "frame" :depends-on ("configuration"))
   (:file "lexical-unit" :depends-on ("configuration" "frame"))
   (:file "initialization" :depends-on ("lexical-unit"))))
	

