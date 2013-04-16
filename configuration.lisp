(in-package :cl-framenet)

(export '(framenet-file *framenet-data-directory* *frame-network*
*lexical-units* *sem-type-tree* *fn-indexes*))

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Make sure *framenet-data-directory* points correctly.
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++

(defparameter *framenet-data-directory* "/path/to/your/framenet-data"
  "A string pointing to the framenet data folder on your system.")



(defun framenet-file (string)
  "Convenience function to create a string path in the framenet
directory."
  (format nil "~a/~a" *framenet-data-directory* string))


;; -----------------------------------
;; Don't manually change the following
;; -----------------------------------

(defparameter *fn-indexes* nil
  "Refers to an fn-indexes object that point to all of the parsed
  index files. Will be set by the function init-fn-data, which is
  assumed to be called before accessing any framenet-data.")

(defparameter *sem-type-tree* nil
  "A tree structure containing all the framenet sem types. Will be set
  by the function init-fn-data.")

(defparameter *frame-network* nil
  "A network structure containing all the framenet frames. Might be
  set by the function init-fn-data.")

(defparameter *lexical-units* nil
  "A list containing all the framenet lexical units. Is not
  necessarily set.")
