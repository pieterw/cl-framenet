(in-package :cl-framenet)

;; -------------
;; Lexical units
;; -------------

;; From the framenet xml schema

;; A sketch of the XML under the root is as follows:

;; <lexUnit ...>
;;   <header> ... </header>
;;   <definition> ... </definition>
;;   <lexeme />+
;;   <semType />+
;;   <valences> ... </valences>
;;   <subCorpus> ... </subCorpus>*
;; </lexUnit>


;; Functions that retrieve and operate on the (parsed) xml of a
;; lexical unit

(defun read-lu (ID &key (lu-directory (framenet-file "lu/")))
  (let ((lu-path-string (format nil "~alu~a.xml" lu-directory ID)))
    (if (probe-file lu-path-string)
        (with-open-file (lu-file lu-path-string :direction :input)
          (xmls:parse lu-file)))))

(defun xml-lu-name (xml-lu)
  (xmlrep-attrib-value "name" xml-lu))

(defun xml-lu-ID (xml-lu)
  (parse-integer (xmlrep-attrib-value "ID" xml-lu)))

(defun xml-lu-POS (xml-lu)
  (xmlrep-attrib-value "POS" xml-lu))

(defun xml-lu-frame-name (xml-lu)
  (xmlrep-attrib-value "frame" xml-lu))

(defun xml-lu-frame-ID (xml-lu)
  (xmlrep-attrib-value "frameID" xml-lu))

(defun xml-lu-lexemes (xml-lu)
  (xmlrep-find-child-tags "lexeme" xml-lu))

(defun xml-lu-lexeme-strings (xml-lu)
  (loop for xml-lexeme in (xml-lu-lexemes xml-lu)
     collect (xmlrep-attrib-value "name" xml-lexeme)))


;; Lexical unit abstraction

(export '(lexical-unit xml id name POS frame-name frame-ID lexemes
create-lu-from-id find-lu-from-id all-lexical-units find-lu-by-name
find-lus-by-frame-ID find-lus-by-frame-name lexemes lexeme-strings POS frame-name frame-id))

(defclass lexical-unit ()
  ((xml :accessor xml
        :initarg :xml
        :initform nil
        :documentation "The parsed xml, a rather large s-expression that
        can be queried using xmls.")
   (id :initarg :id
       :initform nil
       :documentation "The ID of the lexicl unit. nil or an integer.")
   (name :initarg :name
         :initform  nil
         :documentation "The name of the lexical unit. A string. This
   is not the lemma. For example it might look like
   stand.v. (including the pos v)")))

(defun remove-subcorpus-children (xml-lu)
  (make-xmlrep (xmlrep-tag xml-lu)
               :attribs (xmlrep-attribs xml-lu)
               :children (remove-children "subCorpus" xml-lu)))

(defmethod initialize-instance :after ((lu lexical-unit) &key id (lu-directory (framenet-file "lu/"))
                                                    &allow-other-keys)
  (unless (xml lu)
    (setf (xml lu) (read-lu id :lu-directory lu-directory)))
  (setf (xml lu) ;; to reduce memory consumption
        (remove-subcorpus-children (xml lu)))
  lu)

(defun create-lu-from-id (id &key (lu-directory (framenet-file "lu/")))
  (make-instance 'lexical-unit :id id :lu-directory lu-directory))

(defmethod id ((lu lexical-unit))
  (or (slot-value lu 'id)
      (setf (slot-value lu 'id) (xml-lu-id (xml lu)))))
                              
(defmethod name ((lu lexical-unit))
  (or (slot-value lu 'name)
        (setf (slot-value lu 'name) (xml-lu-name (xml lu)))))

(defmethod POS ((lu lexical-unit))
  (xml-lu-POS (xml lu)))

(defmethod frame-name ((lu lexical-unit))
  (xml-lu-frame-name (xml lu)))

(defmethod frame-ID ((lu lexical-unit))
  (xml-lu-frame-ID (xml lu)))

(defmethod lexemes ((lu lexical-unit))
  (xml-lu-lexemes (xml lu)))

(defmethod lexeme-strings ((lu lexical-unit))
  (xml-lu-lexeme-strings (xml lu)))


  
(defun find-lu-by-ID (id)
  "Since the ID is part of the file-name we don't need to access the
lu-index. We can immediately open the requested file. Returns a
lexical-unit object or NIL."
  (create-lu-from-id id))

(export '(fn-indexes init-fn-data lu-index frame-index full-text-index))

(defclass fn-indexes ()
  ((lu-index :accessor lu-index
             :initarg :lu-index 
             :initform nil
             :documentation "The parsed index of lexical units.")
   (frame-index :accessor frame-index
             :initarg :frame-index 
             :initform nil
             :documentation "The parsed index of frames.")
   (full-text-index :accessor full-text-index
             :initarg :full-text-index 
             :initform nil
             :documentation "The parsed index of fully annotated
             texts."))
  (:documentation "Contains the lu, frame and text indexes. Used for
  finding the correct files to parse."))


(defun load-all-lexical-units (&key (fn-indexes *fn-indexes*) (max-nr nil))
  "Returns all lexical units. This can take a few minutes and will
consume approx. 1Gb RAM. This is NOT required for any of the other
functions like find-lu-by-name. Returns a (long) list of
lexical-unit objects. The subCorpus children will not be loaded in
memory."
  (format t "~%Loading lexical units:...")
  (loop for xml-lu in (xmlrep-find-child-tags "lu" (lu-index fn-indexes))
     for count from 1
     for lu = (find-lu-by-id (parse-integer (xmlrep-attrib-value "ID" xml-lu nil)))
     while (or (not (numberp max-nr))
               (<= count max-nr))
     when (xml lu)
     collect lu into result
     when (= (mod count 100) 0)
     do (format t ".")
     when (= (mod count 1000) 0)
     do (format t "~a" count)
     finally (format t ": Done!")
       (return result)))

(defun find-lu-by-name (name &key (fn-indexes *fn-indexes*)
                               (case-sensitive t))
  "Returns the lexican unit with the given name. Assumes that
init-fn-data has been called. Returns a lexical-unit object or NIL."
  (unless case-sensitive
    (setf name (string-downcase name)))
  (loop for xml-lu in (xmlrep-find-child-tags "lu" (lu-index fn-indexes))
     for found-name = (if case-sensitive
                          (xmlrep-attrib-value "name" xml-lu nil)
                          (string-downcase (xmlrep-attrib-value "name" xml-lu nil)))
     when (string= name found-name)
     return (find-lu-by-id (parse-integer (xmlrep-attrib-value "ID" xml-lu nil)))))

(defun find-lus-by-frame-ID (frame-id &key (fn-indexes *fn-indexes*))
  "Returns all lexical units that subscribe to the given
frame-id. Assumes that init-fn-data has been called. Returns a list of
lexical-unit objects."
  (when (stringp frame-id) 
    (setf frame-id (parse-integer frame-id)))
  (loop for xml-lu in (xmlrep-find-child-tags "lu" (lu-index fn-indexes))
     when (= frame-id (parse-integer (xmlrep-attrib-value "frameID" xml-lu nil)))
     collect (find-lu-by-id (parse-integer (xmlrep-attrib-value "ID" xml-lu nil)))))

(defun find-lus-by-frame-name (frame-name &key (fn-indexes *fn-indexes*)
                                            (case-sensitive t))
  "Returns all lexical units that subscribe to the given
frame-name. Assumes that init-fn-data has been called. Returns a list
of lexical-unit objects."
  (unless case-sensitive
    (setf frame-name (string-downcase frame-name)))
  (loop for xml-lu in (xmlrep-find-child-tags "lu" (lu-index fn-indexes))
     for found-frame-name = (if case-sensitive
                                (xmlrep-attrib-value "frameName" xml-lu nil)
                                (string-downcase (xmlrep-attrib-value "frameName" xml-lu nil)))
     when (string= frame-name found-frame-name)
     collect (find-lu-by-id (parse-integer (xmlrep-attrib-value "ID" xml-lu nil)))))
     
