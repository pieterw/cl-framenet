(in-package :cl-framenet)


(defun assert-type (type node)
  (assert (string-equal (xmlrep-tag node) type)))

(defun remove-children (tag treenode &key (test #'string-equal) (key #'xmlrep-tag))
  (loop for child in (xmlrep-children treenode)
     unless (funcall test (funcall key child) tag)
     collect child))

;; -----------------------------------------
;; Semantic types aka sem-types aka semTypes
;; -----------------------------------------

(defclass sem-type-tree (tree)
  ()
  (:documentation "A tree that can represent all sem-types."))

(defclass sem-type (tree-node)
  ((id :accessor id
       :initarg :id
       :initform nil
       :documentation "An integer number.")
   (name :accessor name
         :initarg :name
         :initform nil
         :documentation "The full name of the semantic type. A
         string.")
   (abbrev :accessor abbrev
           :initarg :abbrev
           :initform nil
           :documentation "An abbreviated version of the name. Also a
           string. Can be the same as name.")
   (definition :accessor definition
     :initarg :definition
     :initform nil
     :documentation "A string describing the semantic type. Can be
     quite long, multiple sentences.")
   (parent :accessor super-type
           :accessor parent
           :initarg :super-type
           :initarg :parent
           :initform nil
           :documentation "The super type of the sem type. It refers
           to another sem-type. This is part of the tree-node
           superclass. It has a second accessor and initarg.")
   (children :accessor sub-types
             :accessor children
             :initarg :sub-types
             :initarg :children 
             :initform nil
             :documentation "The sub-types of the sem-type. A list of
             sem-types. Is part of the tree-node superclass. A new
             accessor and initarg is added."))
  (:documentation "Abstract for framenet sem-types. They are also
  nodes in a tree."))

(defmethod string-for-s-dot ((node sem-type) &key (key #'name))
  "Needed for exporting to s-dot."
  (mkdotstr (funcall key node)))

(defmethod print-object ((st sem-type) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
        (format stream "<sem-type ~(~a~):~:_ name: ~a,
        super-type (parent): ~a,:~:_ sub-types (children): ~a,~:_"
                (id st) (name st) (and (super-type st) (name (super-type st))) 
                (mapcar #'name (sub-types st)))
        (call-next-method)
        (format stream ">"))
      (format stream "<sem-type: ~a>" (name st))))

(defun xml-sem-type-definition (xml-sem-type)
  "Helper function to get the definition (as a string) from the xml
version sem-type."
  (first (xmlrep-children (xmlrep-find-child-tag "definition" xml-sem-type nil))))

(defun xml-sem-type-supertype (xml-sem-type)
  "Helper function to get the supertype as a (ID . name) cons from the
xml version sem-type."
  (let ((super-type (xmlrep-find-child-tag "superType" xml-sem-type nil)))
    (when super-type
      (cons (parse-integer (xmlrep-attrib-value "supID" super-type))
            (xmlrep-attrib-value "superTypeName" super-type)))))

(defun create-sem-type (xml-sem-type)
  "Create a sem-type object from an xml sem-type description."
  (assert-type "semType" xml-sem-type)
  (make-instance 'sem-type
   :id (parse-integer (xmlrep-attrib-value "ID" xml-sem-type))
   :name (xmlrep-attrib-value "name" xml-sem-type)
   :abbrev (xmlrep-attrib-value "abbrev" xml-sem-type)
   :definition (xml-sem-type-definition xml-sem-type)
   :super-type (xml-sem-type-supertype xml-sem-type)))

(defun set-super-types (sem-type-tree)
  "Internal function. Retrieves the real references to the super-types
instead of a (id . name) cons. This allows for a much faster traversal
of the tree. This should only be called when the complete *sem-types*
has been filled and normally should be called only once."
  (loop for sem-type in (nodes sem-type-tree)
     when (consp (super-type sem-type))
     do (setf (super-type sem-type) 
              (get-sem-type (first (super-type sem-type)) sem-type-tree))))

(defun set-sub-types (sem-type-tree)
  "Internal function. The framenet data only keeps links to its
superType (parent) and not its subTypes (children). This function
explicitly calculates and sets the subtypes. It assumes
set-super-types has been called."
  (loop for st in (nodes sem-type-tree)
     when (and (super-type st)
               (typep (super-type st) 'sem-type))
     do (push st (sub-types (super-type st)))))

(defun init-sem-types (&key (file-path (framenet-file "semTypes.xml")))
  "Public function. Will read all semTypes from framenet data and load
them into memory. Afterwards you can access this data through the
accessor function get-sem-type."
  (format t "~%Loading semantic types...")
  (unless (probe-file file-path)
    (error "Could not locate framenet file \"~A\", perhaps
    you forgot to unpack the framenet data file?" file-path))
  (let ((sem-type-tree (make-instance 'sem-type-tree))
        (xml-sem-types (with-open-file (s (framenet-file "semTypes.xml")
                                      :direction :input)
                     (xmls:parse s))))
    (dolist (xml-sem-type (xmlrep-find-child-tags "semType" xml-sem-types))
      (push (create-sem-type xml-sem-type) (nodes sem-type-tree)))
    (set-super-types sem-type-tree) ;; will correct all :parent links
    (set-sub-types sem-type-tree) ;; will correct all :children links
    
    ;; one problem is that the sem-types are not necessarily a nice
    ;; tree with a single root. I create an artificial top sem-type
    ;; node to which all actual (previous) top-level sem-types link.
    (let ((root-type (make-instance 'sem-type :id 0 :abbrev "Sem_type"
                                    :name "Sem_type" :parent nil)))
      (push root-type (nodes sem-type-tree))
      (setf (top sem-type-tree) root-type)
      (loop for sem-type in (nodes sem-type-tree)
         when (top? sem-type)
         do (setf (super-type sem-type) root-type)
           (push sem-type (sub-types root-type))))
    (format t ": Done!")
    sem-type-tree))


;; -------------------------------------------------------------------
;; convenience and accessor functions for sem-types and sem-type-trees
;; -------------------------------------------------------------------

(export '(get-sem-type all-supertypes all-subtypes))

(defgeneric get-sem-type (id-or-name sem-type-tree &key &allow-other-keys)
  (:documentation "Finds the sem-type with the given name or ID. If it
  is not found then NIL is returned."))

(defmethod get-sem-type (anything sem-type-tree &key (key #'identity))
  (find-node sem-type-tree anything :key key))

(defmethod get-sem-type ((name string) (sem-type-tree sem-type-tree) &key)
 (find-node sem-type-tree name :key #'name :test #'string-equal))

(defmethod get-sem-type ((id number) (sem-type-tree sem-type-tree) &key)
  (find-node sem-type-tree id :key #'id))

(defun all-supertypes (sem-type)
  "Retrieves all of the supertypes (the upward branch) from the given
sem-type. The first element is the top."
  (parents sem-type))

(defun all-subtypes (sem-type)
  "Recursively retrieves (as a flat list) all subtypes of the given
sem-type. Essentially these are all of the sem-types in the underlying
tree of the given sem-type."
  (all-children sem-type))
