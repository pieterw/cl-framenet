(in-package :cl-framenet)

;; -------------
;; frame-element
;; -------------

(defun xml-fe-ID (xml-fe)
  (parse-integer (xmlrep-attrib-value "ID" xml-fe)))

(defun xml-fe-name (xml-fe)
  (xmlrep-attrib-value "name" xml-fe))

(defun xml-fe-abbrev (xml-fe)
  (xmlrep-attrib-value "abbrev" xml-fe))

(defun xml-fe-core-type (xml-fe)
  (xmlrep-attrib-value "coreType" xml-fe))

(defun xml-fe-definition (xml-fe)
  "Helper function to get the definition (as a string) from the xml
version frame-element."
  (first (xmlrep-children (xmlrep-find-child-tag "definition" xml-fe nil))))

(defun xml-fe-sem-types (xml-fe)
  "Helper function to get the sem-types from the xml version
frame-element. Assumes that init-fn-data has been called so that
sem-types are known."
  (loop for xml-sem-type in (xmlrep-find-child-tags "semType" xml-fe)
     collect (get-sem-type (xmlrep-attrib-value "ID" xml-sem-type) *sem-type-tree*)))

(defclass frame-element ()
  ((xml :accessor xml
        :initarg :xml
        :initform nil
        :documentation "The parsed xml, an s-expression that can be
        queried using xmls.")
   ;; There are no accessors on purpose, getters are defined below,
   ;; setters are not needed.
   (id :initarg :id
       :initform nil
       :documentation "A unique integer number.")
   (name :initarg :name
         :initform nil
         :documentation "The name of the frame-element. A string.")
   (abbrev :initarg :abbrev
           :initform nil
           :documentation "An abbreviation of the name. Also a string
           and can be the same as name.")
   (core-type :initarg :core-type
              :initform nil
              :documentation "")
   (sem-types :initarg :sem-types
             :initform nil
             :documentation "A list of sem-type objects."))
  (:documentation "Frames consist of frame-elements. Each
  frame-element has an id, a name (and abbrev), a definition, a
  core-type and a list of associated sem-types."))

(defmethod id ((fe frame-element))
  (or (slot-value fe 'id)
      (setf (slot-value fe 'id) (xml-fe-id (xml fe)))))
           
(defmethod name ((fe frame-element))
  (or (slot-value fe 'name)
      (setf (slot-value fe 'name) (xml-fe-name (xml fe)))))

(defmethod abbrev ((fe frame-element))
  (or (slot-value fe 'abbrev)
      (setf (slot-value fe 'abbrev) (xml-fe-abbrev (xml fe)))))

(defmethod core-type ((fe frame-element))
  (or (slot-value fe 'core-type)
      (setf (slot-value fe 'core-type) (xml-fe-core-type (xml fe)))))

(defmethod sem-types ((fe frame-element))
  (or (slot-value fe 'sem-types)
      (setf (slot-value fe 'sem-types) (xml-fe-sem-types (xml fe)))))

(defmethod string-for-s-dot ((fe frame-element) &key (key #'name))
  "Needed for exporting to s-dot."
  (mkdotstr (funcall key fe)))

(defmethod print-object ((fe frame-element) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
        (format stream "<frame-element ~(~a~):~:_ name: ~a, core-type:
        ~a,:~:_"
                (id fe) (name fe) (core-type fe))
        (call-next-method)
        (format stream ">"))
      (format stream "<frame-element: ~a>" (name fe))))

(defun create-fe (xml-fe)
  (assert-type "FE" xml-fe)
  (make-instance 'frame-element
                 :xml xml-fe))


;; -----
;; Frame
;; -----

(defun read-frame (name &key (frame-directory (framenet-file "frame/")))
  (let ((frame-path-string (format nil "~a~a.xml" frame-directory name)))
    (if (probe-file frame-path-string)
        (with-open-file (frame-file frame-path-string :direction :input)
          (xmls:parse frame-file)))))

(defun xml-frame-ID (xml-frame)
  (parse-integer (xmlrep-attrib-value "ID" xml-frame)))

(defun xml-frame-name (xml-frame)
  (xmlrep-attrib-value "name" xml-frame))

(defun xml-frame-abbrev (xml-frame)
  (xmlrep-attrib-value "abbrev" xml-frame))

(defun xml-frame-definition (xml-fr)
  "Helper function to get the definition (as a string) from the xml
version frame."
  (xmlrep-string-child (xmlrep-find-child-tag "definition" xml-fr)))

(defun xml-frame-fes (xml-fr)
  (mapcar #'create-fe (xmlrep-find-child-tags "FE" xml-fr)))

(defun xml-frame-lus (xml-fr)
  (xmlrep-find-child-tags "lexUnit" xml-fr))

(defun xml-frame-relations (xml-fr)
  (xmlrep-find-child-tags "frameRelation" xml-fr))

(defun xml-superframes (xml-fr)
  "Returns nil or a list of strings with all of the frames the given
frame inherits from."
  (loop for frame-relation in (xml-frame-relations xml-fr)
     when (find (xmlrep-attrib-value "type" frame-relation) 
                '("Inherits from" "Perspective on" "Uses" "Subframe of") ;;maybe "See also" as well
                :test #'string-equal)       
     append (mapcar #'xmlrep-string-child (xmlrep-find-child-tags "relatedFrame" frame-relation))))

;; (defun xml-superframes-rec (xml-fr)
;;   "Returns nil or a list of strings with all of the frames the given
;; frame inherits from."
;;   (let ((superframes (xml-superframes xml-fr)))
;;     (if superframes
;;         (loop for super-frame-name in superframes
;;            for super-frame = (
;;            append (xml-superframes-rec super-frame)))))
        

(defun xml-subframes (xml-fr)
  "Returns nil or a list of strings with all of the frames that
inherit from the given frame"
  (loop for frame-relation in (xml-frame-relations xml-fr)
     when (find (xmlrep-attrib-value "type" frame-relation) 
                '("Is Inherited by" "Is Perspectivized in" "Is Used by" "Has Subframe(s)")
                :test #'tree-equal)
     append (mapcar #'xmlrep-string-child (xmlrep-find-child-tags "relatedFrame" frame-relation))))

(defclass frame-network (network)
  ()
  (:documentation "A network structure containing all of the
  frames."))

(export '(frame-network frame abbrev frame-elements lexical-units
superframes subframes all-super-frames inherits_from find-frame))

(defclass frame (net-node)
  ((xml :accessor xml
        :initarg :xml
        :initform nil
        :documentation "The parsed xml, an s-expression that can be
        queried using xmls.")
   (id :initarg :id
       :initform nil
       :documentation "The ID of the frame. An integer. Should be
       unique.")
   (name :initarg :name
         :initform nil
         :documentation "The full name of the frame. A string.")
   (abbrev :initarg :abbrev
           :initform nil
           :documentation "Shorter abbreviation of the name. Also a
           string, can be the same as the name.")
   ;; (definition :accessor definition
   ;;   :initarg :definition
   ;;   :initform nil
   ;;   :documentation "The full written out definition of the frame. A
   ;;   rather large string. Multiple sentences.")
   ;; (sem-types :accessor sem-types
   ;;            :initarg :sem-types
   ;;            :initform nil
   ;;            :documentation "A list of sem-types associated with the
   ;;            frame.")
   (frame-elements :initarg :frame-elements
                   :initform nil
                   :documentation "A list of frame-element objects.")
   (lexical-units :initarg :lexical-units
                  :initform nil
                  :documentation "The lexical units associated with
                  the frame. These are NOT lexical-unit objects (since
                  they might not exist) but the parsed xml as found in
                  the frame.")
   (superframes :initarg :superframes
                :initform nil
                :documentation "All the frames this frame inherits from.")
   (subframes :initarg :subframes
             :initform nil
             :documentation "All the frames this frame is inherited
             by.")))

(defmethod initialize-instance :after ((frame frame) &key name (frame-directory (framenet-file "frame/"))
                                                    &allow-other-keys)
  (unless (xml frame)
    (setf (xml frame) (read-frame name :frame-directory frame-directory)))
  frame)

(defmethod print-object ((frame frame) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
        (format stream "frame (~a):~:_ name: ~a,
        superframes : ~a,:~:_ subframes: ~a,~:_"
                (id frame) (name frame) (superframes frame) (subframes frame))
        (call-next-method)
        (format stream ">"))
      (format stream "<frame ~a>" (name frame))))

(defmethod string-for-s-dot ((frame frame) &key)
  (mkdotstr (name frame)))

(defun create-frame-from-name (name &key (frame-directory (framenet-file "frame/")))
  (make-instance 'frame :name name :frame-directory frame-directory))

(defun create-frame-from-id (id &key (frame-directory (framenet-file "frame/"))
                                  (fn-indexes *fn-indexes*))
  (loop for frame-index-el in (xmlrep-find-child-tags "frame" (frame-index fn-indexes))
     when (= id (parse-integer (xmlrep-attrib-value "ID" frame-index-el)))
     return (create-frame-from-name (xmlrep-attrib-value "name" frame-index-el) 
                                    :frame-directory frame-directory)))

(defmethod id ((f frame))
  (or (slot-value f 'id)
      (setf (slot-value f 'id) (xml-frame-id (xml f)))))
           
(defmethod name ((f frame))
  (or (slot-value f 'name)
      (setf (slot-value f 'name) (xml-frame-name (xml f)))))

(defmethod abbrev ((f frame))
  (or (slot-value f 'abbrev)
      (setf (slot-value f 'abbrev) (xml-frame-abbrev (xml f)))))

(defmethod frame-elements ((f frame))
  (or (slot-value f 'frame-elements)
      (setf (slot-value f 'frame-elements) (xml-frame-fes (xml f)))))

(defmethod lexical-units ((f frame))
  (or (slot-value f 'lexical-units)
      (setf (slot-value f 'lexical-units) (xml-frame-lus (xml f)))))

(defmethod superframes ((f frame))
  (or (slot-value f 'superframes)
      (setf (slot-value f 'superframes) (xml-superframes (xml f)))))

(defmethod subframes ((f frame))
  (or (slot-value f 'subframes)
      (setf (slot-value f 'subframes) (xml-subframes (xml f)))))


;; -------------------------------------------------------------------
;; Convenience and accessor functions on the lisp abstraction of frame
;; -------------------------------------------------------------------

(defun load-all-frames (&key (fn-indexes *fn-indexes*))
  (loop for frame-index-el in (xmlrep-find-child-tags "frame" (frame-index fn-indexes))
     collect (create-frame-from-name (xmlrep-attrib-value "name" frame-index-el))))

(defun init-frames (&key (fn-indexes *fn-indexes*))
  (let ((frame-network (make-instance 'frame-network)))
    (format t "~%Loading frames...") 
    (setf (nodes frame-network) (load-all-frames :fn-indexes fn-indexes))
    (format t ": Done!")
    (format t "~%Connecting frames...")
    (loop for frame in (nodes frame-network)
       do (loop for superframe-string in (superframes frame)
             for superframe = (find superframe-string (nodes frame-network) :key #'name :test #'string-equal)
             do (create-and-add-edge frame-network 'link frame superframe)))
    (format t ": Done!")
    frame-network))

(defun find-frame (frame network &key (test #'eql) (key #'identity))
  (find frame (nodes network) :test test :key key))

(defgeneric all-super-frames (frame &key &allow-other-keys))


(defmethod all-super-frames ((frame frame) &key)
  (all-neighbours frame :direction :out))

(defmethod all-super-frames ((frame string) &key (frame-network *frame-network*))
  (setf frame (find-frame frame frame-network :key #'name :test #'equal))
  (all-super-frames frame))

