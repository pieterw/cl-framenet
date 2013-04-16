;;; Very simple implementation of a tree. 

(in-package :cl-framenet)

(export '(tree-node parent children tree nodes top empty? traverse
	  leaf? top? leafs find-node add-node replace-node has-parent?  parents
	  depth cut-node insert-node all-children id))

(defclass tree-node ()
  ((id :accessor id
       :initarg :id
       :initform (gensym "node-")
       :documentation "Needed for drawing. You can overwrite them as
       long as each node has a unique ID.")
   (parent :initform nil
	   :initarg :parent
	   :accessor parent)
   (children :type list
	     :initform nil
	     :initarg :children
	     :accessor children))
  (:documentation "A node is a very general object. When a class subclasses from
  a node all it means is that it can be maintained in a tree
  structure."))

(defclass tree () 
  ((nodes :type list
	  :initform nil
	  :accessor nodes
	  :initarg :nodes)
   (top :type (or null tree-node)
	:initarg :top
	:accessor top
	:initform nil))
  (:documentation "A basic tree keeping all the nodes and a reference
  to the top."))

(defgeneric top? (node)
  (:documentation "Returns true if the node is the top of the tree."))
(defmethod top? ((node tree-node))
  (null (parent node)))

(defgeneric empty? (container)
  (:documentation "Returns true if the container is empty."))

(defmethod empty? ((list list))
  (null list))
(defmethod empty? ((tree tree))
  (empty? (nodes tree)))

(defgeneric traverse (tree func &key &allow-other-keys)
  (:documentation "Traverses the tree and evaluates the
  function for every node. The key parameter from allows
  you to specify another start-node than the top of the tree. The
  traversal is depth-first."))

(defmethod traverse ((node tree-node) (func function) &key &allow-other-keys)
  (loop
   initially (funcall func node)
   for child in (children node)
   do (traverse child func)))

(defmethod traverse ((tree tree) (func function) &key &allow-other-keys)
  (when (top tree)
    (traverse (top tree) func)))

(defgeneric leaf? (node)
  (:documentation "Returns true if the node is a leaf"))
(defmethod leaf? ((node tree-node))
  (null (children node)))

(defgeneric leafs (tree)
  (:documentation "Returns all the leafs of the tree."))

(defmethod leafs ((tree tree))
  (loop for node in (nodes tree)
     when (leaf? node)
     collect node))

(defgeneric find-node (tree node &key key test &allow-other-keys)
  (:documentation "Finds the node based on the given key and test in
  the tree. If not found returns nil."))

(defmethod find-node ((tree tree) node &key (key #'identity) (test #'eql))
  (find node (nodes tree) :key key :test test))

(defgeneric add-node (tree node &key &allow-other-keys)
  (:documentation "Adds the given node to the tree. It can only
  allow adding under existing nodes. This means that the first
  element added has to be the top."))

(defmethod add-node :before ((tree tree) (node tree-node) &key (parent nil))
  ;; error checking
  (cond ((and (not parent)
	      (top tree))
         (error "Please supply a :parent when there already is a top-node."))
        ((find node (nodes tree))
	 (error "Cannot add a node that is already part of the tree."))
        ((and parent 
              (not (find parent (nodes tree))))
         (error "The supplied :parent is not a node in the tree."))))

(defmethod add-node ((tree tree) (node tree-node) &key (parent nil))
  ;; This method ignores any value that might have already been set in
  ;; the parent slot of the node to be added.
  (setf (parent node) parent)
  (if parent
      (push node (children (parent node)))
      (setf (top tree) node))
  (push node (nodes tree))
  tree)

(defgeneric replace-node (tree old-node new-node)
  (:documentation "replaces the old-node by the new-node in the
  tree. It only works when old-node is a leaf."))

(defmethod replace-node ((tree tree) (old-node tree-node) (new-node tree-node))
  (unless (find old-node (nodes tree) :test #'equal)
    (error "Old node ~a not found in tree ~a" old-node tree))
  (when (find new-node (nodes tree))
    (error "Cannot add a node that is already part of the tree."))
  ;; removing the old-node
  (when (top? old-node)
    (setf (top tree) nil))
  (setf (nodes tree) (delete old-node (nodes tree)))
  ;; adding the new-node
  (add-node tree new-node :parent (parent old-node))
  ;; the old-node however might have had children, we need to make
  ;; sure all referencing is ok
  (setf (children new-node) (children old-node))
  (dolist (child-node (children new-node)) ;; re-link children
    (setf (parent child-node) new-node))
  (when (parent new-node) ;; re-link parent
    (setf (children (parent new-node)) (delete old-node (children (parent new-node)))))
  tree)

(defgeneric cut-node (tree node)
  (:documentation "Destructively cuts the node from the tree. Returns
  the tree if successful, nil if the top node is cut."))

(defmethod cut-node ((tree tree) (node tree-node))
  (unless (find node (nodes tree))
    (error "Node ~a not found in tree ~a" node tree))
  (if (eq (top tree) node)
      (progn (setf (top tree) nil
		   (nodes tree) nil)
	     tree)
      (progn 
	(dolist (child (children node))
	  (cut-node tree child))
	(setf (children (parent node)) 
	      (delete node (children (parent node)))
	      (nodes tree) (delete node (nodes tree)))
	tree)))

(defgeneric insert-node (tree node &optional parent)
  (:documentation "Inserts node in tree at where."))

(defmethod insert-node ((tree tree) (node tree-node) &optional (parent (top tree)))
  #+dbg
  (unless (find parent (nodes tree))
    (error "Node ~a not found in tree ~a" parent tree))
  (setf (children node) (children parent)
	(parent node) parent
	(children parent) (list node))
  (dolist (child (children node))
    (setf (parent child) node))
  (push node (nodes tree))
  tree)

(defgeneric depth (node)
  (:documentation "Returns the depth of a certain node (eg. the number of
parent nodes to the top of the tree)."))

(defmethod depth ((node tree-node))
  (loop 
     for current-node = node then (parent current-node)
     while (parent current-node)
     count current-node))

(defgeneric parents (node &key key)
  (:documentation "Traverses the branch of the given node upwards and
  returns these elements in a list (i.e. its parents). The first element will be the
  top (root)."))

(defmethod parents ((node tree-node) &key (key #'identity))
  (let ((parent (parent node)))
    (if parent
      (cons (funcall key parent) (parents parent :key key))
      nil)))

(defgeneric all-children (node &key key)
  (:documentation "Traverses the branch of the given node downwards and
  returns these elements in a list (i.e. all its children)."))

(defmethod all-children ((node tree-node) &key (key #'identity))
  (loop 
   for child in (children node)
   collect (funcall key child)
   append (all-children child :key key)))

;; You could rewrite depth like this but this would require another
;; iteration for length, so it's better (faster) to keep the original
;; depth.

;; (defun depth (node)
;;   (length (parents node)))


;; --------------------------------------------
;; Functions for exporting a tree to dot format
;; --------------------------------------------

(export '(make-s-dot))

(defun string-replace (str1 sub1 sub2)
  "Nondestructively replaces all occurences of sub1 in str1 by sub2"
  (let ((str1 (string str1))
        (str2 "")
        (sub1 (string sub1))
        (sub2 (string sub2))
        (index1 0))
    (loop
       if (string-equal str1 sub1
                        :start1 index1
                        :end1 (min (length str1)
                                   (+ index1 (length sub1))))
       do (setq str2 (concatenate 'string str2 sub2))
         (incf index1 (length sub1))
       else do 
         (setq str2 (concatenate 'string str2
                                 (subseq str1 index1 (1+ index1))))
         (incf index1)
       unless (< index1 (length str1))
       return str2)))

(defun mkdotstr (symbol)
  "Dot cannot handle - in strings so it just replaces all - by _. It
is best to call this function on each symbol or string you pass to
dot."
  (string-replace (format nil "~a" symbol) "-" "_"))

(defgeneric string-for-s-dot (object &key &allow-other-keys)
  (:documentation "Should return a short string (one word) for the
  object so that it fits in a node for exporting to dot."))

(defmethod string-for-s-dot ((node tree-node) &key)
  (mkdotstr (id node)))

(defgeneric make-s-dot (object &key &allow-other-keys)
  (:documentation "Should return a valid s-dot expression for the
  object. If in doubt use s-dot:check-syntax function."))

(defmethod make-s-dot ((tree tree) &key (rankdir "TB"))
  "rankdir is a s-dot command that specifies the orientation of the
tree. Set it to LR to draw from left to right."
  `(s-dot::graph ((s-dot::rankdir ,rankdir))
                 (s-dot::cluster ((s-dot::id "tree")))
                 ,@(loop for node in (nodes tree)
                     append `((s-dot::node ((s-dot::id ,(mkdotstr (id node)))
                                            (s-dot::label ,(string-for-s-dot node))))))
                 ,@(loop for node in (nodes tree)
                      append (loop for child in (children node)
                                collect `(s-dot::edge 
                                          ((s-dot::from ,(mkdotstr (id node)))
                                           (s-dot::to ,(mkdotstr (id child)))))))))


