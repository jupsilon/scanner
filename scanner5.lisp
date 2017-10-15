
(load "~/.sbclrc")

(require :cl-ppcre)
(require :cl-sxml)

(defun build-input-tree (path)
  (labels ((remove-empty-values (protocol)
	     (remove-if (lambda (item) (and (stringp item) (ppcre:scan "^\\s+$" item)))
			(mapcar (lambda (item)
				  (if (listp item)
				      (remove-empty-values item)
				      item))
				protocol))))
    (list (remove-empty-values (cxml:parse-file path (cxml-xmls:make-xmls-builder))))))

(defmacro symbol-macrolet-prefixed ((prefix &rest bindings) &body body)
  `(symbol-macrolet
       ,(mapcar
	 (lambda (binding)
	   (cons (intern
		  (string-upcase
		   (concatenate 'string
				(string prefix)
				"-"
				(string (car binding)))))
		 (cdr binding)))
	 bindings)
     ,@body))

(defmacro node-let ((node node-label attribute-specifiers) &body body)
  `(symbol-macrolet-prefixed (,node-label
			      (node ,node)
			      (attributes (nth 1 ,node))
			      (string-values (node-string-lines (caddr ,node)))
			      (children (cddr ,node))
			      ,@(mapcar (lambda (attribute-specifier)
					  (list attribute-specifier
						`(cadr (assoc ,(string attribute-specifier) (nth 1 ,node)
							      :test #'string-equal))))
					attribute-specifiers))
     ,@body))

(defmacro node-let-map ((node-list node-label attribute-specifiers) &body body)
  `(mapcar (lambda (node)
	    (node-let (node ,node-label ,attribute-specifiers)
	      ,@body))
	   (remove-if-not (lambda (node) (string-equal (string ',node-label) (car node))) ,node-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((root (build-input-tree "wayland.xml")))
  (print (node-let-map (root protocol (name))
	   (node-let-map (protocol-children interface (name))
	     interface-name))))
