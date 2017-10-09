
(load "~/.sbclrc")

(require :cl-ppcre)
(require :cl-sxml)

(defun input-tree-from-protocol-xml (path)
  (labels ((remove-empty-values (protocol)
	     (remove-if (lambda (item) (if (stringp item) (not (null (ppcre:scan "^\\s+$" item)))))
			(mapcar (lambda (item)
				  (if (listp item)
				      (remove-empty-values item)
				      item))
				protocol))))
    (remove-empty-values (cxml:parse-file path (cxml-xmls:make-xmls-builder)))))

(setf foreign
      (let* ((root (input-tree-from-protocol-xml "xdg-foreign-unstable-v1.xml")))
	(print root)))

;(let* ((root (input-tree-from-protocol-xml "xdg-foreign-unstable-v1.xml")))
;  (traverse (root) (type)))

