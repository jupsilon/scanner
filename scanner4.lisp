
(load "~/.sbclrc")

(require :cl-ppcre)
(require :cl-sxml)

(defun concat (&rest args)
  (apply #'concatenate (cons 'string args)))

(let ((indent-unit "  ")
      (indent-level 0))
  (defun << (&rest args)
    (if (null args)
	(terpri)
	(progn
	  (loop repeat indent-level do (princ indent-unit))
	  (dolist (item args)
	    (princ item))
	  (terpri))))
  (defun set-indent-unit (new-value)
    (setf indent-unit new-value))
  (defun do-indent ()
    (incf indent-level))
  (defun do-unindent ()
    (decf indent-level)))

(defmacro indent (&body body)
  `(unwind-protect
	(progn
	  (do-indent)
	  ,@body)
     (do-unindent)))

(defun == (header-first lines header-rest)
  (<< header-first (car lines))
  (dolist (line (cdr lines))
    (<< header-rest line)))

(defun lines-list (multiline-str)
  (remove-if (lambda (line) (string= line ""))
	     (mapcar (lambda (line) (ppcre:regex-replace "^\\s+" line ""))
		     (ppcre:split "\\n" multiline-str))))

(defmacro when-node-exists ((node-label) &body body)
  `(when (find-if (lambda (node) (string-equal (string ',node-label) (car node))) next)
     ,@body))

(defmacro node-let ((node-label attribute-specifiers &rest bindings) &body body)
  `(format t "~{~a~^~&~}"
	   (mapcar
	    (lambda (node)
	      (symbol-macrolet ((node-attributes (nth 1 node))
				(lines (lines-list (caddr node)))
				(next (cddr node)))
		(let* ,(append (mapcar (lambda (attribute-specifier)
					 (list (intern
						(string-upcase
						 (concatenate 'string
							      (string node-label)
							      "-"
							      (string attribute-specifier))))
					       `(cadr (assoc ,(string attribute-specifier) node-attributes
							     :test #'string-equal))))
				       attribute-specifiers)
			       bindings)
		  (with-output-to-string (*standard-output*)
		    ,@body))))
	    (remove-if-not (lambda (node) (string-equal (string ',node-label) (car node))) next))))

(defmacro node-let-0 ((node-label attribute-specifiers &rest bindings) &body body)
  `(format nil "~{~a~^, ~}"
	   (mapcar
	    (lambda (node)
	      (symbol-macrolet ((node-attributes (nth 1 node))
				(lines (lines-list (caddr node)))
				(next (cddr node)))
		(let* ,(append (mapcar (lambda (attribute-specifier)
					 (list (intern
						(string-upcase
						 (concatenate 'string
							      (string node-label)
							      "-"
							      (string attribute-specifier))))
					       `(cadr (assoc ,(string attribute-specifier) node-attributes
							     :test #'string-equal))))
				       attribute-specifiers)
			       bindings)
		  (with-output-to-string (*standard-output*)
		    ,@body))))
	    (remove-if-not (lambda (node) (string-equal (string ',node-label) (car node))) next))))

(defmacro node-let-2 ((node-label attribute-specifiers &rest bindings) &body body)
  `(format t "~{~a~^~%~}"
	   (mapcar
	    (lambda (node)
	      (symbol-macrolet ((node-attributes (nth 1 node))
				(lines (lines-list (caddr node)))
				(next (cddr node)))
		(let* ,(append (mapcar (lambda (attribute-specifier)
					 (list (intern
						(string-upcase
						 (concatenate 'string
							      (string node-label)
							      "-"
							      (string attribute-specifier))))
					       `(cadr (assoc ,(string attribute-specifier) node-attributes
							     :test #'string-equal))))
				       attribute-specifiers)
			       bindings)
		  (with-output-to-string (*standard-output*)
		    ,@body))))
	    (remove-if-not (lambda (node) (string-equal (string ',node-label) (car node))) next))))

(defun build-input-tree (path)
  (labels ((remove-empty-values (protocol)
	     (remove-if (lambda (item) (and (stringp item) (ppcre:scan "^\\s+$" item)))
			(mapcar (lambda (item)
				  (if (listp item)
				      (remove-empty-values item)
				      item))
				protocol))))
    (list (remove-empty-values (cxml:parse-file path (cxml-xmls:make-xmls-builder))))))

(defun client-interface-identifier (interface-name)
  (concat interface-name "_t"))

(defun wire-type (type)
  (cond ((string= type "int"   ) "int32_t"    )
	((string= type "uint"  ) "uint32_t"   )
	((string= type "fixed" ) "wl_fixed_t" )
	((string= type "string") "char const*")
	((string= type "array" ) "wl_array"   )
	((string= type "fd"    ) "int32_t"    )
	(t (error "unknown type ~a" type))))

(defun c++-function-with-formal-parameters-list (method-node)
  (concat 
   (node-let-0 (arg (name type interface))
     (princ arg-type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((next (build-input-tree "xdg-foreign-unstable-v1.xml")))
  (node-let (protocol (name))
    (<<           "/// @file      " protocol-name)
    (node-let (description (summary))
      (<<         "/// @brief     " description-summary))
    (node-let (copyright ())
      (==         "/// @copyright " lines
		  "///            "))
    (node-let (description ())
      (==         "/// @note      " lines
		  "///            "))
    (<<)
    (<<           "#ifndef INCLUDE_" (string-upcase protocol-name) "_CLIENT_HPP_")
    (<<           "#define INCLUDE_" (string-upcase protocol-name) "_CLIENT_HPP_")
    (<<)
    (<<           "namespace wayland_client")
    (<<           "{")
    (indent
      (node-let (interface (name))
	(<<         "class " (client-interface-identifier interface-name) ";"))
      (node-let (interface
		 (name version)
		 (class-identifier (client-interface-identifier interface-name))
		 (version interface-version)
		 (base-identifier (concat "client_wrapper<" class-identifier ", " version ">")))
	(<<)
	(<<         "/// @class "class-identifier)
	(node-let (description (summary))
	  (<<       "/// @brief " description-summary)
	  (==       "/// @note  " lines
		    "///        "))
	(<<         "class " class-identifier " : public " base-identifier " {")
	(<<         "public:")
	(indent
	  (<<         "using base_type     = " base-identifier ";")
	  (<<         "using c_struct_type = " interface-name ";"))
	(<<)
	(<<         "public:")
	(indent
	  (<<         "/// @constructor")
	  (<<         "/// @param src source raw pointer")
	  (<<          class-identifier "(c_struct_type const* src)")
	  (<<         "  : base_type(src)")
	  (<<         "{")
	  (<<         "}"))
	(when-node-exists (request)
	  (<<)
	  (<<       "public:")
	  (indent
	    (node-let-2 (request (name type))
	      (<<     "/// request method for " interface-name "::" request-name)
	      (node-let (description (summary))
		(<<   "/// @brief  " description-summary))
	      (node-let (arg (name summary type))
		(unless (string= arg-type "new_id")
		  (<< "/// @param  " arg-name " " arg-summary)))
	      (node-let (arg (summary type interface))
		(when (string= arg-type "new_id")
		  (<< "/// @return " (if arg-interface
					 (client-interface-identifier arg-interface)
					 "void*"))
		  (<< "/// @retval " arg-summary)))
	      (node-let (description ())
		(==   "/// @note   " lines
		      "///         "))
	      (<<    request-name "(" (node-let-0 (arg (type)) (princ arg-type)) ");")
	      (<<     "// ------------------------------------------------------- (method decl)"))))
	(when-node-exists (event)
	  (<<)
	  (<<       "public:")
	  (indent
	    (node-let-2 (event (name type))
	      (<<     "/// event handler for " interface-name "_listener::" event-name)
	      (node-let (description (summary))
		(<<   "/// @brief  " description-summary))
	      (node-let (arg (name summary type))
		(unless (string= arg-type "new_id")
		  (<< "/// @param  " arg-name " " arg-summary)))
	      (node-let (arg (summary type interface))
		(when (string= arg-type "new_id")
		  (<< "/// @return " (if arg-interface
					 (client-interface-identifier arg-interface)
					 "void*"))
		  (<< "/// @retval " arg-summary)))
	      (node-let (description ())
		(==   "/// @note   " lines
		      "///         "))
	      (<<     "// ------------------------------------------------------- (event decl)"))))
	(<<         "};")))

    (<<           "} // namespace wayland_client")
    (<<)
    (<<         "#endif/*INCLUDE_" (string-upcase protocol-name) "_CLIENT_HPP_*/")))
