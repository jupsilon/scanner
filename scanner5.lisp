
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
			      (string-value (node-string-value (caddr ,node)))
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

(defmacro when-node-exists ((node-list node-str) &body body)
  `(when (find-if (lambda (node) (string-equal ,node-str (car node))) ,node-list)
     ,@body))

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

(defun concat (&rest args)
  (apply #'concatenate (cons 'string args)))

(defun node-string-value (multiline-str)
  (remove-if (lambda (line) (string= line ""))
	     (mapcar (lambda (line) (ppcre:regex-replace "^\\s+" line ""))
		     (ppcre:split "\\n" multiline-str))))

(defparameter +header-ext+ "hpp")
(defparameter +namespace-identifier+ "wayland_client")
(defun include-guard-identifier (protocol-name)
  (string-upcase (concat "INCLUDE_" protocol-name "_CLIENT_" +header-ext+ "_")))
(defun interface-identifier (interface-name)
  (concat interface-name "_t"))
(defun base-identifier (node)
  (node-let (node interface (name version))
    (concat "wayland_client_impl<" (interface-identifier interface-name) ", " interface-version ">")))
(defun interface-definition (node)
  (node-let (node interface (name version))
    (concat "class " (interface-identifier interface-name) " : public " (base-identifier node))))
(defun move-constructor-definition (interface-name)
  (concat (interface-identifier interface-name) "(" (interface-identifier interface-name) "&& other)"))
(defun event-identifier (event-name)
  event-name)
(defun thunk-identifier (event-name)
  (concat "thunk_" (event-identifier event-name)))
(defun listener-initializer (node-list)
  (format nil "~{~a~^, ~}" (node-let-map (node-list event (name)) (thunk-identifier event-name))))

(defun emit-output-tree (path &aux (input-tree (build-input-tree path)))
  (node-let-map (input-tree protocol (name))
    (<<             "/// @file      " (car (cl-ppcre:split "\\." path)) "." +header-ext+)
    (node-let-map (protocol-children description (summary))
      (<<           "/// @brief     " description-summary))
    (node-let-map (protocol-children copyright ())
      (==           "/// @copyright " copyright-string-value
		    "///            "))
    (node-let-map (protocol-children description ())
      (==           "/// @note      " description-string-value
		    "///            "))
    (<<)
    (<<             "#ifndef " (include-guard-identifier protocol-name))
    (<<             "#define " (include-guard-identifier protocol-name))
    (<<)
    (<<             "namespace " +namespace-identifier+)
    (<<             "{")
    (when-node-exists (protocol-children "interface")
      (indent
	(node-let-map (protocol-children interface (name))
	  (<<         "class " (interface-identifier interface-name) ";"))
	(<<)
	(node-let-map (protocol-children interface (name))
	  (<<         "/// @class " (interface-identifier interface-name))
	  (node-let-map (interface-children description (summary))
	    (<<       "/// @brief " description-summary)
	    (==       "/// @note  " description-string-value
		      "///        "))
	  (<<         "class " (interface-definition interface-node) " {")
	  (<<         "public:")
	  (indent
	    (<<         "using base_type     = " (base-identifier interface-node) ";")
	    (<<         "using c_struct_type = " interface-name ";")
	    (<<))
	  (<<         "public:")
	  (indent
	    (<<         "/// constructor")
	    (<<         "/// @param src source raw pointer")
	    (<<         (interface-identifier interface-name) "(c_struct_type* src)")
	    (indent
	      (<<         ": base_type(src)")
	      (when-node-exists (interface-children "event")
		(<<       ", listener_{ "(listener-initializer interface-children) " }")))
	    (<<         "{")
	    (when-node-exists (interface-children "event")
	      (indent
		(<<        interface-name "_add_listener(this->pointer_, this->listener_, this);")))
	    (<<         "}")
	    (<<)
	    (<<         "/// move constructor")
	    (<<         "/// @param other lvalue-reference")	    
	    (<<         (move-constructor-definition interface-name))
	    (indent
	      (<<         ": base_type(std::move(other))")
	      (when-node-exists (interface-children "event")
		(<<       ", listener_(std::move(other.listener_)")))
	    (<<         "{")
	    (<<         "}")
	    (<<)
	    (<<         "/// destructor")
	    (<<         "~" (interface-identifier interface-name) "() override")
	    (<<         "{")
	    (<<         "}")
	    (<<))
	  (when-node-exists (interface-children "event")
	    (<<       "private:")
	    (indent
	      (<<       (concat interface-name "_listener_t listener_;")))
	    (<<))
	  (<<         "private:")
	  (<<         "};")
	  (<<))))
    (<<             "}")
    (<<)
    (<<             "#endif/*" (include-guard-identifier protocol-name)  "*/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(print (emit-output-tree "xdg-foreign-unstable-v1.xml"))
(print (emit-output-tree "relative-pointer-unstable-v1.xml"))
