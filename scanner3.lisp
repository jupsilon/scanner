
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
    (list (remove-empty-values (cxml:parse-file path (cxml-xmls:make-xmls-builder))))))

(defun concat (&rest args)
  (apply #'concatenate (cons 'string args)))

(let ((indent-unit "  ")
      (indent-level 0)
      (output *standard-output*))
  (defun << (&rest args)
    (if (null args)
	(terpri output)
	(progn
	  (dotimes (i indent-level) (princ indent-unit output))
	  (dolist (item args)
	    (princ item output))
	  (terpri output))))
  (defun current-output ()
    output)
  (defun set-indent-unit (new-value)
    (setf indent-unit new-value))
  (defun indent ()
    (incf indent-level))
  (defun unindent ()
    (decf indent-level))
  (defun s/<<-lambda (body)
    (let (prev-output)
      (unwind-protect
	   (with-output-to-string (stream)
	     (setf prev-output output)
	     (setf output stream)
	     (funcall body))
	(setf output prev-output)))))

(defmacro s/<< (&body body)
  `(s/<<-lambda (lambda () ,@body)))

(defmacro indent/<< (&body body)
  `(unwind-protect
	(progn
	  (indent)
	  ,@body)
     (unindent)))

(defun lines-list (multiline-str)
  (remove-if (lambda (line) (string= line ""))
	     (mapcar (lambda (line) (ppcre:regex-replace "^\\s+" line ""))
		     (ppcre:split "\\n" multiline-str))))

(defun == (header-first lines header-rest)
  (<< header-first (car lines))
  (dolist (line (cdr lines))
    (<< header-rest line)))

(defmacro defnode ((nodes node-label attribute-specifiers) &body body)
  `(mapcar
    (lambda (node)
      (symbol-macrolet ((node-attributes (nth 1 node))
			(lines (lines-list (caddr node)))
			(next (cddr node)))
	(let ,(mapcar (lambda (attribute-specifier)
			(list (intern
			       (string-upcase
				(concatenate 'string
					     (string node-label)
					     "-"
					     (string attribute-specifier))))
			      `(cadr (assoc ,(string attribute-specifier) node-attributes :test #'string-equal))))
		      attribute-specifiers)
	  ,@body)))
    (remove-if-not (lambda (node) (string-equal (string ',node-label) (car node))) ,nodes)))

(unintern '+header-ext+)
(defconstant +header-ext+ "hpp")

(unintern '+client-core-template+)
(defconstant +client-core-template+ "interface_core")

(defun client-interface-identifier (interface-name)
  (concat interface-name "_t"))

(defun client-listener-name (interface-name)
  (concat interface-name "_listener_t"))

(defun wire-type (type)
  (cond ((string= type "int"   ) "int32_t"    )
	((string= type "uint"  ) "uint32_t"   )
	((string= type "fixed" ) "wl_fixed_t" )
	((string= type "string") "char const*")
	((string= type "array" ) "wl_array"   )
	((string= type "fd"    ) "int32_t"    )
	(t (error "unknown type ~a" type))))

(let ((xml-path "xdg-foreign-unstable-v1.xml"))
  (princ
   (with-output-to-string (*standard-output*)
     (let ((next (input-tree-from-protocol-xml xml-path)))
       (defnode (next protocol (name))
	 (<<           "/// @file      " protocol-name "." +header-ext+)
	 (defnode (next description (summary))
	   (<<         "/// @brief     " description-summary))
	 (defnode (next copyright ())
	   (==         "/// @copyright " lines
	               "///            "))
	 (defnode (next description ())
	   (==         "/// @note      " lines
	               "///            "))
	 (<<)
	 (<<           "#ifndef INCLUDE_" (string-upcase protocol-name) "_CLIENT_" (string-upcase +header-ext+) "_")
	 (<<           "#define INCLUDE_" (string-upcase protocol-name) "_CLIENT_" (string-upcase +header-ext+) "_")
	 (<<)
	 (<<           "namespace wayland_client_wrappers")
	 (<<           "{")
	 (defnode (next interface (name))
	   (indent/<<
	     (<<         "class " (client-interface-identifier interface-name) ";")))
	 (defnode (next interface (name))
	   (when (defnode (next event ()))
	     (indent/<<
	       (<<       "class " (client-listener-name interface-name) ";"))))
	 (defnode (next interface (name version))
	   (let* ((class-identifier (client-interface-identifier interface-name))
		  (version interface-version)
		  (base-class (concat +client-core-template+ "<" class-identifier ", " version ">")))
	     (indent/<<
	       (<<)
	       (<<       "/// @class " class-identifier)
	       (defnode (next description (summary))
		 (<<     "/// @brief " description-summary)
		 (==     "/// @note  " lines
		         "///        "))
	       (<<       "class " class-identifier " : public " base-class " {")
	       (<<       "public:")
	       (indent/<<
		 (<<       "using base_type = " base-class ";")
		 (<<       "using c_struct_type = " interface-name ";"))
	       (<<)
	       (<<       "public:")
	       (indent/<<
		 (<<       "/// @constructor")
		 (<<       "/// @param src source raw pointer")
		 (<<        class-identifier "(c_struct_type const* src)")
		 (<<       "  : base_type(src)")
		 (<<       "{")
		 (<<       "}"))
	       (when (defnode (next request ()))
		 (<<)
		 (<<     "public:"))
	       (defnode (next request (name type))
		 (indent/<<
		   (defnode (next description (summary))
		     (<<   "/// @brief   " description-summary))
		   (defnode (next arg (name summary type))
		     (unless (string= arg-type "new_id")
		       (<< "/// @param   " arg-name " " arg-summary)))
		   (defnode (next arg (name summary type interface))
		     (when (string= arg-type "new_id")
		       (<< "/// @retval  " arg-summary)
		       (<< "/// @return  " (if arg-interface
					       (client-interface-identifier arg-interface)
					       "void* (you must bind these types later)"))))
		   (defnode (next description ())
		     (==   "/// @note    " lines
			   "///          "))
		   (when (string= request-type "destructor")
		     (<<   "/// @remarks the server destructor"))))
	       (<<       "};"))))
	 (<<           "} // namespace wayland_client_wrappers")
	 (<<)
	 (<<           "#endif/*INCLUDE_" (string-upcase protocol-name) "_CLIENT_" (string-upcase +header-ext+) "_*/"))))))
