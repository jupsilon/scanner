;#!/usr/local/bin/sbcl --script

(load "~/.sbclrc")

(require :cl-ppcre)
(require :cl-sxml)

(defun concat (&rest args)
  (apply #'concatenate (cons 'string args)))

(defun << (&rest args)
  (dolist (item args)
    (princ item))
  (terpri))

(defun node-get-label (node)
  (car node))

(defun node-get-attributes (node)
  (mapcar (lambda (item) (cons (car item) (cadr item))) (cadr node)))

(defun attributes-get-value (attributes key)
  (cdr (assoc key attributes :test #'string=)))

(defun node-get-value (node key)
  (attributes-get-value (node-get-attributes node) key))

(defun node-get-nodes (node)
  (cddr node))

(defun node-get-rest (node)
  (let ((last (last (node-get-subnodes node))))
    (when (stringp last)
      last)))

(defun nodes-select (nodes label)
  (remove-if-not (lambda (node) (string= (node-get-label node) label)) nodes))

(defun preprocess (protocol)
	   (remove-if (lambda (item) (if (stringp item) (not (null (ppcre:scan "^\\s+$" item)))))
		      (mapcar (lambda (item)
				(cond ((listp item) (preprocess item))
				      (t item)))
			      protocol)))

(defun attribute (list key)
  (cdr (assoc key
	      (mapcar (lambda (item)
			(unless (= (length item) 2) (error "bad attributes: ~a" list))
			(cons (car item) (cadr item))) list)
	      :test #'string=)))

(defun resolve-comments (node)
  (let (rest
	summary
	text)
    (let ((desc (caddr node)))
      (if (string= (car desc) "description")
	  (progn
	    (setf summary (attribute (cadr desc) "summary"))
	    (setf text    (caddr desc))
	    (setf rest    (cdddr node)))
	  (progn
	    (setf rest (cddr node))))
      (values rest summary text))))

(defun trim-note (note)
  (remove-if (lambda (line) (string= line ""))
	     (mapcar (lambda (line) (ppcre:regex-replace "^\\s+" line ""))
		     (ppcre:split "\\n" note))))

(defun wire-type (type)
  (cond ((string= type "int"   ) "int32_t"    )
	((string= type "uint"  ) "uint32_t"   )
	((string= type "fixed" ) "wl_fixed_t" )
	((string= type "string") "char const*")
	((string= type "array" ) "wl_array"   )
	((string= type "fd"    ) "int32_t"    )
	(t (error "unknown type ~a" type))))

(defun emit-formal-parameters (args)
  (if (null args) "(void) -> void"
      (progn
	(unless (string= (caar args) "arg") (error "missing arg"))
	(let ((retn (find-if   (lambda (arg) (string= (attribute (cadr arg) "type") "new_id")) args))
	      (prms (remove-if (lambda (arg) (string= (attribute (cadr arg) "type") "new_id")) args)))
	  (if (null retn)
	      (setf retn "void")
	      (let ((interface-name (attribute (cadr retn) "interface")))
		(if (null interface-name)
		    (setf retn "void*")
		    (setf retn (concat (attribute (cadr retn) "interface") "_t")))))
	  (if (null prms)
	      (setf prms "(void)")
	      (progn
		(setf prms (mapcar (lambda (arg) (if (string= (attribute (cadr arg) "type") "object")
						     (concat  (attribute (cadr arg) "interface") "* "
							      (attribute (cadr arg) "name"))
						     (concat  (wire-type (attribute (cadr arg) "type")) " "
							      (attribute (cadr arg) "name"))))
				   prms))
		(setf prms (format nil "(~{~a~^, ~})" prms))))
	  (concat prms " -> " retn)))))

(defun emit-client-request-decl (member)
  (let ((name (attribute (cadr member) "name")))
    (multiple-value-bind (args brief note) (resolve-comments member)
      (when (not (null brief))
	(<< "    /// @brief  " brief))
      (when (not (null args))
	(let ((retn (find-if   (lambda (arg) (string= (attribute (cadr arg) "type") "new_id")) args))
	      (prms (remove-if (lambda (arg) (string= (attribute (cadr arg) "type") "new_id")) args)))
	  (dolist (prm prms)
	    (let ((param-name    (attribute (cadr prm) "name"))
		  (param-summary (attribute (cadr prm) "summary")))
	      (if (not (null param-summary))
		  (<< "    /// @param  " param-name " " param-summary)
		  (<< "    /// @param  " param-name))))
	  (when (not (null retn))
	    (let ((return-summary (attribute (cadr retn) "summary")))
	      (<< "    /// @return " return-summary)))))
      (when (not (null note))
	(let ((lines (trim-note note)))
	  (<< "    /// @note   " (car lines))
	  (dolist (rest (cdr lines))
	    (<< "    ///         " rest))))
      (<< "    auto " name (emit-formal-parameters args) ";")
      (terpri))))

(defun emit-client-interface-members (interface-members)
  (dolist (member interface-members)
    (cond ((string= (car member) "request") (emit-client-request-decl member)))))

(defun emit-client-interface (interface)
  (when (string= (car interface) "interface")
    (let* ((name       (attribute (cadr  interface) "name"))
	   (version    (attribute (cadr  interface) "version"))
	   (class      (concat name "_t"))
	   (base-class (concat "wayland_client_core<" class ", " version ">")))
      (multiple-value-bind (members brief note) (resolve-comments interface)
	(when (not (null brief))
	  (<< "  /// @class " class)
	  (<< "  /// @brief " brief " (version " version ")"))
	(when (not (null note))
	  (let ((lines (trim-note note))) (<< "  /// @note  " (car lines))
	       (dolist (rest (cdr lines)) (<< "  ///        " rest))))
	(<< "  class " class " : public " base-class " {" )
	(<< "  public:"                                   )
	(<< "    using protocol_type = " name ";"         )
	(<< "    using base_type     = " base-class ";"   )
	(<< ""                                            )
	(<< "  public:"                                   )
	(<< "    " class "(protocol_type* src = nullptr)" )
	(<< "      : base_typeo(src)"                     )
	(<< "    {"                                       )
	(<< "    }"                                       )
	(<< ""                                            )
	(<< "  public:"                                   )
	(emit-client-interface-members members)
	(<< "  };"                                        )
	(terpri)))))

(defun emit-client-interfaces (interfaces)
  (dolist (interface interfaces)
    (emit-client-interface interface)))

(defun emit-client-namespace (interfaces protocol-name)
  (terpri)
  (<< "namespace " protocol-name "_client")
  (<< "{"                                 )
  (emit-client-interfaces interfaces)
  (<< "}"                                 )
  (terpri))

(defun emit-client-include-guard (interfaces protocol-name)
  (let ((cap-name (string-upcase protocol-name)))
    (<< "#ifndef INCLUDE_" cap-name "_CLIENT_H_")
    (<< "#define INCLUDE_" cap-name "_CLIENT_H_")
    (emit-client-namespace interfaces protocol-name)
    (<< "#endif/*INCLUDE_" cap-name "_CLIENT_H_*/")))

(defun emit (node)
  (let ((protocol-name (node-get-value node "name"))
	(nodes (node-get-nodes node)))
    (let ((copyright (nodes-select nodes "copyright"))
	  (interface (nodes-select nodes "interface")))
      (emit-client-include-guard interface protocol-name)
      (print copyright))))

;(emit (preprocess (cxml:parse      *standard-input*           (cxml-xmls:make-xmls-builder))))
;(emit (preprocess (cxml:parse-file "weston-desktop-shell.xml" (cxml-xmls:make-xmls-builder))))
(emit (preprocess (cxml:parse-file "wayland.xml"              (cxml-xmls:make-xmls-builder))))


