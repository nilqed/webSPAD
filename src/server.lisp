(in-package :boot)


;;;
;;; Config
;;;
(defparameter +port+ 4242)

;;;
;;; Structure
;;;
(defstruct webspad-data
    (input      ""  :type string  )
    (multiline? nil :type boolean )
    (alg-output ""  :type string  )
    (spad-type  ""  :type string  )
    (tex-output ""  :type string  )
    (texmode?   boot::|$texFormat| :type boolean))
    



;;;
;;; Helpers
;;;
(defun concstr (list)
  (if (listp list)
      (with-output-to-string (s)
         (dolist (item list)
           (if (stringp item)
             (format s "~a~%" item))))))

;;;
;;; SPAD eval
;;;
(defun spad_eval (code)
  (let ((*package* (find-package :boot)))
    (boot::concstr (boot::|parseAndEvalToString| code))))



;;;
;;; WEB server
;;;  
(hunchentoot:define-easy-handler (fricas-eval :uri "/eval") (code)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~A~%" (spad_eval code)))
    

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port +port+))

