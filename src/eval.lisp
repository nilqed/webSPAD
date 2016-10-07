(defpackage webspad
    (:use common-lisp)
    (:documentation "see docs"))

(in-package :webspad)


(defstruct ws-format
    (algebra  boot::|$algebraFormat|)
    (tex      boot::|$texFormat|)
    (html     boot::|$htmlFormat|)
    (mathml   boot::|$mathmlFormat|)
    (formula  boot::|$formulaFormat|)
    (fortran  boot::|$fortranFormat|)
    (texmacs  boot::|$texmacsFormat|)
    (openmath boot::|$openMathFormat|))

;; (type-of x) => SB-IMPL::STRING-OUTPUT-STREAM

(defstruct ws-out-stream
    (algebra  nil)
    (tex      nil)
    (html     nil)
    (mathml   nil)
    (formula  nil)
    (fortran  nil)
    (texmacs  nil)
    (openmath nil))


(defstruct webspad-data
    (input           ""       :type string  )
    (multiline?      nil      :type boolean )
    (spad-2d         ""       :type string  )
    (spad-type       ""       :type string  )
    (algebra         ""       :type string  )
    (tex             ""       :type string  )
    (html            ""       :type string  )
    (mathml          ""       :type string  )
    (formula         ""       :type string  )
    (fortran         ""       :type string  )
    (texmacs         ""       :type string  )
    (openmath        ""       :type string  )
    (format-flags    (make-ws-format)      :type ws-format))


(defun webspad-eval (s)
    
    (setf fmt (make-ws-format)) ;;; ?
    (setf out (make-ws-out-stream))
    
    (setf data (make-webspad-data :format-flags fmt)) ;;; ?
    (setf data (make-webspad-data :input s))
    
    (if (ws-format-algebra fmt) 
        (progn (setf (ws-out-stream-algebra out) boot::|$algebraOutputStream|) 
         (setf boot::|$algebraOutputStream| (make-string-output-stream))))
        
    (if (ws-format-tex fmt) 
        (progn (setf (ws-out-stream-tex out) boot::|$texOutputStream|) 
         (setf boot::|$texOutputStream| (make-string-output-stream))))
          
    (if (ws-format-html fmt) 
        (progn (setf (ws-out-stream-html out) boot::|$htmlOutputStream|) 
         (setf boot::|$htmlOutputStream| (make-string-output-stream))))
          
    (if (ws-format-mathml fmt) 
        (progn (setf (ws-out-stream-mathml out) boot::|$mathmlOutputStream|) 
         (setf boot::|$mathmlOutputStream| (make-string-output-stream))))
                    
    (if (ws-format-formula fmt) 
        (progn (setf (ws-out-stream-formula out) boot::|$formulaOutputStream|) 
         (setf boot::|$formulaOutputStream| (make-string-output-stream))))
          
    (if (ws-format-fortran fmt) 
        (progn (setf (ws-out-stream-fortran out) boot::|$fortranOutputStream|) 
         (setf boot::|$fortranOutputStream| (make-string-output-stream))))
 
    (if (ws-format-texmacs fmt) 
        (progn (setf (ws-out-stream-texmacs out) boot::|$texmacsOutputStream|) 
           (setf boot::|$texmacsOutputStream| (make-string-output-stream))))
          
    (if (ws-format-openmath fmt) 
        (progn (setf (ws-out-stream-openmath out) boot::|$openMathOutputStream|) 
           (setf boot::|$openMathOutputStream| (make-string-output-stream))))
    
    (setf alg (boot::|parseAndEvalToString| s))
 
    (if (ws-format-algebra fmt) 
        (progn (setf (webspad-data-algebra data) 
                   (get-output-stream-string boot::|$algebraOutputStream|))
           (setf boot::|$algebraOutputStream| (ws-out-stream-algebra out))))
                
    (if (ws-format-tex fmt) 
        (progn (setf (webspad-data-tex data) 
                   (get-output-stream-string boot::|$texOutputStream|))
           (setf boot::|$texOutputStream| (ws-out-stream-tex out))))
          
    (if (ws-format-html fmt) 
        (progn (setf (webspad-data-html data)
                   (get-output-stream-string boot::|$htmlOutputStream|))
           (setf boot::|$htmlOutputStream| (ws-out-stream-html out))))
          
    (if (ws-format-mathml fmt) 
        (progn (setf (webspad-data-mathml data)
                   (get-output-stream-string boot::|$mathmlOutputStream|))
           (setf boot::|$matmlOutputStream| (ws-out-stream-mathml out))))
          
    (if (ws-format-formula fmt) 
        (progn (setf (webspad-data-formula data)
                   (get-output-stream-string boot::|$formulaOutputStream|))
           (setf boot::|$formulaOutputStream| (ws-out-stream-formula out))))        
          
    (if (ws-format-fortran fmt) 
        (progn (setf (webspad-data-fortran data)
                   (get-output-stream-string boot::|$fortranOutputStream|))
           (setf boot::|$fortranOutputStream| (ws-out-stream-fortran out))))

    (if (ws-format-texmacs fmt) 
        (progn (setf (webspad-data-texmacs data)
                   (get-output-stream-string boot::|$texmacsOutputStream|))
           (setf boot::|$texmacsOutputStream| (ws-out-stream-texmacs out))))        
          
    (if (ws-format-openmath fmt) 
        (progn (setf (webspad-data-openmath data)
                   (get-output-stream-string boot::|$openMathOutputStream|))
           (setf boot::|$openMathOutputStream| (ws-out-stream-openmath out))))
    
    ;(setf (webspad-data-spad2d data) (get-alg alg))
    ;(setf (webspad-data-spad-type data) (get-type alg))
    (list data alg))
          
 
        
        
        
        
          
(defun webspad-eval-if-texformat (s)
  (let ((*package* (find-package :boot)))
    (let ((tmpout (make-string-output-stream))
          (save boot::|$texOutputStream|))
      (setq boot::|$texOutputStream| tmpout)
      (let ((alg (boot::|parseAndEvalToString| s))
            (tex (get-output-stream-string boot::|$texOutputStream|)))
        (setq boot::|$texOutputStream| save)
            (make-webspad-data :input s
                               :alg-output (get-algform alg)
                               :spad-type  (get-type alg)
                               :tex-output (get-texform tex))
))))

(defun webspad-eval-default (s)
    (let ((alg (boot::|parseAndEvalToString| s)))
        (make-webspad-data :input s
                           :alg-output (get-algform alg)
                           :spad-type  (get-type alg))))

(defvar str '("abc" "def" "ghi" "123456"))
(defvar str2 (format nil "~{~A~%~}" str))

(defun concstr (list)
;; concatenate a list of strings ; recall ~% = newline"
;; +kfp temp solution (move this to utils later)
(if (listp list)
  (with-output-to-string (s)
     (dolist (item list)
       (if (stringp item)
         (format s "~a~%" item))))))

