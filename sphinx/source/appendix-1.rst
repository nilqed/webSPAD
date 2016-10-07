.. example of a minimal HT embedding

================================
Minimal embedding of Hunchentoot
================================
The following code provides a concise example how to embed *Hunchentoot*
into a running *FriCAS* instance. The ``GET`` and ``POST`` method will be
demonstrated by using urls in a web-browser and a short *Python* code 
respectively.

The code
--------
Paste the following code into a file ``minserver.lisp``
    
.. code::

  (load "~/quicklisp/setup")
  (ql:quickload :hunchentoot)

  ;;; test: http://localhost:4242/eval?code=D(x^n,x,6)

  (in-package :boot)

  ;;; Config
  (defparameter +port+ 4242)

  ;;; SPAD eval

  (defun spad_eval (code)
    (let ((*package* (find-package :boot))
          (alg (boot::|parseAndEvalToString| code)))
            (format nil "~{~A~%~}" alg)))   
    
  ;;; WEB server
  (hunchentoot:define-easy-handler (fricas-eval :uri "/eval") (code)
    (setf (hunchentoot:content-type*) "text/plain")
      (format nil "~A~%" (spad_eval code)))
    
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port +port+))

  ;;; add :address "localhost"  if you wish local access only!
  
  
Load server
-----------
We will do all steps manually for the sake of clarity. Start *FriCAS* and
load the file ``minserver.lisp``::
    
    $ fricas
    
                               FriCAS Computer Algebra System
                             Version: FriCAS 2016-08-28
                      Timestamp: Sam Sep 17 00:34:49 CEST 2016
    ---------------------------------------------------------------------------
       Issue )copyright to view copyright notices.
       Issue )summary for a summary of useful system commands.
       Issue )quit to leave FriCAS and return to shell.
    ---------------------------------------------------------------------------
    
    (1) -> )lisp (load "minserver")
    To load "hunchentoot":
      Load 1 ASDF system:
        hunchentoot
    ; Loading "hunchentoot"
    ............
    Value = T
    (1) ->
    
    
Open a web browser
------------------
Now open a web browser (some terminal based browsers like *w3m*, *lynx* or
*links*, do not accept all urls as e.g. Firefox does, however, when using
quotes most urls will work). Enter the following url::
    
    http://localhost:4242/eval?code=D(x^n,x,6)
    
The result in the browser window should exactly look like as below::
    
     6      5      4       3       2         n - 6
   (n  - 15n  + 85n  - 225n  + 274n  - 120n)x
                                                    Type: Expression(Integer)

                                                    
    
Explanation
-----------
The connection of the url example above with the lisp code is almost obvious::
    
    (fricas-eval :uri "/eval") (code)
    
The easy handler *expects* a query ``?`` and one variable ``code``. Then this
query will be evaluated and the result formatted and written to the client.


GET and POST
------------
:Source: http://www.w3schools.com/TAGS/ref_httpmethods.asp) 

Two commonly used methods for a request-response between a client and server 
are::
    
    GET and POST.

    GET - Requests data from a specified resource
    POST - Submits data to be processed to a specified resource
    
The GET Method
~~~~~~~~~~~~~~
Note that the query string (name/value pairs) is sent in the URL of a GET 
request::

    /test/demo_form.asp?name1=value1&name2=value2 


:Restrictions: The GET method adds the data to the URL; and the length of a 
URL is limited (maximum URL length is 2048 characters and ``ASCII`` characters 
only are allowed.


The POST Method
~~~~~~~~~~~~~~~
**Note** that the query string (name/value pairs) is sent in the HTTP message 
body of a POST request::
    
    POST /test/demo_form.asp HTTP/1.1
    Host: w3schools.com
    name1=value1&name2=value2
    
The restrictions of the GET method do not apply here, however, to get a full
overview consult the link above. There also are other HTTP request 
possibilities.

Python example for POST
-----------------------

