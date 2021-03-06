**webSPAD** is a project in progress and its goal is to provide web 
services for **FriCAS** and friends. Eventually it shall serve to 
write a new Jupyter kernel for those systems. 

The documentation sources are in the folder ``sphinx`` whereas the built
HTML files will go to ``docs`` (GitHub pages:
https://nilqed.github.io/webSPAD). 

Below is an excerpt from the appendix in order to demonstrate the idea.

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
  ;;; test example: http://localhost:4242/eval?code=D(x^n,x,6)  
  
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
:Source: http://www.w3schools.com/TAGS/ref_httpmethods.asp

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
URL is limited (maximum URL length is **2048** characters and ``ASCII`` 
characters only are allowed.

Certain characters have a special meaning in an URL, so url encoding must 
follow some `rules`_ (HTML URL Encoding Reference).

.. _rules: http://www.w3schools.com/tags/ref_urlencode.asp

There are some `online`_ URL **en/decoders** which will encode more complex 
input, for instance::
    
    integrate(1/(1+x^2),x)
    
reads in encoded form as::
    
    integrate(1%2F(1%2Bx%5E2)%2Cx)
    
The latter string can be used in a url, whereas the former will not be
accepted (e.g. due to the plus character ``+``).

.. _online: http://meyerweb.com/eric/tools/dencoder/
    

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

.. code:: python

  import requests
  url = 'http://localhost:4242/eval'
  payload = {'code': 'D(x^n,x,8)'}

  r = requests.post(url, data=payload)

  print(r.text)

  
Live action::
    
    
    C:\Users\nilqed>python
    Python 2.7.10 (default, May 23 2015, 09:40:32) [MSC v.1500 32 bit (Intel)]
    Type "help", "copyright", "credits" or "license" for more information.
    >>> import requests
    >>> url = 'http://localhost:4242/eval'
    >>> payload = {'code': 'D(x^n,x,8)'}
    >>> r = requests.post(url, data=payload)
    >>> r
    <Response [200]>
    >>> print(r.text)
    
         8      7       6        5        4         3         2          n - 8
       (n  - 28n  + 322n  - 1960n  + 6769n  - 13132n  + 13068n  - 5040n)x
                                                       
                                                      Type: Expression(Integer)
    
    
    >>>
  
With the POST method we can use high *payloads* and there are many languages 
with HTTP support libraries. 

Conclusion
----------
This skeleton example shows the general method and of course only uses the 
most basic features of *Hunchentoot*. We can define many more easy handlers 
and other - even more sophisticated - methods to access the internals of 
*FriCAS* by HTTP requests. The sky is the limit ;-) 
