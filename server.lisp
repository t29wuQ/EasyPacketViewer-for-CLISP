(defun server ()
    (let ((socket (socket-server 4010))) ;tcp/4010でソケット
    (unwind-protect 
        (loop (with-open-stream (stream (socket-accept socket))
            (parse-method-path (read-line stream))
            (format stream (concatenate 'string (create-http-header) (get-packet-html))))
    ) (socket-server-close socket))))


(defun parse-method-path (str) 
    (let ((method (subseq str  0 (position #\space str))) ;method (GET POST)
          (path (subseq str (+ 2 (position #\space str)) (position #\space str :from-end t)))) ;path
    (print method) (print path)))

(defun create-http-header () 
    (return-from create-http-header "HTTP/1.1 200 ~%Content-Type: text/html;~2% "))

(defun get-packet-html () 
    (with-open-file (stream "packet.html" :direction :input) ;packet.htmlを読み込んで返す
        (let ((buf (make-string (file-length stream))))
            (read-sequence buf stream) (return-from get-packet-html buf))))


;; (print (get-packet-html))
(server)