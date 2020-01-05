(defun server ()
    (let ((socket (socket-server 4010))) ;tcp/4010でソケット
    (unwind-protect 
        (loop (with-open-stream (stream (socket-accept socket))
            (parse-method-path (read-line stream))
            (format stream "HTTP/1.1 404 ~%Content-Type: text/html;~2%<html><head><title>test</title></head><body>404 Not Found</body></html>"))
    ) (socket-server-close socket))))


(defun parse-method-path (str) 
    (let ((method (subseq str  0 (position #\space str))) ;method (GET POST)
          (path (subseq str (+ 2 (position #\space str)) (position #\space str :from-end t)))) ;path
    (print method) (print path)))



(server)