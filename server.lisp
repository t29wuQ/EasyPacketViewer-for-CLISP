(defun server ()
    (let ((socket (socket-server 4010))) ;tcp/4010で鯖立て
    (unwind-protect 
        (loop (with-open-stream (stream (socket-accept socket))
            (read stream))
    ) (socket-server-close socket))))


(server)