(defun server ()
    (let ((socket (socket-server 4010))) ;tcp/4010でソケット
    (unwind-protect 
        (loop (with-open-stream (stream (socket-accept socket))
            (let ((access (parse-method-path (read-line stream))))
                    (router stream (car access) (cdr access))))
    ) (socket-server-close socket))))


(defun parse-method-path (str) ;methodとpathを取得
    (let ((method (subseq str  0 (position #\space str))) ;method (GET POST)
          (path (subseq str (+ 2 (position #\space str)) (position #\space str :from-end t)))) ;path
    (return-from parse-method-path (cons method path)) ))

(defun create-http-header ()  ;http headerを返すだけ
    (return-from create-http-header "HTTP/1.1 200 ~%Content-Type: text/html;~2% "))

(defun router (stream method path) ;pathから処理関数を選択
    (cond ((equal path "packet")  
            (res-packet stream method))))

(defun res-packet (stream method) ;/pathの処理
    (cond ((equal method "GET") ;GET
            (format stream (concatenate 'string (create-http-header) (get-html "packet.html"))))
            ((equal method "POST") ;POST
                (loop (cond ((equal (read-line stream) "Content-Disposition: form-data; name=\"data\"")
                            (read-line stream)
                            (format stream (concatenate 'string "HTTP/1.1 200 ~%Content-Type: application/json;~2%" (packet-parser (read-line stream))))
                            (return-from res-packet))))
                ) ))

(defun get-html (name) ;html読み込む  name : filename
    (with-open-file (stream name :direction :input) 
        (let ((buf (make-string (file-length stream))))
            (read-sequence buf stream) (return-from get-html buf))))

(defun packet-parser (packet) ;packetのパーサー jsonにして返す
    (let ((json (make-string-output-stream))
            (header-length (subseq packet 1 2))
            (protocol (subseq packet 18 20)))
        (princ "{" json)
        (princ "\"version\":\"" json)
        (princ (subseq packet 0 1) json)
        (princ "\"," json)
        (princ "\"headerlength\":\"" json)
        (princ header-length json)
        (princ "\"," json)
        (princ "\"service\":\"" json)
        (princ (subseq packet 2 4) json)
        (princ "\"," json)
        (princ "\"packetlength\":\"" json)
        (princ (subseq packet 4 8) json)
        (princ "\"," json)
        (princ "\"identifier\":\"" json)
        (princ (subseq packet 8 12) json)
        (princ "\"," json)
        (princ "\"flag\":\"" json)
        (princ (subseq packet 12 16) json)
        (princ "\"," json)
        (princ "\"TTL\":\"" json)
        (princ (subseq packet 16 18) json)
        (princ "\"," json)
        (princ "\"protocol\":\"" json)
        (princ protocol json)
        (princ "\"," json)
        (princ "\"checksum\":\"" json)
        (princ (subseq packet 20 24) json)
        (princ "\"," json)
        (princ "\"source\":\"" json)
        (princ (convert-ip-format (subseq packet 24 32)) json)
        (princ "\"," json)
        (princ "\"destination\":\"" json)
        (princ (convert-ip-format (subseq packet 32 40)) json)
        (princ "\"," json)
        (princ "\"option\":\"" json)
        (let ((start (* (parse-integer header-length) 8)))
            (if (< 40 start) 
            (princ (subseq packet 40 start)  json))
            (princ "\"," json)
            (princ "\"body\":\"" json)
            (princ (subseq packet start)  json))
        (princ "\"," json)
        (princ "\"protocolname\":\"" json)
        (cond ((equal protocol "01") (princ "ICMP" json))
                ((equal protocol "11") (princ "NVP-II" json))
                ((equal protocol "06") (princ "TCP" json))
                ((equal protocol "17") (princ "UDP" json))
                )
        (princ "\"" json)
        (princ "}" json)
        (return-from packet-parser (get-output-stream-string json))))

(defun convert-ip-format (raw-address) ;ipアドレスを見やすい形に加工 
    (return-from convert-ip-format 
        (concatenate 'string (write-to-string (parse-integer (subseq raw-address 0 2) :radix 16)) 
                    "." 
                    (write-to-string (parse-integer (subseq raw-address 2 4) :radix 16)) 
                    "." 
                    (write-to-string (parse-integer (subseq raw-address 4 6) :radix 16)) 
                    "." 
                    (write-to-string (parse-integer (subseq raw-address 7 8) :radix 16)))))

(server)
