;; working with streams, pages 237 - 251

;; server code used to recreate of Land of Lisp's sockets demonstation on pages
;; 245 - 249, using the usocket library
;;
;; for more information, see:
;;
;;     https://github.com/usocket/usocket
;;     https://common-lisp.net/project/usocket/api-docs.shtml
;;     https://gist.github.com/shortsightedsid/71cf34282dfae0dd2528

;; assumes availability of quicklisp to load usocket
(ql:quickload "usocket")

;; set up connection
(defparameter server-socket (usocket:socket-listen "127.0.0.1" 4321))
(defparameter server-connection (usocket:socket-accept server-socket :element-type 'character))

;; wait for message from client
(usocket:wait-for-input server-connection)
(format t "~A~%" (read-line (usocket:socket-stream server-connection)))

;; write message to client
(format (usocket:socket-stream server-connection) "What up, Client!~%")
(force-output (usocket:socket-stream server-connection))

;; close connections
(usocket:socket-close server-connection)
(usocket:socket-close server-socket)
