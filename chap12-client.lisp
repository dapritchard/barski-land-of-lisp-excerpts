;; working with streams, pages 237 - 251

;; client code used to recreate of Land of Lisp's sockets demonstation on pages
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
(defparameter client-socket (usocket:socket-connect "127.0.0.1" 4321 :element-type 'character))

;; write message to server
(format (usocket:socket-stream client-socket) "Yo Server!~%")
(force-output (usocket:socket-stream client-socket))

;; wait for message from server
(usocket:wait-for-input client-socket)
(format t "~A~%" (read-line (usocket:socket-stream client-socket)))

;; close connection
(usocket:socket-close client-socket)
