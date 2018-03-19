#lang racket/gui
(require net/url)
(require net/ftp)

(provide download-file)
(provide download-ftp-file)
(provide upload-ftp-file)


(define (download-file resFile urlAddress)
  (call-with-output-file resFile
    (lambda (p) (display (port->bytes (get-pure-port (string->url urlAddress))) p))
    #:exists 'replace))

(define (download-ftp-file *server* *remote-dir* *user* *password* *object-file*)
  (let* ([server *server*]
         [remote-dir *remote-dir*]
         [conn (ftp-establish-connection
                server
                21
                *user*
                *password*)])
    (ftp-cd conn remote-dir)
    (ftp-download-file conn "." *object-file*)
    (ftp-close-connection conn)))


(define (upload-ftp-file *server* *remote-dir* *user* *password* *object-file*)
  (let* ([server *server*]
         [remote-dir *remote-dir*]
         [conn (ftp-establish-connection
                server
                21
                *user*
                *password*)])
    (ftp-cd conn remote-dir)
    (ftp-upload-file conn *object-file* #:progress #f)
    (ftp-close-connection conn)))

;; control
(define (upload-ftp-file-with-dialog *server* *remote-dir* *user* *password* *object-file*)
  (let* ([server *server*]
         [remote-dir *remote-dir*]
         [conn (ftp-establish-connection
                server
                21
                *user*
                *password*)])
    (ftp-cd conn remote-dir)
    (set! *object-file* (put-file))
    (ftp-upload-file conn *object-file*)
    (ftp-close-connection conn)))


(define (show-ftp-directory-list *server* *remote-dir* *user* *password*)
  (let* ([server *server*]
         [remote-dir *remote-dir*]
         [conn (ftp-establish-connection
                server
                21
                *user*
                *password*)])
    (ftp-cd conn remote-dir)
    (map
     (lambda (elem) (displayln (string-join elem "\t")))
     (ftp-directory-list conn "."))
    (ftp-close-connection conn)))
