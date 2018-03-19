#lang racket/gui

(require mrlib/path-dialog)
(require net/url)
(require net/ftp)
(require "ftp_assistant.rkt")

(provide manager_dialog)


(define manager_dialog (new frame%
                   [label "Net Manager Dialog"]
                   [width 300]
                   [height 300]))
(define v-panel (new vertical-panel% [parent manager_dialog]))
(define user-panel (new vertical-panel% [parent v-panel]))
(define h-panel-1 (new horizontal-panel% [parent v-panel]))
(define h-panel-2 (new horizontal-panel% [parent v-panel]))

(define hostname-input (new text-field%
                            [label "Hostname"]
                            [parent user-panel]))

(define username-input (new text-field%
                            [label "username"]
                            [parent user-panel]))

(define password-input (new text-field%
                            [label "password"]
                            [parent user-panel]))

(define remote-dir-input (new text-field%
                            [label "remote-dir"]
                            [parent user-panel]))

(define object-file-input (new text-field%
                            [label "object-file-input"]
                            [parent user-panel]))

(define file-list-box (new list-box%
                           [parent user-panel]
                           [label "Files"]
                           [choices (list "")]
                           [style (list 'variable-columns
                                        'single
                                        'column-headers
                                        'clickable-headers)]
                           [columns (list "Files")]))

(define show-ftp-create-directory-button (new button%
                               [parent h-panel-1]
                               [label "Make directory"]
                               [callback
                                (lambda (button event) (cb-show-create-directory))]))

(define ftp-delete-directory-button (new button%
                                         [parent h-panel-2]
                                         [label "Delete Directory"]
                                         [callback
                                          (lambda (button event) (cb-delete-directory))]))
(define show-rename-file-button (new button%
                                     [parent h-panel-2]
                                     [label "Rename File"]
                                     [callback
                                      (lambda (button event) (cb-show-rename-file))]))


(define ftp-cd-button (new button%
                           [parent h-panel-1]
                           [label "Cd .."]
                           [callback
                            (lambda (button event) (change-directory))]))


(define ftp-download-button (new button%
                                [parent h-panel-1]
                                [label "Download"]
                                [callback
                                 (lambda (button event) (activate-downloading))]))

(define ftp-download-button-wd (new button%
                                [parent h-panel-1]
                                [label "Download wd"]
                                [callback
                                 (lambda (button event) (activate-downloading-with-dialog))]))

(define ftp-upload-button (new button%
                                [parent h-panel-1]
                                [label "Upload"]
                                [callback
                                 (lambda (button event) (activate-uploading-with-dialog))]))

(define ftp-upload-button-wd (new button%
                                [parent h-panel-1]
                                [label "Upload wd"]
                                [callback
                                 (lambda (button event) (activate-uploading-with-dialog))]))

(define ftp-list-files (new button%
                            [parent h-panel-1]
                            [label "List Files"]
                            [callback
                             (lambda (button event) (show-ftp-directory-list-fl))]
                            ))


(define exit-button (new button%
                         [parent h-panel-1]
                         [label "Exit"]
                         [callback
                          (lambda (button event) (send manager_dialog on-exit))]))

(define (activate-downloading)
  (let* ([usr (send username-input get-value)]
         [hstname (send hostname-input get-value)]
         [psswrd (send password-input get-value)]
         [object-file (send object-file-input get-value)]
         [rmt-dir (send remote-dir-input get-value)])
    (download-ftp-file hstname rmt-dir usr psswrd object-file)))


;; control
(define (activate-downloading-with-dialog)
  ;; (show-ftp-directory-list-fl)
    
  (let* ([elem-num (send file-list-box get-selection)]
         [elem-data-str (send file-list-box get-string elem-num)]
         [elem-list (string-split elem-data-str "\t")]
         [elem-file (list-ref elem-list 2)]
         [usr (send username-input get-value)]
         [hstname (send hostname-input get-value)]
         [psswrd (send password-input get-value)]
         [rmt-dir (send remote-dir-input get-value)]
         [conn (ftp-establish-connection hstname 21 usr psswrd)]
         ;; [object-file (get-file)])
         [object-file elem-file])
    (ftp-cd conn rmt-dir)    
    (download-ftp-file hstname rmt-dir usr psswrd object-file)
    (ftp-close-connection conn)))


(define (activate-uploading)
  (let* ([usr (send username-input get-value)]
         [hstname (send hostname-input get-value)]
         [psswrd (send password-input get-value)]
         [object-file (send object-file-input get-value)]
         [rmt-dir (send remote-dir-input get-value)])
    (upload-ftp-file hstname rmt-dir usr psswrd object-file)))

(define (activate-uploading-with-dialog)
  (let* ([usr (send username-input get-value)]
         [hstname (send hostname-input get-value)]
         [psswrd (send password-input get-value)]
         [object-file (get-file)]
         [rmt-dir (send remote-dir-input get-value)])
    (upload-ftp-file hstname rmt-dir usr psswrd object-file)))


(define (show-ftp-directory-list-fl)
  (let* ([*server* (send hostname-input get-value)]
         [*remote-dir* (send remote-dir-input get-value)]
         [*user* (send username-input get-value)]
         [*password* (send password-input get-value)]
         [conn (ftp-establish-connection
                *server*
                21
                *user*
                *password*)])
    (send file-list-box clear)
    (ftp-cd conn *remote-dir*)
    (map
     (lambda (elem) (send file-list-box append (string-join elem "\t")))
     (ftp-directory-list conn "."))
    (ftp-close-connection conn)))

;; control
(define (change-directory)
    (let* ([elem-num (send file-list-box get-selection)]
           [elem-data-str (send file-list-box get-string elem-num)]
           [elem-list (string-split elem-data-str "\t")]
           [elem-file (list-ref elem-list 2)]
           [elem-file-str (string-append elem-file "/")]
           [*server* (send hostname-input get-value)]
           [*remote-dir* (send remote-dir-input get-value)]
           [*user* (send username-input get-value)]
           [*password* (send password-input get-value)]
           [conn (ftp-establish-connection
                  *server*
                  21
                  *user*
                  *password*)])
      (send file-list-box clear)
      (ftp-cd conn *remote-dir*)
      (ftp-cd conn elem-file-str)
      (map
       (lambda (elem) (send file-list-box append (string-join elem "\t")))
       (ftp-directory-list conn "."))
      (ftp-close-connection conn)))

(define make-dir-frame (new frame% [label "Make dir"] [width 100] [height 100]))
(define dir-name-input (new text-field% [label "dir name"] [parent make-dir-frame]))
(define new-dir-button (new button% [label "Close"] [parent make-dir-frame]
                            [callback (lambda (button event) (cb-create-directory))]))

(define rename-file-frame (new frame% [label "New File"] [width 100] [height 100]))
(define old-file-name-input (new text-field% [label "old name"] [parent rename-file-frame]))
;; this button void
(define choose-file-button (new button% [label "choose file"] [parent rename-file-frame]))
(define new-file-name-input (new text-field% [label "new name"] [parent rename-file-frame]))
(define rename-file-button (new button% [label "new file"] [parent rename-file-frame]
                                [callback (lambda (button event) (cb-rename-file))]))

(define (cb-create-directory)
  (let* ([new-dir-name (send dir-name-input get-value)]
         [elem-num (send file-list-box get-selection)]
         [elem-data-str (send file-list-box get-string elem-num)]
         [elem-list (string-split elem-data-str "\t")]
         [elem-file (list-ref elem-list 2)]
         [elem-file-str (string-append elem-file "/")]
         [*server* (send hostname-input get-value)]
         [*remote-dir* (send remote-dir-input get-value)]
         [*user* (send username-input get-value)]
         [*password* (send password-input get-value)]
         [conn (ftp-establish-connection
                *server*
                21
                *user*
                *password*)])
    (send file-list-box clear)
    (ftp-cd conn *remote-dir*)
    (ftp-make-directory conn new-dir-name)
    (ftp-close-connection conn)))

(define (cb-delete-directory)
  (let* ([elem-num (send file-list-box get-selection)]
         [elem-data-str (send file-list-box get-string elem-num)]
         [elem-list (string-split elem-data-str "\t")]
         [elem-dir-name (list-ref elem-list 2)]
         ;; [elem-dir-name-str (string-append elem-dir-name "/")]
         [*server* (send hostname-input get-value)]
         [*remote-dir* (send remote-dir-input get-value)]
         [*user* (send username-input get-value)]
         [*password* (send password-input get-value)]
         [conn (ftp-establish-connection
                *server*
                21
                *user*
                *password*)])
    ;; necessary?? (send file-list-box clear)
    (ftp-cd conn *remote-dir*)
    (ftp-delete-directory conn elem-dir-name)
    (ftp-close-connection conn)))

(define (cb-show-create-directory)
  (send make-dir-frame show #t))

(define (cb-show-rename-file)
  (send rename-file-frame show #t))

(define (cb-rename-file)
  (let* ([new-file-name (send new-file-name-input get-value)]
         [elem-num (send file-list-box get-selection)]
         [elem-data-str (send file-list-box get-string elem-num)]
         [elem-list (string-split elem-data-str "\t")]
         [elem-old-file (list-ref elem-list 2)]
         ;; [elem-old-file-str (string-append elem-file "/")]
         [*server* (send hostname-input get-value)]
         [*remote-dir* (send remote-dir-input get-value)]
         [*user* (send username-input get-value)]
         [*password* (send password-input get-value)]
         [conn (ftp-establish-connection
                *server*
                21
                *user*
                *password*)])
    ;; ?? (send file-list-box clear)
    (send old-file-name-input set-value elem-old-file)
    (ftp-cd conn *remote-dir*)
    (ftp-rename-file conn elem-old-file new-file-name)
    (ftp-close-connection conn)))
   
