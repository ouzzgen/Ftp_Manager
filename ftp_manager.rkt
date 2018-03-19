#lang racket/gui

(require "manager_dialog.rkt")


(define manager_frame (new frame%
                   [label "M Editor"]
                   [width 300]
                   [height 300]))
;; (define m-tab (new tab-panel% [choices [new label "new tab"][parent frame]))
(define m-menubar (new menu-bar% [parent manager_frame]))
(define m-menu (new menu%
                     [label "Menu"]
                     [parent m-menubar]))
(define m-open-item (new menu-item%
                         [label "Open"]
                         [parent m-menu]
                         [callback
                          (lambda (menu event) (define abc (get-file)) (send m-text load-file abc))]))
(define m-show-files-item (new menu-item%
                               [label "Show Files"]
                               [parent m-menu]
                               [callback
                                (lambda (menu event) (get-file-list))]))

(define m-save-file-item (new menu-item%
                              [label "Save File"]
                              [parent m-menu]
                              [callback
                               (lambda (menu event)
                                 (define m-file-name (put-file))
                                 (define out (open-output-file m-file-name))
                                 (define m-file-content (send m-text get-text))
                                 (display m-file-content out)
                                 (close-output-port out))]))


(define m-close-item (new menu-item%
                          [label "Close"]
                          [parent m-menu]
                          [callback
                           (lambda (menu event) (send manager_frame on-exit))]))

(define m-edit (new menu%
                    [label "Edit"]
                    [parent m-menubar]))
(define m-cut (new menu-item%
                   [label "Cut"]
                   [parent m-edit]
                   [callback (lambda (menu event) (send m-text cut))]))
(define m-copy (new menu-item%
                    [label "C&opy"]
                    [parent m-edit]
                    [callback
                     (lambda (menu event) (send m-text copy))]))

(define m-select-all (new menu-item%
                          [label "Select All"]
                          [parent m-edit]
                          [callback
                           (lambda (menu event) (send m-text select-all))]))

(define m-paste (new menu-item%
                     [label "Paste"]
                     [parent m-edit]
                     [callback
                      (lambda (menu event) (send m-text paste))]))

;; "clear function" must be search in this item
(define m-clear (new menu-item%
                     [label "Clear"]
                     [parent m-edit]
                     [callback
                      (lambda (menu event) (send m-text clear))]))

(define m-font (new menu%
                    [label "Font"]
                    [parent m-menubar]))
(append-editor-font-menu-items m-font)

(define menu_net_manager (new menu%
                         [label "Net Manager"]
                         [parent m-menubar]))
(define show_net_manager (new menu-item%
                              [label "Show Manager"]
                              [parent menu_net_manager]
                              [callback
                               (lambda (menu event) (send manager_dialog show #t))]))

(define m-editor (new editor-canvas%[parent manager_frame]))
(define m-text (new text%))
(send m-editor set-editor m-text)


;; (send manager_frame show #t)
