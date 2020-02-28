;;-*-Emacs-Lisp-*-

;; Determine where we are.
(defvar system-type-as-string (prin1-to-string system-type))
(defvar on_windows_nt (string-match "windows-nt" system-type-as-string))
(defvar on_darwin     (string-match "darwin"     system-type-as-string))

;; Don't ring the bell.
(setq-default visible-bell t)

;; Adjust colors
(add-to-list 'default-frame-alist '(background-color . "black"))
(add-to-list 'default-frame-alist '(foreground-color . "white"))
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(set-foreground-color "white")
(set-background-color "black")

;; Maximize (perhaps).
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Adjust fonts.
(cond (on_darwin
       (add-to-list 'default-frame-alist
                    '(font . "-apple-monaco-medium-r-normal--18-*-*-*-*-*-*-*"))))
(cond (on_windows_nt
       (add-to-list 'default-frame-alist
                    '(font . "Lucida Console-11:bold"))))

;; Font-lock (highlight syntax of) .ipe files as XML files.
(setq auto-mode-alist (cons '("\\.ipe\\'" . xml-mode) auto-mode-alist))

;; Open files from a directory listing with 'o'.
(cond (on_darwin
       (require 'dired)
       (define-key dired-mode-map "o" 'dired-open-mac)
       (defun dired-open-mac ()
         (interactive)
         (let ((file-name (dired-get-file-for-visit)))
           (if (file-exists-p file-name)
               (call-process "/usr/bin/open" nil 0 nil file-name))))))
;; Repeat for windows.

;; Define shortcut to go to Dired.
(define-key global-map "\C-x\C-j" 'dired-jump)

;; Adjust the path.
(cond (on_darwin
       (add-to-list 'exec-path "/Applications/Ipe.app/Contents/MacOS/ipe")))
;; Repeat for windows.

;; If you're building your own Ipe, point to the libraries.
(cond (on_darwin
       (setenv "DYLD_LIBRARY_PATH" "/usr/local/ipe/build/lib/")))

;; Open files with .ipe extension using Ipe.
(setq dired-guess-shell-alist-user
      (list
       (list "\\.ipe$"     "ipe ")))
