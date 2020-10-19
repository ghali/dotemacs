;;****************************************************************
;;****************************************************************
;;****************************************************************
;;****************    PROGRAMMING INDENTATION     ****************
;;****************************************************************
;;****************************************************************
;;****************************************************************

(defvar on_windows_nt)
(defvar on_darwin)
(defvar on_gnu_linux)
(defvar on_cygwin)

;;________________________________
;;    Python & Ruby
;;________________________________

(setq python-indent-offset 4)
(setq ruby-indent-level 4)
(setq sql-indent-level 4)

;;________________________________
;;    HTML etc
;;________________________________

(add-hook 'html-mode-hook
          (lambda()
            (setq sgml-basic-offset 4)
            (setq indent-tabs-mode nil)))

(setq css-indent-offset 4)
(setq sgml-basic-offset 4)
(setq nxml-child-indent 4)
(setq nxml-attribute-indent 4)

;;________________________________________________________________
;;    C/C++
;;________________________________________________________________

;; C-c . <RET> STYLE <RET>    ("C-c ." is c-set-style)
;; Possible styles:
;; awk bsd cc-mode ellemtel gnu java k&r linux python stroustrup user whitesmith

(require 'cc-mode)

(setq-default indent-tabs-mode nil          ; Insert spaces, not tabs, for all modes,
              c-basic-offset 4              ; Amount of basic offset used by + and - symbols in `c-offsets-alist'
              tab-width 4
              c-comment-only-line-offset 0) ; Extra offset for line which contains only the start of a comment

;; When needed, run manually:
;; (setq-default indent-tabs-mode nil)
;; (setq-default indent-tabs-mode t)

(c-set-offset 'substatement-open 0)     ; The brace that opens a substatement block
(c-set-offset 'brace-list-open 0)       ; Open brace of an enum or static array list

;;(c-set-offset 'namespace-open 0)
;;(c-set-offset 'namespace-close 0)
;;(c-set-offset 'innamespace 0)
(c-set-offset 'namespace-open 4)
(c-set-offset 'namespace-close 4)
(c-set-offset 'innamespace 4)

(add-to-list 'c-offsets-alist '(annotation-top-cont .0))

(add-hook 'java-mode-hook
          (lambda ()
            (c-set-offset 'annotation-top-cont 0)
            )
          )

(defconst my-cc-style
  '("gnu"
    (c-offsets-alist . ((innamespace . [4])))))

(c-add-style "my-cc-style" my-cc-style)


;;________________________________
;; text-mode
;; Duplicate tabs on subsequent lines in text-mode
;; (C-j does this by default; here we bind RET to C-j).
;;________________________________

(add-hook 'text-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            ))

(add-hook 'text-mode-hook
          (lambda ()
            (setq tab-width 8)
            ))

(add-hook 'text-mode-hook               ; Insert TABS in text-mode.
          '(lambda ()
             (local-set-key (kbd "RET") 'newline-and-indent)
             )
          )

;; May also need:
;; (c-set-offset 'inline-open 0)
;; (c-set-offset 'case-label +))

;;________________________________
;; makefile-mode
;;________________________________

(add-hook 'makefile-mode-hook
          (lambda ()
            (setq tab-width 8)
            )
          )

;;________________________________________________________________
;; indent-marked-files
;;________________________________________________________________
;; http://stackoverflow.com/a/2555475/

(defun indent-marked-files ()
  (interactive)
  (dolist (file (dired-get-marked-files))
    (find-file file)
    (indent-region (point-min) (point-max))
    (save-buffer)
    (kill-buffer nil)))

;;________________________________________________________________
;; YAML
;;________________________________________________________________
(setq yaml-indent-offset 4)

;;________________________________________________________________
;; Experimental
;;________________________________________________________________
;;
;;
;;________________________________________________________________
;; Use foreign mode.
;;________________________________________________________________
;; dtrt-indent tries to guess the indentation style. It works only for simple code.
;; (when
;;     (try-require 'dtrt-indent)
;;   (dtrt-indent-mode 1)
;;   )

;; Starting from Emacs 24 an alternative to dtrt is M-x c-guess.
;; Use dtrt-indent manually when necessary.
;;________________________________________________________________
