;;****************************************************************
;;****************************************************************
;;****************************************************************
;;****************           HTML ETC             ****************
;;****************************************************************
;;****************************************************************
;;****************************************************************

(defvar sg-on_windows_nt)
(defvar sg-on_darwin)
(defvar sg-on_gnu_linux)
(defvar sg-on_cygwin)

;;________________________________________________________________
;; XML indent
;;________________________________________________________________

;; From:
;; http://sinewalker.wordpress.com/2008/06/26/pretty-printing-xml-with-emacs-nxml-mode/

;; remember that Emacs (25.1.1) has json-pretty-print.

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Done XML beautifying!"))


;;--- ;;________________________________________________________________
;;--- ;; For JSON, on 24.3.92+
;;--- ;;                use json-pretty-print or json-pretty-print-buffer in json.el.
;;--- ;; For XML, use bf-pretty-print-xml-region, defined below.
;;--- ;;________________________________________________________________
;;---
;;--- ;; Only for 24.3.92+
;;--- (setq json-encoding-default-indentation "    ")
;;---
;;--- ;;________________________________________________________________
;;--- ;; Using web-mode for embedded css etc.
;;--- ;;________________________________________________________________
;;---
;;--- ;; Also tried: nXhtml, Mumamo, multi-mode, mmm-mode
;;---

(try-require 'web-mode)


(eval-after-load "web-mode"
  '(progn 
     (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.mjs\\'" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.svg\\'" . web-mode))
     
     ;; https://github.com/fxbois/web-mode/issues/447
     ;; doesn't work. investigate later.
     ;; (add-to-list 'web-mode-comment-formats '("javascript" . "//"))

     (setq web-mode-script-padding 4)
     (setq web-mode-style-padding 4)
     (setq web-mode-block-padding 4)

     (setq web-mode-markup-indent-offset 4)
     (setq web-mode-css-indent-offset 4)
     (setq web-mode-code-indent-offset 4)
     (setq web-mode-attr-indent-offset 4)
     (setq web-mode-attr-value-indent-offset 4)
     (setq web-mode-indentless-elements 4)
     (setq web-mode-markup-indent-offset 4)
     (setq web-mode-sql-indent-offset 4)

     ;; http://emacs.stackexchange.com/a/17015/
     (setq web-mode-engines-alist
           '(("django"    . "\\.html\\'")
             )
           )
     ))

;; Settings for syntax highlighting within web-mode

;;----------------------------------------------------------------
;; web-mode Syntax Highlighting of
;;----------------------------------------------------------------
;; ;; Change face color
(eval-after-load "web-mode"
  '(progn 
     (set-face-attribute 'web-mode-css-at-rule-face nil :foreground "yellow green")
     (set-face-attribute 'web-mode-folded-face nil :foreground "yellow3")
     (set-face-attribute 'web-mode-warning-face nil :foreground "green")
     (set-face-attribute 'web-mode-whitespace-face nil :foreground "red")
     (set-face-attribute 'web-mode-comment-keyword-face nil :foreground "forestgreen")

     ;; ;; html
     (set-face-attribute 'web-mode-doctype-face nil :foreground "LightGoldenrod")
     (set-face-attribute 'web-mode-html-tag-face nil :foreground "gold")
     (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "gold3")
     (set-face-attribute 'web-mode-html-attr-value-face nil :foreground "yellow green")
     ;;
     ;; ;; json
     ;; ;; web-mode-json-key-face, web-mode-json-context-face, web-mode-json-string-face
     ;;
     ;; ;; css
     (set-face-attribute 'web-mode-css-at-rule-face nil :foreground "yellow green")
     (set-face-attribute 'web-mode-css-property-name-face nil :foreground "yellow3")
     (set-face-attribute 'web-mode-css-function-face nil :foreground "green")
     (set-face-attribute 'web-mode-css-priority-face nil :foreground "gold")
     (set-face-attribute 'web-mode-css-pseudo-class-face nil :foreground "LightSalmon")
     (set-face-attribute 'web-mode-css-selector-face nil :foreground "gold3")
     (set-face-attribute 'web-mode-css-string-face nil :foreground "forestgreen")

     ;; code
     ;; web-mode-string-face
     (set-face-attribute 'web-mode-comment-face nil :foreground "forestgreen")
     ;; web-mode-preprocessor-face, web-mode-variable-name-face,
     ;; web-mode-function-name-face, web-mode-constant-face, web-mode-type-face, web-mode-keyword-face,
     ;; web-mode-symbol-face, web-mode-builtin-face

     ;; block
     ;; web-mode-block-control-face web-mode-block-face (see web-mode-enable-block-face), web-mode-block-string-face,
     (set-face-attribute 'web-mode-block-comment-face nil :foreground "forestgreen")

     ;; part
     ;; web-mode-part-face (see web-mode-enable-part-face), web-mode-part-string-face,
     (set-face-attribute 'web-mode-part-comment-face nil :foreground "forestgreen")
     ))
;; web-mode-javascript-string-face

;;---   ;; Shortcuts
;;---   ;; Change the shortcut for element navigation
;;---   ;; (define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)
;;---   ;; Snippets
;;---   ;; Add a snippet
;;---
;;---   ;; (setq web-mode-extra-snippets
;;---   ;;       '(("erb" . (("name" . ("beg" . "end"))))
;;---   ;;         ("php" . (("name" . ("beg" . "end"))
;;---   ;;                   ("name" . ("beg" . "end"))))
;;---   ;;        ))
;;---
;;---   ;; Auto-pairs
;;---   ;; Add auto-pair
;;---
;;---   ;; (setq web-mode-extra-auto-pairs
;;---   ;;       '(("erb"  . (("open" "close")))
;;---   ;;         ("php"  . (("open" "close")
;;---   ;;                    ("open" "close")))
;;---   ;;        ))
;;---   ;;
;; Enable / disable features
;; Auto-pairing
;; (setq web-mode-disable-auto-pairing t)
;; CSS colorization
;; (setq web-mode-disable-css-colorization t)
;; Block face: can be used to set blocks background (see web-mode-block-face)
;; (setq web-mode-enable-block-face t)
;; Part face: can be used to set parts background (see web-mode-part-face)
;; (setq web-mode-enable-part-face t)
;; Comment keywords (see web-mode-comment-keyword-face)
;; (setq web-mode-enable-comment-keywords t)
;; Highlight current HTML element (see web-mode-current-element-highlight-face)
;; (setq web-mode-enable-current-element-highlight t)
;; Heredoc (cf. PHP strings) fontification (when the identifier is <<<EOTHTML or <<<EOTJAVASCRIPT)
;; (setq web-mode-enable-heredoc-fontification t)
;; The customisations below should not be put in the hook. Declare them before loading web-mode.el
;; Keywords
;; Add PHP constants
;; (setq web-mode-extra-php-constants '("constant1" "constant2"))
;; Also available: web-mode-extra-php-keywords, web-mode-extra-javascript-keywords,
;; web-mode-extra-jsp-keywords, web-mode-extra-asp-keywords

;;--- )
;;---
;;--- ;;________________________________________________________________
;;--- ;;    css-mode
;;--- ;;________________________________________________________________
;;---
;;--- (setq auto-mode-alist (cons '("\\.css\\'" . css-mode) auto-mode-alist))


;;________________________________
;;    web
;;________________________________

;; Treat .rhtml files as HTML
(setq auto-mode-alist (cons '("\\.rhtml\\'" . html-mode) auto-mode-alist))

;;________________________________
;;    javascript-mode
;;________________________________

;; web-mode is superior to javascript-mode.

;;________________________________________________________________
;;    jinja2
;;________________________________________________________________

;; web-mode is superior to jinja2-mode.

;;________________________________________________________________
;;    fontify html 
;;________________________________________________________________

;; even though emacs > 21.xx.xx has an html mode, htmlfontify is
;; useful because it makes it possible to 
;;    M-x htmlfontify-copy-and-link-dir

(cond (sg-on_darwin
       (when (< emacs-major-version 23)
         (setq load-path (cons "~/ghali/dotfiles/emacs/elisp/htmlfontify" load-path))
         (when (try-require 'htmlfontify)
           ()
           )
)))

