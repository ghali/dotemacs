;;****************************************************************
;;****************************************************************
;;****************************************************************
;;****************             MISC               ****************
;;****************************************************************
;;****************************************************************
;;****************************************************************

(defvar on_windows_nt)
(defvar on_darwin)
(defvar on_gnu_linux)
(defvar on_cygwin)

;;________________________________________________________________
;;    Calendar/ Holidays
;;________________________________________________________________

;; We need to test whether we are running emacs >= 23 because
;; holiday-other-holidays used to be called other-holidays.

(when (>= emacs-major-version 23)
  (setq holiday-local-holidays
    '(
      (holiday-float 10 1 2 "Canadian Thanksgiving") ; second Monday in October
      ))
  (setq holiday-other-holidays nil)
)

;;________________________________________________________________
;; Prevent down-arrow from adding empty lines to the bottom of the buffer
;; (the default behaviour).
(setq next-line-add-newlines nil)

;; To make it possible for M-q (fill-paragraph) to end a line with a period,
;; one needs to tell emacs to abandon its convention of distinguishing between
;; single and double space.
(setq sentence-end-double-space nil)

;;________________________________________________________________
;;    More Misc
;;________________________________________________________________

;; Highlight the marked region.
(setq-default transient-mark-mode t)

;; 'woman' mode is an improvement on 'man' mode for manual pages
;; (setq-default woman-use-own-frame nil)
;; Man-notify-method controls the behaviour of [wo]man mode.
(setq-default Man-notify-method 'pushy)

;; Always display line and column numbers.
(setq-default line-number-mode 't)
(setq-default column-number-mode 't)

;;________________________________________________________________
;;    Disable all help, particularly tooltips
;;________________________________________________________________

(setq show-help-function nil)

;;________________________________________________________________
;;    I hardly ever use the menu bar or the tool bar
;;________________________________________________________________
;; Asking for no menu bar on OS X/Darwin is harmless, but also futile.
(cond (on_windows_nt
       (menu-bar-mode -1)
))
(when window-system
  (tool-bar-mode -1)
)
;; speedbar, on the other hand, is useful.

;;________________________________________________________________
;;    Settings related to split-window-horizontally (C-x 3)
;;________________________________________________________________

;;________________________________
;;    Avoid vertical splits
;;________________________________

;; If a window is wider than split-width-threshold, Emacs will split a
;; window horizontally (C-x 3) when one compiles. Since my preferred
;; default is to truncate-lines, it means that I have to scroll
;; horizontally to read the error messages. Change the variable to
;; something half of which makes it possible to read compilation
;; messages.

;; (setq split-width-threshold 240)


;;________________________________
;;    Track more of one buffer
;;________________________________

;; just a reminder, nothing to set..
;; M-x follow-mode: sync-lock the same buffer within C-x 3.



;;________________________________________________________________
;;    Auto-insert newlines to maintain limited line width
;;________________________________________________________________

(add-hook 'text-mode-hook '(lambda () (auto-fill-mode 1)))
(add-hook 'mail-mode-hook '(lambda () (auto-fill-mode 1)))
(add-hook 'latex-mode-hook '(lambda () (auto-fill-mode 1)))

;;________________________________________________________________
;;    Put all .save's in one place
;;________________________________________________________________

(cond (on_darwin
       (setq auto-save-list-file-prefix "~/ghaliweekly/misc/.save/.saves-" )
))

;;________________________________________________________________
;;    Latex
;;________________________________________________________________

;; Notice that MacTeX installs to /usr/local/texlive.
;; MacPorts ('port install texlive') installs in /opt/local.
;; Avoid having a duplicate tex installation.

;; (cond (on_darwin
;;        (setenv "TEXINPUTS"
;;                ".:/usr/local/texlive/2020/texmf-dist/tex//:"
;;                "$HOME/ghaliweekly/latex/styles//")
;; ))

;; If I return to installing LaTeX via MacPorts, reset to:
;;     (setenv "TEXINPUTS" ".:/opt/local/share/texmf-dist/tex//")

;;________________________________________________________________
;;    pgp/gnupg / gpg en/decrypt
;;________________________________________________________________

;; file extensions: .cpt  don't use
;;                  .gpg  handled by `port install gnupg`
(when (try-require 'ps-ccrypt)
  ()
)

;; Alternative: epa-dired-do-en/decrypt

;;________________________________________________________________
;;    Change window title
;;________________________________________________________________

(setq frame-title-format
      (list ""
        "%b"
        ))

;; (setq frame-title-format
;;       (list ""
;;         "%b"
;;         "  |  "
;;         `default-directory
;;         "  |  "
;;         (system-name)
;;             " ("
;;             system-type-as-string
;;             "; Emacs "
;;             emacs-version
;;             ")"
;;       ))

;;________________________________________________________________
;;    Do not publish my email on Usenet
;;________________________________________________________________

(setq user-full-name       "Sherif Ghali")
;; (setq user-login-name      "myLastname")
;; (setq user-real-login-name "myLastname")
;; (setq user-mail-address    "myName@somewhere")
;; (setq system-name          "machine.somewhere")

;;________________________________________________________________
;;    Don't display initial logo
;;________________________________________________________________

(setq inhibit-startup-message t)

;;________________
;; Initial loading
;;________________

;; (cond (on_darwin
;;        (find-file "/Users/me/ghali/gcb/tex/book2/chaps")
;; ))

;;________________________________________________________________
;;    scroll-left is disabled by default; enable it.
;;________________________________________________________________
(put 'scroll-left 'disabled nil)

(put 'set-goal-column 'disabled nil)

;;________________________________________________________________
;;    Display long lines by truncating them.
;;________________________________________________________________

(cond (on_windows_nt   (set-default 'truncate-lines   t) ))
(cond (on_darwin       (set-default 'truncate-lines nil) ))

;;________________________________________________________________
;;    But do not truncate in compilation mode.
;;________________________________________________________________

(defun my-compilation-mode-hook () 
  (setq truncate-lines nil) 
  (setq truncate-partial-width-windows nil))
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

;;________________________________________________________________
;;    Show matching parenthesis
;;        show-paren-mode is both a function and a variable, but
;;        "Setting this variable directly does not take effect;
;;        either customize it (see the info node `Easy Customization')...or.."
;;________________________________________________________________

(show-paren-mode t)

;;________________________________________________________________
;;    Enter accented characters
;;________________________________________________________________

;; C-x <RET> C-\ -- set-input-method (press space to see;
;;                                    french-prefix is convenient on a qwerty keyboard)
;; C-\           -- toggle-input-method
;; C-\           -- french-prefix

;;________________________________________________________________
;;    Long lines
;;________________________________________________________________

;; longlines-mode is sometimes useful for text files.
;;     longlines-auto-wrap

;;________________________________________________________________
;;    The server waits for emacsclient (launched from git, ipe, ...)
;;________________________________________________________________

;; todo: wrap
;; (featurep 'server)
; `M-x server-force-delete'

;; (when window-system
;;   (server-start)
;; )

;; (when (and
;;        window-system
;;        (not (boundp 'server-process))
;;        )
;;   (server-start)
;; )


;; (and (boundp 'server-process)
;;      (memq (process-status server-process) '(connect listen open run)))

;; (when window-system
;;   (when (or (not (boundp 'server-process))
;;             (not (eq (process-status server-process)
;;                      'listen)))
;;     (server-start))
;; )

;; Press  C-x #  when done.

;;________________________________________________________________
;;    unfill paragraphs and regions
;;________________________________________________________________
;; Source:
;;     http://xahlee.org/emacs/emacs_unfill-paragraph.html

(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the reverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column 9000000))
    (fill-paragraph nil)))

(defun unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the reverse of `fill-region'."
  (interactive "r")
  (let ((fill-column 9000000))
    (fill-region start end)))

;; Default:

(setq fill-column 110)

;;________________________________________________________________
;; Pov-Ray
;;________________________________________________________________

(add-to-list 'load-path "~/ghali/dotfiles/emacs/elisp/pov-mode-pov-mode/")
(when (try-require 'pov-mode)
  (autoload 'pov-mode "pov-mode" "PoVray scene file mode" t)
  (add-to-list 'auto-mode-alist '("\\.pov\\'" . pov-mode))
  (add-to-list 'auto-mode-alist '("\\.inc\\'" . pov-mode))
)

;;________________________________________________________________
;; Visual Line Mode
;;________________________________________________________________

(setq-default word-wrap t)

;; With word-wrap paragraphs continue to have a single logical newline, coinciding with the actual newline.
;; With global-visual-line-mode emacs inserts additional *logical* newlines.
;;
;; Another option is toggle-truncate-lines, which hides lines that extend beyond the window boundary.

;; ;;________________________________________________________________
;; ;; rtf-mode
;; ;;________________________________________________________________
;; ;; From:
;; ;;         http://interglacial.com/rtf/emacs/
;; 
;; (autoload 'rtf-mode "rtf-mode" "RTF mode" t)
;; (add-to-list 'auto-mode-alist
;;   '("\\.rtf$" . rtf-mode))

;;________________________________________________________________
;; Emacs objects to writing UTF-8 in capitals.
;;________________________________________________________________
;; I could use: (http://stackoverflow.com/a/14033335/
;; (define-coding-system-alias 'UTF-8 'utf-8)

;;________________________________________________________________
;;________________________________________________________________
;;________________________________________________________________
;;________________________________________________________________
;;________________________________________________________________
;;________________________________________________________________
;;________________________________________________________________
;;________________________________________________________________

;; not in melpa
(try-require 'color-moccur)

;;________________________________________________________________
;; https://www.emacswiki.org/emacs/SqlBeautify
;;________________________________________________________________

(defun sql-beautify-region (beg end)
  "Beautify SQL in region between beg and END."
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end
                             "/usr/local/bin/anbt-sql-formatter" ;; "sqlbeautify"
                             nil t)))
    ;; change sqlbeautify to anbt-sql-formatter if you
    ;;ended up using the ruby gem

(defun sql-beautify-buffer ()
 "Beautify SQL in buffer."
 (interactive)
 (sql-beautify-region (point-min) (point-max)))

(defun sql-beautify-region-or-buffer ()
  "Beautify SQL for the entire buffer or the marked region between beg and end"
  (interactive)
  (if (use-region-p)
      (sql-beautify-region (region-beginning) (region-end))
    (sql-beautify-buffer)))

;;________________________________________________________________
;; recognize tables
;;________________________________________________________________
;; Too slow. Use on demand.
;; (require 'table)
;; (add-hook 'text-mode-hook 'table-recognize)

;;________________________________________________________________
;; po-mode
;;________________________________________________________________
;; Ref: https://emacs.stackexchange.com/a/17147/
;; (add-to-list 'load-path "/usr/local/Cellar/gettext/0.19.8.1/share/emacs/site-lisp/")
;; (autoload "po-mode" "po-mode")

(when (try-require 'po-mode)
  (add-to-list 'auto-mode-alist '("\\.po$" . po-mode))
)

;;________________________________________________________________
;; ESS
;;________________________________________________________________

(add-to-list 'load-path "/Users/me/ghali/dotfiles/emacs/elisp/ess-17.11/lisp")
(require 'ess-site)
;; (require "ess-site")

;;________________________________________________________________
;; Dockerfile-mode
;;________________________________________________________________
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;;________________________________________________________________
;; smooth scrolling
;;________________________________________________________________
;; Ref: https://www.reddit.com/r/emacs/comments/41vicb/emacs_os_x_smooth_scroll/
;; https://github.com/k-talo/smooth-scroll.el
;; (require 'smooth-scroll)
;; (smooth-scroll-mode t)

;; OR
;; https://github.com/aspiers/smooth-scrolling
;; (require 'smooth-scrolling)
;; (smooth-scrolling-mode 1)

(pixel-scroll-mode t)

;;________________________________________________________________
;; Horizontally scroll just the current line.
;; Not using this, but it may be helpful for some CSV files.
;;________________________________________________________________
;; (setq truncate-lines t)
;; (setq-local auto-hscroll-mode 'current-line)
;; Ref: https://emacs.stackexchange.com/questions/40864/scroll-only-current-line-when-truncating-lines

;;________________________________________________________________
;; Cycle between snake case, camel case, etc.
;;________________________________________________________________
;; Ref: https://stackoverflow.com/a/27422814/
;; https://raw.githubusercontent.com/akicho8/string-inflection/master/string-inflection.el
;; camelify/uncamelify
(require 'string-inflection)
(global-set-key (kbd "C-c i") 'string-inflection-cycle)
(global-set-key (kbd "C-c C") 'string-inflection-camelcase)        ;; Force to CamelCase
(global-set-key (kbd "C-c L") 'string-inflection-lower-camelcase)  ;; Force to lowerCamelCase
(global-set-key (kbd "C-c J") 'string-inflection-java-style-cycle) ;; Cycle through Java styles

;;________________________________________________________________
;;    ess is a text-based web browser.
;;________________________________________________________________
(setq browse-url-browser-function 'eww-browse-url)

;;________________________________________________________________
;; window-number
;; Ref: https://stackoverflow.com/a/11184101
;; https://github.com/nikolas/window-number/blob/master/window-number.el
;;________________________________________________________________
;; (when (try-require 'window-number)
;;   (window-number-mode 1)
;; )

;; window-number is poorly designed.
;; The following settings don't work here:
;;     (window-number-define-keys window-number-mode-map "M-n ")
;;     (setq window-number-active-foreground "black")
;;     (setq window-number-active-background "SteelBlue2")
;;     (setq window-number-inactive-foreground "black")
;;     (setq window-number-inactive-background "SteelBlue2")
;; I had to edit elisp/window-number.el directly.

;;________________________________________________________________
;; competition to window-number: winum
;;________________________________________________________________

(when (try-require 'winum)
  (winum-set-keymap-prefix (kbd "M-n"))
)

;;________________________________________________________________
;; Accelerate buffer switching
;;________________________________________________________________

;; (ido-mode 1)
;; (setq ido-separator "\n")
