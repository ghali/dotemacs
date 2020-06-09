;;****************************************************************
;;****************************************************************
;;****************************************************************
;;****************       FILES AND DIRED          ****************
;;****************************************************************
;;****************************************************************
;;****************************************************************

(defvar sg-on_windows_nt)
(defvar sg-on_darwin)
(defvar sg-on_gnu_linux)
(defvar sg-on_cygwin)

;;________________________________________________________________
;;    Some dired settings
;;________________________________________________________________

(cond (sg-on_windows_nt
       (when (try-require 'nw-win)
         ()
         )
       ))

(when (try-require 'facemenu)
  ()
)

;;________________________________________________________________
;;    Compress/decompress directories on the fly
;;________________________________________________________________

;; not in melpa
(when (try-require 'dired-tar)
  ()
)

;;________________________________________________________________
;;    Keys
;;________________________________________________________________

(cond (sg-on_darwin
        (require 'dired)

        (define-key dired-mode-map "o" 'dired-open-mac)

        (defun dired-open-mac ()
          (interactive)
          (let ((file-name (dired-get-file-for-visit)))
            (if (file-exists-p file-name)
                (call-process "/usr/bin/open" nil 0 nil file-name))))
))

;;________________________________________________________________
;;    dired-x
;;________________________________________________________________

;; dired-x is a nice substitute for Windows Explorer and OSX's Finder.
;; M-o: avoid seeing all the backup files.
;; C-x C-j: enter dired/dired-x mode.

(add-hook 'dired-mode-hook
          (lambda()
            (require 'dired-x)
            (define-key dired-mode-map (kbd "M-o") 'dired-omit-mode)))


;; Q: How does one do dired-open on Windows?

;; Emacs 22 already had dired-toggle-read-only, but it was called
;; wdired-change-to-wdired-mode
;; We override C-x C-q. Its previous binding in emacs 22 is
;; toggle-read-only.
;; This makes it possible to rename files by editing the dired buffer.
(when (= emacs-major-version 22)
  (require 'dired)
  (define-key dired-mode-map "\C-x\C-q" 'wdired-change-to-wdired-mode)
)

;;________________________________________________________________
;;    Mark user-written files (for subsequent searching)
;;________________________________________________________________

(defun mark-interesting-files () "Mark program (etc) files" (interactive)
  (dired-mark-files-regexp (concat "\\.h$\\|"
                                   "\\.c$\\|"
                                   "\\.hpp$\\|"
                                   "\\.cpp$\\|"
                                   "\\.php$\\|"
                                   "\\.ipp$\\|"
                                   "\\.py$\\|"
                                   "\\.fsh$\\|"
                                   "\\.vsh$\\|"
                                   "\\.awk$\\|"
                                   "\\.sed$\\|"
                                   "\\.bash$\\|"
                                   "\\.C$\\|"
                                   "\\.cc$\\|"
                                   "\\.ipp$\\|"
                                   "\\.java$\\|"
                                   "\\.sql$\\|"
                                   "\\.html$\\|"
                                   "\\.css$\\|"
                                   "\\.js$\\|"
                                   "\\.dart$\\|"
                                   "\\.txt$\\|"
                                   "\\.el$\\|"
                                   "\\.xml$\\|"
                                   "\\.djhtml$\\|"
                                   "\\.pro$\\|"
                                   "\\.tex$")))

(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map ";" 'mark-interesting-files)
             ))


;; todo:
;; In dired-mode the binding
;;    Q               dired-do-query-replace-regexp
;; is more convenient than
;;    A               dired-do-search
;; Consider changing the default binding of 'A' to be dired-do-isearch-regexp,
;; perhaps duplicating the default bindings:
;;    M-s a C-s       dired-do-isearch
;;    M-s a C-M-s     dired-do-isearch-regexp

;;________________________________________________________________
;; todo
;;________________________________________________________________

;; Accidentally running '!' in dired-mode under windows-nt on a
;; directory crashes emacs.

;;________________________________
;; It's been a while since it was necessary to define C-x C-j oneself. Why?
(define-key global-map "\C-x\C-j" 'dired-jump)


;; Because Explorer and Finder have a mapping from file type to application,
;; we need to tell emacs what to do with each file type.
;; ! (dired-do-shell-command) uses this map.

(setq dired-guess-shell-alist-user
      (list
       (list "\\.gp$"      "gnuplot ")
       (list "\\.gnuplot$" "gnuplot ")
;;       (list "\\.ly$"      "/Applications/LilyPond.app/Contents/MacOS/LilyPond")
;;       doesn't work - don't bother fixing - just use open-file.
       (list "\\.ipe$"     "ipe ")
       (list "\\.xml$"     "ipe ")
       (list "\\.prn$"     "gv ")
       (list "\\.ps$"      "gv ")
       (list "\\.ps.gz$"   "gv ")
       (list "\\.eps$"     "gv ")
       (list "\\.eps.gz$"  "gv ")
       (list "\\.pdf$"     "acroread")
       (list "\\.PDF$"     "acroread")
       (list "\\.iv$"      "SceneViewer")
       (list "\\.\\(rgb\|tiff\|tif\|xbm\|gif\|pgm\|ppm\|bmp\|tga\\)$"  "display")
       (list "\\.ppm$" "display")
       (list "\\.gif$" "display")
       (list "\\.png$" "display")
       (list "\\.jpg$" "display")
       (list "\\.JPG$" "display")
       (list "\\.avi$" "movieplayer")
       (list "\\.sc$" "showcase")
       (list "\\.wav$" "realplay")
       (list "\\.drawtool$" "drawtool")
       (list "\\.py$" "/usr/local/bin/python3")
       (list "\\.js$" "/usr/local/bin/node")
       (list "\\.swift$" "/usr/bin/swift")
       ))

;; macOS also has: /System/Library/Frameworks/JavaScriptCore.framework/Versions/Current/Resources/jsc

;;________________________________
;; Suggest second dired directory if one is displayed.
;;________________________________
(setq dired-dwim-target t)

;;________________________________
;; We need the following two lines because ls on Darwin does not support --dired.
;; Emacs runs these two lines by default on Windows.
;; See the variable dired-use-ls-dired for details.
;;________________________________
;;    ****Messes up with search in dired marked files.****
;;________________________________
;; (cond (sg-on_darwin
;;        (setq ls-lisp-use-insert-directory-program nil)
;;        (try-require 'ls-lisp)
;; ))

;;________________________________________________________________
;;    Dired-mode settings
;;________________________________________________________________

;; A few customizations:
;; Among them: make copy and delete in dired recursive.

;;________________________________________________________________
;;    Local Variable Values
;;________________________________________________________________

;; May need:
;; (TeX-brace-indent-level . 4) (TeX-auto-untabify)
;; (TeX-auto-regexp-list . LaTeX-auto-regexp-list))))

;;________________________________________________________________
;;    Dired omit
;;        dired-omit-files is only effective if emacs is restarted.
;;________________________________________________________________

(add-hook 'dired-mode-hook
          (lambda ()
            (setq dired-omit-files
                  (concat "^\\.$\\|"
                          "^\\.\\.$\\|"
                          "^\\.git$\\|"
                          "^\\.gitignore$\\|"
                          "^__pycache__$\\|"
                          "^\\.pytest_cache$\\|"
                          "^moc_\\|"
                          "^qrc_\\|"
                          "^\\.qmake\\.stash$\\|"
                          "^\\.DS_Store$\\|"
                          "\\.idea$\\|"
                          ;; "^requirements\\.txt\\|"
                          "\\.out$\\|"
                          "\\.nav$\\|"
                          "\\.snm$\\|"
                          "\\.vrb$\\|"
                          "\\.d$\\|"
                          "\\.ncb$\\|"
                          "\\.sln$\\|"
                          "\\.suo$\\|"
                          "^vc90\\|"
                          "\\.pdb$\\|"
                          "^vc90\\.idb$\\|"
                          "^Jamfile$\\|"
                          "^Jamfile\\.v2$\\|"
;;                           "^Makefile$\\|"
                          "^Makefile\\.Debug$\\|"
                          "^Makefile\\.Release$\\|"
                          "\\.sqlite$\\|"
                          "\\.vcproj$\\|"
                          "\\.vcproj\\.NA\\.ghali\\.user$"))
            ))

;; We don't hide \\|^debug$\\|^release$ to keep it possible to run (! or &) executables from emacs.
;; 
;; Do not set
;;     dired-omit-extensions
;; directly. See below.

;;________________________________________________________________
;;    Completion
;;________________________________________________________________

'(completion-ignored-extensions
  (quote ("__pycache__"
          "CVS/" ".o" "~" ".bin" ".lbin" ".fasl" ".ufsl" ".a" ".ln" ".blg" ".bbl"
          ".log" ".out"
          ".elc" ".lof" ".glo" ".idx" ".lot" ".dvi" ".fmt" ".tfm" ".class" ".fas"
          ".lib" ".x86f" ".sparcf" ".lo" ".la" ".toc" ".log" ".aux" ".cp" ".fn"
          ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".lbl"
          ".out" ".brf" ".ncb" ".sln" ".suo" ".vcproj.AD.ghali.user" ".idb" ".pdb"
          ".synctex.gz" ".svn")))

;; dired-omit-extensions defaults to elements of
;;     `completion-ignored-extensions'
;;     `dired-latex-unclean-extensions'
;;     `dired-bibtex-unclean-extensions'
;;     `dired-texinfo-unclean-extensions'

;;________________________________________________________________
;;    Testing whether auto-updating of dired buffers is convenient
;;________________________________________________________________

;; (setq dired-auto-revert-buffer t)
;; This is a bad idea. It makes deeply recursive directories unusable.

;;________________________________________________________________
;;    Scroll lock still needs improvement.
;;________________________________________________________________

;; (setq scroll-all-mode 't)

;;________________________________________________________________
;;    uniquify -- though using <1>, <2> also has its advantages.
;;________________________________________________________________

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;;________________________________________________________________
;;    pulse delay for dired search
;;________________________________________________________________
(setq pulse-delay 0.8)

;;________________________________________________________________
;;
;;________________________________________________________________
;; Doing:
;;     (setq split-width-threshold nil)
;; is not enough. We need to: (Ref: https://emacs.stackexchange.com/a/21653/)

;; (setq split-height-threshold 0)
;; (setq split-width-threshold 0)
;; 
;; 
;; (defun my-sensible-window-split (&optional window)
;;   (cond
;;     ((and (> (window-width window)
;;              (window-height window))
;;           (window-splittable-p window 'horizontal))
;;       (with-selected-window window
;;         (split-window-right)))
;;     ((window-splittable-p window)
;;       (with-selected-window window
;;         (split-window-below)))))
;; 
;; (setq split-window-preferred-function #'my-sensible-window-split)
