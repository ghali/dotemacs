;;****************************************************************
;;****************************************************************
;;****************************************************************
;;****************       PROGRAMMING MODES        ****************
;;****************************************************************
;;****************************************************************
;;****************************************************************

(defvar on_windows_nt)
(defvar on_darwin)
(defvar on_gnu_linux)
(defvar on_cygwin)

;;________________________________________________________________
;;    Settings for compilation scrolling
;;________________________________________________________________

;; scroll the *compilation* buffer window as output appears.
(defvar compilation-scroll-output)
(setq compilation-scroll-output t)
;; (setq compilation-window-height 20)
(defvar compile-auto-highlight)
(setq compile-auto-highlight t)

;;________________________________________________________________
;;    Set the default compilation command
;;________________________________________________________________

;; default on unixes is 'make -t'

(cond (on_windows_nt    (setq compile-command "nmake") ))
;; (cond (on_darwin        (setq compile-command "qmake; make ") ))
(cond (on_darwin        (setq compile-command "make ") ))
(cond (on_gnu_linux     (setq compile-command "make ") ))

;;________________________________________________________________
;;    Compile without pressing enter to confirm command
;;________________________________________________________________

;; (setq compilation-read-command nil)
;; Bad idea. Precludes options (qmake, make -k, etc).

;;________________________________________________________________
;;    Kill previously running compilation, if any
;;________________________________________________________________

;; Particularly helpful for pdflatex
(defvar compilation-always-kill)
(setq compilation-always-kill t)

;;________________________________________________________________
;;    Settings for modes
;;________________________________________________________________

;;________________________________
;;    c++-mode
;;________________________________

;; Even if the file extension is just .c or .h, assume it is a C++ file:
(setq auto-mode-alist (cons '("\\.c\\'" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.h\\'" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.fx\\'" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ipp\\'" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cu\\'" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cuh\\'" . c++-mode) auto-mode-alist))

;;________________________________
;;    c-mode
;;________________________________

;; Treat vertex and fragment shaders as C programs
(setq auto-mode-alist (cons '("\\.fsh\\'" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.vsh\\'" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.vert\\'" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.frag\\'" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.glsl\\'" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.vert.txt\\'" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.frag.txt\\'" . c-mode) auto-mode-alist))

;; Accept more filenames as Makefiles.
(setq auto-mode-alist (cons '(".*Makefile.*" . makefile-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mak" . makefile-mode) auto-mode-alist))


;;________________________________
;;    Trailing whitespaces
;;________________________________
;; If we just run:
;; (setq-default show-trailing-whitespace 't)
;; we'd have trailing whitespace highlighted in calendar mode.
;; So instead we do:
;;     (Ref: https://lists.gnu.org/archive/html/emacs-devel/2013-05/msg00254.html)
;; (add-hook 'find-file-hook (lambda ()
;;          (setq-local show-trailing-whitespace t)))
;; Rather leave out. That's still too distracting.

;;________________________________
;;    tex-mode
;;________________________________

(setq auto-mode-alist (cons '("\\.aux\\'" . tex-mode) auto-mode-alist))

;;________________________________
;;    objc-mode
;;________________________________

(setq auto-mode-alist (cons '("\\.m\\'" . objc-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mm\\'" . objc-mode) auto-mode-alist))

;;________________________________
;;    ruby-mode
;;________________________________

;; Treat .rjs files as Ruby
(setq auto-mode-alist (cons '("\\.rjs\\'" . ruby-mode) auto-mode-alist))
;; Rakefiles are Ruby files:
(setq auto-mode-alist (cons '("\\Rakefile\\'" . ruby-mode) auto-mode-alist))
;; So is Gemfile:
(setq auto-mode-alist (cons '("\\Gemfile\\'" . ruby-mode) auto-mode-alist))

;; Treat .vssettings files (Visual Studio) as XML
(setq auto-mode-alist (cons '("\\.vssettings\\'" . xml-mode) auto-mode-alist))

;;________________________________
;;    Fixlog & ssql
;;________________________________
;; Treat .fixlog files (FIX) as XML -- large files; use on demand
;; (setq auto-mode-alist (cons '("\\.fixlog\\'" . xml-mode) auto-mode-alist))
;; or else: don't font-lock fixlog files
;; (setq auto-mode-alist (cons '("\\.fixlog\\'" . fundamental-mode) auto-mode-alist))

;; Treat .ssql files (Streambase) as SQL
(setq auto-mode-alist (cons '("\\.ssql\\'" . sql-mode) auto-mode-alist))

;;________________________________
;;    lua-mode
;;________________________________

;; http://lua-mode.luaforge.net/

(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;________________________________
;;    Boost jam-mode
;;________________________________

(cond (on_windows_nt
       (require 'jam-mode)
       (add-to-list 'auto-mode-alist '("\\.jam$" . jam-mode))
       (add-to-list 'auto-mode-alist '("^Jamfile$\\|^Jamfile\\.v2$" . jam-mode))
))

;;________________________________
;;    DOS batch files
;;________________________________
;; Do not predicate dos-mode on being on Windows.
;; Enable editing batch files elsewhere.
;; (cond (on_windows_nt
       (autoload 'dos-mode "dos" "Edit Dos scripts." t)
       (add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))
;; ))

;;________________________________
;;    gnuplot
;;________________________________

(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t)

(setq auto-mode-alist (append '(("\\.gp$"      . gnuplot-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.gnuplot$" . gnuplot-mode)) auto-mode-alist))

;;________________________________
;;    qt-pro-mode and qml-mode
;;________________________________

(when (try-require 'qt-pro-mode)
  (add-to-list 'auto-mode-alist '("\\.pr[io]$" . qt-pro-mode))
)

(when (try-require 'qml-mode)
  (add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))
)

;;________________________________
;;    lilypond-mode
;;________________________________

;; Extracted from:
;; /Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp/lilypond-init.el
(cond (on_darwin
       (setq load-path (append (list (expand-file-name "/Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp")) load-path))

       (autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
       (add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
       (add-to-list 'auto-mode-alist '("\\.ily$" . LilyPond-mode))
       (add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

       ))

;;________________________________
;;    Qt
;;________________________________

;; Treat .moc files (Qt) as C++
;; (setq auto-mode-alist (cons '("\\.moc\\'" . c++-mode) auto-mode-alist))
;; Nokia/Trolltech renamed moc files to moc_*.cpp. Pattern added to dired-omit-files.

;; Treat .ui files (Qt) as XML
(setq auto-mode-alist (cons '("\\.ui\\'" . xml-mode) auto-mode-alist))

;;________________________________
;;    java-mode
;;________________________________

(setq auto-mode-alist (cons '("\\.aidl\\'" . java-mode) auto-mode-alist))

;;________________________________
;;    csharp-mode
;;________________________________

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
   (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;;(defconst csharp-font-lock-keywords-2 (c-lang-const c-matchers-2 csharp)
;;"Fast normal highlighting for C# mode.")

;; ________________________________
;;    xml-mode
;; ________________________________

(setq auto-mode-alist (cons '("\\.ipe\\'" . xml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.qrc\\'" . xml-mode) auto-mode-alist))

;; ________________________________
;;    groovy-mode
;; ________________________________

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
          '(lambda ()
             (require 'groovy-electric)
             (groovy-electric-mode)))


;;________________________________________________________________
;;    hideshow;
;;    On Windows: S-Mouse-2 hides/unhides initial comment block.
;;    On Mac:     M-x hs-toggle-hiding
;;                `C-c @ C-c'   Either hide or show the current block.
;;________________________________________________________________

;; (load-library "hideshow")
;; No longer needed. hideshow is now part of Emacs.

;;________________________________________________________________
;;    General
;;________________________________________________________________
(add-hook 'c++-mode-hook       (lambda () (hs-minor-mode 1)) )
(add-hook 'c-mode-hook         (lambda () (hs-minor-mode 1)) )
(add-hook 'java-mode-hook      (lambda () (hs-minor-mode 1)) )
(add-hook 'python-mode-hook    (lambda ()
                                 (when (> (count-lines (point-min) (point-max)) 25)
                                   (hs-minor-mode 1))) )
(add-hook 'makefile-mode-hook  (lambda () (hs-minor-mode 1)) )
(add-hook 'html-mode-hook      (lambda () (hs-minor-mode 1)) )
(add-hook 'jinja2-mode-hook    (lambda () (hs-minor-mode 1)) )
(add-hook 'qml-mode-hook       (lambda () (hs-minor-mode 1)) )
(add-hook 'swift-mode-hook     (lambda () (hs-minor-mode 1)) )

;; (add-hook 'css-mode-hook       (lambda () (hs-minor-mode 1)) )
;; (add-hook 'lua-mode-hook 'hs-minor-mode)

(defvar hs-minor-mode-hook)
(setq hs-minor-mode-hook  'hs-hide-initial-comment-block)

(global-set-key [M-f8] 'hs-toggle-hiding)

;;________________________________________________________________
;;    Highlighting - Enable hi lock globally
;;________________________________________________________________

;; Highlight current line
;; (global-hl-line-mode 1)
;; (set-face-background hl-line-face "grey30")
;; (setq global-hl-line-sticky-flag t)

;; hi lock is only available since Emacs 22.
(defvar hi-lock-file-patterns-policy)
(when (>= emacs-major-version 22)
  (global-hi-lock-mode 1)
  (setq hi-lock-file-patterns-policy (lambda (pattern) t))
)

;;________________________________
;;    hi-lock mini-manual
;;________________________________

;; `C-x w h REGEXP <RET> FACE <RET>'    highlight-regexp
;; `C-x w r REGEXP <RET>'               unhighlight-regexp
;; `C-x w l REGEXP <RET> FACE <RET>'    highlight-lines-matching-regexp
;; `C-x w b'                            hi-lock-write-interactive-patterns
;; `C-x w i'                            hi-lock-find-patterns

;; C-x w b: hi-lock-write-interactive-patterns
;; C-x w i: hi-lock-find-patterns

;;________________________________________________________________
;;    Handle files with mixed UNIX and DOS line endings.
;;________________________________________________________________

(defun remove-dos-eol ()
  "Do not use '^M' in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; *HACK AlERT*
;; In elisp source I replaced
;;    (c-basic-offset . 2)
;; with
;;    (c-basic-offset . 4)

;;________________________________
;; scss
;;________________________________

(try-require 'scss-mode)

;;________________________________
;; asciidoc   adoc-mode
;;________________________________

(try-require 'adoc-mode)
(setq auto-mode-alist (cons '("\\.adoc\\'" . adoc-mode) auto-mode-alist))

(add-hook 'adoc-mode-hook '(lambda () (auto-fill-mode 0)))

(add-hook 'adoc-mode-hook 'my-adoc-mode-hook)

(defun my-adoc-mode-hook ()
  "iimage as minor mode" (iimage-mode 1))

;;________________________________
;; markdown-mode
;;________________________________

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;________________________________________________________________
;;    TeX preview
;;________________________________________________________________

(autoload 'tex-math-preview "tex-math-preview" nil t)

(defvar texinfo-mode-map)
(add-hook 'texinfo-mode-hook
           (lambda ()
             (define-key texinfo-mode-map [f10] 'tex-math-preview)))

;;________________________________

(set-frame-parameter (selected-frame) 'alpha 100)


;;________________________________________________________________
;;    Continuously sync with disk to capture changes made
;;    in Xcode/ADT/Eclipse/VisualStudio.
;;________________________________________________________________

(global-auto-revert-mode t)

;;________________________________________________________________
;;    Cmake
;;________________________________________________________________
;; http://www.cmake.org/Wiki/CMake/Editors/Emacs

; Add cmake listfile names to the mode list.
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

(autoload 'cmake-mode "cmake-mode.el" t)
(defvar cmake-tab-width)
(setq cmake-tab-width 4)

;;________________________________________________________________
;;    YAML
;;________________________________________________________________

;; https://github.com/yoshiki/yaml-mode
(cond (on_darwin
       (when (try-require 'yaml-mode)
         (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
         )
       ))

;;________________________________________________________________
;;    CSV
;;________________________________________________________________

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;;________________________________________________________________
;;    Flag Python lines more than 87-columns wide
;;________________________________________________________________
;; Ref: https://www.emacswiki.org/emacs/EightyColumnRule
(require 'whitespace)
;; (setq whitespace-style '(face))
;; (global-whitespace-mode t)
;; (setq whitespace-line-column 84)

;; Ref: https://www.emacswiki.org/emacs/HighlightLongLines
(font-lock-add-keywords
     'python-mode
     '(("^[^\n]\\{87\\}\\(.*\\)$"
        1 font-lock-warning-face prepend)))

;;________________________________________________________________
;;    TypeScript
;;________________________________________________________________

;; (setq auto-mode-alist (cons '("\\.ts\\'" . typescript-mode) auto-mode-alist))

(when (try-require 'typescript-mode)
  (add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
)

;;________________________________________________________________
;;    INI
;;________________________________________________________________

(autoload 'ini-mode "ini-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ini\\'" . ini-mode))
(add-to-list 'auto-mode-alist '("\\.flake8\\'" . ini-mode))

;;================================================================
;; logview
;; Ref: https://raw.githubusercontent.com/doublep/logview/master/logview.el
;;================================================================
(try-require 'logview)


;;________________________________________________________________
;;    lsp-dart
;;________________________________________________________________
;; https://github.com/emacs-lsp/lsp-dart
;; Install use-package
(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

(use-package lsp-mode :ensure t)
(use-package lsp-dart
  :ensure t
  :hook (dart-mode . lsp))

;; Optional packages
(use-package projectile :ensure t) ;; project management
(use-package yasnippet
  :ensure t
  :config (yas-global-mode)) ;; snipets
(use-package lsp-ui :ensure t) ;; UI for LSP
(use-package company :ensure t) ;; Auto-complete

;; Optional Flutter packages
(use-package hover :ensure t) ;; run app from desktop without emulator
