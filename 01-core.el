;;****************************************************************
;;****************************************************************
;;****************************************************************
;;****************             CORE               ****************
;;****************************************************************
;;****************************************************************
;;****************************************************************

;;________________________________________________________________
;;    Set up package repositories.
;;________________________________________________________________

(require 'package)

(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("MELPA Stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

;;________________________________________________________________
;;    package-install a list
;;    https://stackoverflow.com/a/10093312
;;________________________________________________________________

;; Package list
;; missing: (setq package-list '(ess typescript-mode markdown-mode logview ac-html markdown-preview-mode tide))
(setq package-list '(eslint-fix hideshow-org po-mode string-inflection qml-mode winum flycheck-pkg-config with-editor highlight yaml-mode window-number web-mode-edit-element smooth-scrolling smooth-scroll scss-mode qt-pro-mode pov-mode magit lua-mode js-auto-beautify jinja2-mode dtrt-indent dockerfile-mode datetime color-moccur cmake-mode browse-kill-ring py-import-check ini-mode csharp-mode flycheck-lilypond epl diff-hl edbi-sqlite edbi nginx-mode mmm-mode flycheck web-mode swift3-mode swift-mode exec-path-from-shell js2-mode json-mode csv-mode))
;; TODO: Avoid duplication with main .emacs file.

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;________________________________________________________________
;;    Determine where we are.
;;________________________________________________________________

(defvar system-type-as-string (prin1-to-string system-type))

(defvar on_windows_nt (string-match "windows-nt" system-type-as-string))
(defvar on_darwin     (string-match "darwin"     system-type-as-string))
(defvar on_gnu_linux  (string-match "gnu/linux"  system-type-as-string))
(defvar on_cygwin     (string-match "cygwin"     system-type-as-string))

;;________________________________________________________________
;;    Set the elisp path.
;;________________________________________________________________

(cond (on_darwin      (setq load-path (cons "~/ghali/dotfiles/emacs/elisp" load-path)) ))
(cond (on_windows_nt  (setq load-path (cons "~/ghali/dotfiles/emacs/elisp" load-path)) ))
(cond (on_gnu_linux   (setq load-path (cons "~/elisp"             load-path)) ))
(cond (on_cygwin      (setq load-path (cons "/cygdrive/h/elisp"   load-path)) ))

;;________________________________________________________________
;;    Stop cursor from blinking
;;________________________________________________________________

(blink-cursor-mode 0)
(setq x-stretch-cursor t)

;;________________________________________________________________
;; try-require: attempt to load a feature/library, failing silently
;;________________________________________________________________

(defvar missing-packages-list nil
  "List of packages that `try-require' can't find.")

(defun try-require (feature)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (condition-case err
      ;; protected form
      (progn
        (message "Checking for library `%s'..." feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "Checking for library `%s'... Found" feature))
    ;; error handler
    (file-error  ; condition
     (progn
       (message "Checking for library `%s'... Missing" feature)
       (add-to-list 'missing-packages-list feature))
     nil)))

;;________________________________________________________________
;;    Auto Decompress
;;________________________________________________________________

;; If we read a compressed file, uncompress it on the fly.
;; This works with .gz, .zip, .tar.gz, and .tgz files.
(auto-compression-mode 1)

;;________________________________________________________________
;;    Flash the screen on error; don't beep.
;;________________________________________________________________

(setq-default visible-bell t)

;;________________________________________________________________
;;    Disable "Package cl is deprecated" warnings.
;;________________________________________________________________
;; https://github.com/kiwanami/emacs-epc/issues/35
(setq byte-compile-warnings '(cl-functions))
