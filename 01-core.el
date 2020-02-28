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
(add-to-list 'package-archives
             '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("MELPA" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;;________________________________________________________________
;;    Determine where we are.
;;________________________________________________________________

(defvar system-type-as-string (prin1-to-string system-type))

(defvar sg-on_windows_nt (string-match "windows-nt" system-type-as-string))
(defvar sg-on_darwin     (string-match "darwin"     system-type-as-string))
(defvar sg-on_gnu_linux  (string-match "gnu/linux"  system-type-as-string))
(defvar sg-on_cygwin     (string-match "cygwin"     system-type-as-string))

;;________________________________________________________________
;;    Set the elisp path.
;;________________________________________________________________

(cond (sg-on_darwin      (setq load-path (cons "~/ghali/dotfiles/emacs/elisp" load-path)) ))
(cond (sg-on_windows_nt  (setq load-path (cons "~/ghali/dotfiles/emacs/elisp" load-path)) ))
(cond (sg-on_gnu_linux   (setq load-path (cons "~/elisp"             load-path)) ))
(cond (sg-on_cygwin      (setq load-path (cons "/cygdrive/h/elisp"   load-path)) ))

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
