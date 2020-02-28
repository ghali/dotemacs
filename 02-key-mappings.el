;;****************************************************************
;;****************************************************************
;;****************************************************************
;;****************         Key mappings           ****************
;;****************************************************************
;;****************************************************************
;;****************************************************************

(defvar sg-on_windows_nt)
(defvar sg-on_darwin)
(defvar sg-on_gnu_linux)
(defvar sg-on_cygwin)

;;________________________________________________________________
;;    Basic Keybindings
;;________________________________________________________________

;; The following key binding iconifies emacs. We disable it.
(global-unset-key "\C-x\C-z")
;; The following key binding quits emacs. We disable it too.
(global-unset-key "\C-x\C-c")
;; But we establish a longer sequence that is harder to hit by accident.
(global-set-key "\C-x\C-c\C-v" 'save-buffers-kill-emacs)
;; C-x j, which is bound to kanji entering, is too close to dired's C-x C-j
(global-unset-key "\C-xj")
;; Disable binding of C-z to iconify a window.
(global-unset-key "\C-z")
;; M-` invokes tmm-menubar; disable it.
(global-unset-key "\M-`")
;; C-x C-n invokes set-goal-column; disable it.
(global-unset-key "\C-x\C-n")
;; Set C-c C-z to invoke a shell inside emacs.
(global-set-key "\C-c\C-z" 'eshell)

;;________________________________________________________________
;;    Scrolling
;;________________________________________________________________

;; We also map scroll wheel and trackpad events to scrolling.
;; The mouse wheel on windows generates few events.
;; Scroll by 3 unless shifted.

(defun up-slow () (interactive) (scroll-up 1))
(defun down-slow () (interactive) (scroll-down 1))

(defun up-semi-slow () (interactive) (scroll-up 2))
(defun down-semi-slow () (interactive) (scroll-down 2))

(defun up-medium () (interactive) (scroll-up 3))
(defun down-medium () (interactive) (scroll-down 3))

(cond (sg-on_windows_nt
       ;; xemacs won't like the following:
       (global-set-key [mouse-4] 'down-medium)
       (global-set-key [mouse-5] 'up-medium)

       (global-set-key [S-mouse-4] 'down-slow)
       (global-set-key [S-mouse-5] 'up-slow)
))

;; The trackpad on macOS generates too many events.
;; Scroll by 1 unless shifted.
(cond (sg-on_darwin
       (global-set-key [mouse-4] 'down-slow)
       (global-set-key [mouse-5] 'up-slow)

       (global-set-key [S-mouse-4] 'down-medium)
       (global-set-key [S-mouse-5] 'up-medium)
))

(cond (sg-on_gnu_linux
       (global-set-key [mouse-4] 'down-medium)
       (global-set-key [mouse-5] 'up-medium)

       (global-set-key [S-mouse-4] 'down-slow)
       (global-set-key [S-mouse-5] 'up-slow)
))

(defun up-fast () (interactive) (scroll-up 8))
(defun down-fast () (interactive) (scroll-down 8))
(global-set-key [C-mouse-4] 'down-fast)
(global-set-key [C-mouse-5] 'up-fast)

;; Ordinarily emacs jumps by half a page when scrolling -- reduce:
(setq scroll-step 1)
;; (setq scroll-conservatively 1)

;; The default value is 5, which is too fast on a MacBook or a trackpad; reduce:
(when (and sg-on_darwin window-system)
  (mouse-wheel-mode 1)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
  (setq mouse-wheel-progressive-speed 'f)
  )

;;________________________________________________________________
(defvar sg-scroll-counter)
(defvar sg-scroll-limit)
(defvar sg-last-scroll)
(defvar sg-scroll-interval)

(setq sg-scroll-counter 0)
(setq sg-scroll-limit 5)
(setq sg-last-scroll 0)
(setq sg-scroll-interval 0.1)

(defun up1()
  (interactive)
  (let ((now (float-time)))
    (if (and (eq last-command this-command)
             (< (- now sg-last-scroll) sg-scroll-interval))
        (incf sg-scroll-counter)
      (setq sg-scroll-counter 0))
    (setq sg-last-scroll now))

  (if (> sg-scroll-counter sg-scroll-limit)
      (scroll-up 2)
    (scroll-up 1)))

(defun down1()
  (interactive)
  (let ((now (float-time)))
    (if (and (eq last-command this-command)
             (< (- now sg-last-scroll) sg-scroll-interval))
        (incf sg-scroll-counter)
      (setq sg-scroll-counter 0))
    (setq sg-last-scroll now))

  (if (> sg-scroll-counter sg-scroll-limit)
      (scroll-down 2)
    (scroll-down 1)))

;; Make it possible to scroll from the keyboard.
(global-set-key "\M-N" 'up1)
(global-set-key "\M-P" 'down1)

(global-set-key "\M-]" 'up-slow)
(global-set-key "\M-[" 'down-slow)

;; Slow down mouse wheel (trackpad) scrolling.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;;________________________________________________________________
;;    Function definitions (bound to function keys)
;;________________________________________________________________

(defun altotop () "Align at Top of Screen" (interactive) (recenter 1))
;; now that recenter-top-bottom is C-l, use F7 for something other than (recenter 1).
(defun funcsumm () "function summary" (interactive)
  (occur  "^void\\|int [a-zA-Z_]*([^$])[^;]$"))
;;  (occur  "\(void\|int\) *"))
(defun ptreg1 () "Save current location in slot 1" (interactive) (point-to-register 1))
(defun ptreg2 () "Save current location in slot 2" (interactive) (point-to-register 2))
(defun ptreg3 () "Save current location in slot 3" (interactive) (point-to-register 3))
(defun ptreg4 () "Save current location in slot 4" (interactive) (point-to-register 4))

(defun gtreg1 () "Goto saved location 1" (interactive) (jump-to-register 1))
(defun gtreg2 () "Goto saved location 2" (interactive) (jump-to-register 2))
(defun gtreg3 () "Goto saved location 3" (interactive) (jump-to-register 3))
(defun gtreg4 () "Goto saved location 4" (interactive) (jump-to-register 4))

;;________________________________________________________________
;;    Function Key binding
;;________________________________________________________________

(global-set-key [f1] 'goto-line)
(global-set-key [f2] 'what-line)

(global-set-key [S-f3] 'ptreg1)
(global-set-key [f3] 'gtreg1)
(global-set-key [S-f4] 'ptreg2)
(global-set-key [f4] 'gtreg2)
(global-set-key [S-f5] 'ptreg3)
(global-set-key [f5] 'gtreg3)
(global-set-key [S-f6] 'ptreg4)
(global-set-key [f6] 'gtreg4)

;; The following two commands load the source file automatically and point
;; to the error/warning line. This make emacs essentially a (mouse-free) IDE:
(global-set-key [f7] 'next-error)
(global-set-key [S-f7] 'previous-error)

(global-set-key [f8] 'bury-buffer)
(global-set-key [S-f8] 'unbury-buffer)

;; We also set [M-f8] below to hs-toggle-hiding

(global-set-key [M-f9] 'shrink-window)
(global-set-key [M-f10] 'enlarge-window)

;; We leave intact macOS's definitions for f9 - f12.
;; (global-set-key [f9] 'delete-other-windows) ; ^x 1
;; (global-set-key [f10] 'other-window)    ; ^x o
;; (global-set-key [f11] ..)
;; (global-set-key [f12] ..)

;; Windows issues f13 for S-f3. Define.
(global-set-key [f13] 'ptreg1)
(global-set-key [f14] 'ptreg2)
(global-set-key [f15] 'ptreg3)
(global-set-key [f16] 'ptreg4) ;; by default f16 maps to <print>.
(global-set-key [f17] 'eval-region)

;;________________________________________________________________
;;    Settings for compilation
;;________________________________________________________________

;; Set a brief key binding for compiling.
(global-set-key "\M-C" 'compile)
;; By default 'compile calls "make -k"

;;________________________________________________________________
;;    Unlike emacs 22, emacs 23 & 24 map alt/option to Meta.
;;________________________________________________________________

(defvar mac-option-key-is-meta)
(defvar mac-command-key-is-meta)

(cond (sg-on_darwin
       (when (>= emacs-major-version 23)
         (setq mac-option-key-is-meta nil)
         (setq mac-command-key-is-meta t)
         (setq mac-command-modifier 'meta)
         (setq mac-option-modifier nil)
         )
))

;;________________________________________________________________
;;    Emacs 23 & 24 cut off M-` from OS X. Handle within emacs.
;;________________________________________________________________

(global-set-key (kbd "M-`") 'other-frame)
;; For some reason, emacs 22 wants to see M-§.
;; (global-set-key (kbd "M-§") 'other-frame)

;;________________________________________________________________
;;    Choose interactively from the kill ring.
;;________________________________________________________________

;; yank ring
;; browse-kill-ring is not available in melpa.
(when (try-require 'browse-kill-ring)
  (global-set-key (kbd "C-c C-k") 'browse-kill-ring)
)
