;;****************************************************************
;;****************************************************************
;;****************************************************************
;;****************    FRAMES, FONTS, AND FACES    ****************
;;****************************************************************
;;****************************************************************
;;****************************************************************

(defvar on_windows_nt)
(defvar on_darwin)
(defvar on_gnu_linux)
(defvar on_cygwin)

;;________________________________________________________________
;;    Settings for default frames (make-frame-command, C-x 5 2)
;;________________________________________________________________

;;------------------------------------------------
;; Use (list-colors-display) to see all colors.
;;------------------------------------------------

(add-to-list 'default-frame-alist '(background-color . "black"))
(add-to-list 'default-frame-alist '(foreground-color . "white"))
(add-to-list 'default-frame-alist '(scroll-bar-foreground-color . "red"))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . right))
(add-to-list 'default-frame-alist '(scroll-bar-width . 17))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(left . 0))
(add-to-list 'default-frame-alist '(mouse-color . "#7b89d0"))
(add-to-list 'default-frame-alist '(cursor-color . "PaleVioletRed2")) ;; "#7b89d0" "#7ba9f0"

;;________________________________________________________________
;;    Settings for initial frame
;;________________________________________________________________

(add-to-list 'initial-frame-alist '(top . 200))
(add-to-list 'initial-frame-alist '(left . 0))

(add-to-list 'default-frame-alist '(top . 200))
(add-to-list 'default-frame-alist '(left . 0))

(cond (on_windows_nt
       (add-to-list 'default-frame-alist '(font . "Lucida Console-11:bold"))
       (add-to-list 'default-frame-alist '(height . 64))
       (add-to-list 'default-frame-alist '(width . 120))
       )
)

;; Recall the command text-scale-adjust
;; It is bound to C-x C-0, C-x C-=, C-x C--, C-x C-+.
;;   +, =   Increase the default face height by one step
;;   -      Decrease the default face height by one step
;;   0      Reset the default face height to the global default

(cond (on_darwin
       (add-to-list 'default-frame-alist '(font . "-apple-monaco-medium-r-normal--18-*-*-*-*-*-*-*"))
;;       (add-to-list 'default-frame-alist '(font . "-apple-monaco-medium-r-normal--14-*-*-*-*-*-*-*"))
       ;; (add-to-list 'default-frame-alist '(width . 130))
       ;; (add-to-list 'default-frame-alist '(height . 42))
       )
)

;; Bind the following to keys if necessary. Meanwhile just run:
;; (set-frame-font "-apple-monaco-medium-r-normal--24-*-*-*-*-*-*-*")
;; (set-frame-font "-apple-monaco-medium-r-normal--18-*-*-*-*-*-*-*")

(cond (on_gnu_linux
       (add-to-list 'default-frame-alist '(font . "9x15bold"))
       )
)

;; (cond (on_cygwin
;;        (set-default-font "9x15bold")
;; ))

;;________________________________
;; Position all frames at 0,0
;;________________________________

(set-frame-position (selected-frame) 0 200)

;; (add-hook 'after-make-frame-functions
;;           '(lambda (f)
;;              (with-selected-frame f
;;                (set-frame-position (selected-frame) 0 0)
;;                )))

;; (setq initial-frame-alist
;;       '((top . 1) (left . 1)))
;; 
;; (setq default-frame-alist
;;       '((top . 1) (left . 1) ))


;;________________________________
;; Use larger windows on bigger displays.
;;________________________________

;; (defun
;;     monitorwidth ()
;;   (interactive)
;;   (if (string= emacs-version "24.3.50.1")
;;       (nth 3 (assq 'geometry (car (display-monitor-attributes-list))))
;;     (x-display-pixel-width)
;;     )
;;   )

;; Monaco 14: 63 lines x 235 columns
;; Monaco 18: 49 lines x 170 columns
;; Monaco 24: 35 lines x 132 columns

;; (add-to-list 'default-frame-alist '(width . 171))
;; a(add-to-list 'default-frame-alist '(height . 47))

(add-to-list 'initial-frame-alist '(width . 134))
(add-to-list 'initial-frame-alist '(height . 45))
(add-to-list 'default-frame-alist '(width . 134))
(add-to-list 'default-frame-alist '(height . 45))

;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Will the following do the same thing?
;; (toggle-full-screen)

;; ;; Until Emacs 24: toggle-horizontal-scroll-bar: "Horizontal scroll bars aren't implemented yet"

;;________________________________________________________________
;;    Font lock
;;________________________________________________________________

;; Use font-lock everywhere.
(global-font-lock-mode t)

;; We have CPU cycles to spare. Highlight all syntax categories.
(setq font-lock-maximum-decoration t)

;; It is much more pleasant and less tiring to use a dark background.
(set-foreground-color "white")
(set-background-color "black")

;; Set cursor and mouse colours:
(set-cursor-color "PaleVioletRed2") ;; #7b89d0
(set-mouse-color "white")

;;________________________________________________________________
;;    Define a few colours that look good on reverse video:
;;________________________________________________________________

(set-face-foreground 'bold "LightGoldenrod")
(set-face-background 'bold "grey20")

(set-face-foreground 'font-lock-function-name-face "gold")
(set-face-background 'font-lock-function-name-face "gray10")
;; dired-directory inherits font-lock-function-name-face.

(set-face-background 'region "DarkSlateGrey")


(set-face-foreground 'bold-italic "yellow green")
(set-face-foreground 'italic "yellow3")
(set-face-foreground 'font-lock-comment-face "green")
(set-face-foreground 'font-lock-string-face "LightSalmon")

;; font-lock-doc-face inherits font-lock-string-face.
(set-face-foreground 'font-lock-doc-face "LightSalmon2")

(set-face-foreground 'font-lock-variable-name-face "LightGoldenrod")
(set-face-foreground 'font-lock-keyword-face "gold")

;; Face: font-lock-type-face
;; Documentation: Font Lock mode face used to highlight types and classes.
(set-face-foreground 'font-lock-type-face "cyan")

;; modeline became mode-line in Emacs 24.
(if (>= emacs-major-version 24)
    (progn
      (set-face-foreground 'mode-line "black") ;; "black")
      (set-face-background 'mode-line "SteelBlue2")) ;; "#5cacee" ""))
  (progn
    (set-face-foreground 'modeline "black")
    (set-face-background 'modeline "SteelBlue2")))
;; SteelBlue2 is #5cacee

(set-face-foreground 'highlight "black")
(set-face-foreground 'secondary-selection "black")

;; 'isearch

;;          Green    #00ff00
;;   LightSalmon2    #ee9572    238,149,114
;;           gold    #ffd700    255,215,0
;;         gray15    #262626
;; LightGoldenrod    #eedd82
;;     SteelBlue2    #5cacee    92,172,238
;;  DarkSlateGrey    #2f4f4f
;;     LightBlue2
;;     SteelBlue2

;;----------------------------------------------------------------
;;     What face is under the cursor?
;;----------------------------------------------------------------

;; "what-cursor-position with a prefix argument shows the face under
;; point, among other information. Keyboard shortcut is C-u C-x ="
;; https://stackoverflow.com/a/1242760/

;; https://stackoverflow.com/a/1242366/
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; Use M-x describe-face to see/edit the setting.


;;----------------------------------------------------------------
;;     Improvement over C-x o (other-window): Use Shift-arrow keys
;;----------------------------------------------------------------
;; Ref: https://www.emacswiki.org/emacs/WindMove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

