;;-*-Emacs-Lisp-*-
;; .emacs --  Sherif Ghali -- June 2020

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; Now (v.27?): Warning (package): Unnecessary call to ‘package-initialize’ in init file
;; (package-initialize)

;; (require 'loadhist)
;; (require 'cl)
;; (file-dependents (feature-file 'cl))


(load-file "~/ghali/dotfiles/dotemacs/01-core.el")
(load-file "~/ghali/dotfiles/dotemacs/02-key-mappings.el")
(load-file "~/ghali/dotfiles/dotemacs/03-dev-desktop-modes.el")
(load-file "~/ghali/dotfiles/dotemacs/04-dev-web-mode.el")
(load-file "~/ghali/dotfiles/dotemacs/05-flycheck-desktop-and-web.el")
(load-file "~/ghali/dotfiles/dotemacs/06-programming-indentation.el")
(load-file "~/ghali/dotfiles/dotemacs/07-frames-fonts-and-faces.el")
(load-file "~/ghali/dotfiles/dotemacs/08-environment-paths.el")
(load-file "~/ghali/dotfiles/dotemacs/09-files-and-dired.el")
(load-file "~/ghali/dotfiles/dotemacs/10-version-control.el")
(load-file "~/ghali/dotfiles/dotemacs/11-misc-general.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(csv-separators '("," "	" ";"))
 '(flycheck-checker-error-threshold 1000)
 '(flycheck-python-flake8-executable "/Users/me/.big_38_venv/bin/flake8")
 '(flycheck-python-pycompile-executable "/Users/me/.big_38_venv/bin/python")
 '(flycheck-python-pylint-executable "/Users/me/.big_38_venv/bin/python")
 '(package-selected-packages
   '(groovy-mode hover company lsp-ui yasnippet projectile lsp-dart lsp-mode use-package gradle-mode eslint-fix hideshow-org po-mode ess string-inflection qml-mode winum flycheck-pkg-config with-editor highlight yaml-mode window-number web-mode-edit-element typescript-mode smooth-scrolling smooth-scroll scss-mode qt-pro-mode pov-mode markdown-mode magit lua-mode logview js-auto-beautify jinja2-mode dtrt-indent dockerfile-mode datetime color-moccur cmake-mode browse-kill-ring py-import-check ac-html ini-mode csharp-mode flycheck-lilypond epl diff-hl edbi-sqlite edbi nginx-mode markdown-preview-mode mmm-mode flycheck web-mode swift3-mode swift-mode exec-path-from-shell js2-mode json-mode tide csv-mode))
 '(py-use-font-lock-doc-face-p t)
 '(safe-local-variable-values
   '((dockerfile-image-name . "my-image-name-here")
     (flycheck-gcc-language-standard . c++11))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markup-big-face ((t (:inherit markup-gen-face :height 1.0))))
 '(markup-internal-reference-face ((t (:inherit markup-meta-face))))
 '(markup-meta-face ((t (:inherit markup-gen-face :height 1.0))))
 '(markup-meta-hide-face ((t (:inherit markup-gen-face :height 1.0))))
 '(markup-secondary-text-face ((t (:inherit markup-gen-face :height 1.0))))
 '(markup-subscript-face ((t (:inherit markup-gen-face :height 1.0))))
 '(markup-superscript-face ((t (:inherit markup-gen-face :height 1.0))))
 '(markup-title-0-face ((t (:inherit markup-gen-face :height 1.0))))
 '(markup-title-1-face ((t (:inherit markup-gen-face :height 1.0))))
 '(markup-title-2-face ((t (:inherit markup-gen-face :height 1.0))))
 '(markup-title-3-face ((t (:inherit markup-gen-face :height 1.0))))
 '(markup-title-4-face ((t (:inherit markup-gen-face :height 1.0))))
 '(markup-title-5-face ((t (:inherit markup-gen-face :height 1.0))))
 '(py-decorators-face ((t (:foreground "DeepSkyBlue1"))))
 '(small-face ((t (:inherit markup-gen-face :height 1.0)))))
