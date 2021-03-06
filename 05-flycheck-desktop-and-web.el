;;****************************************************************
;;****************************************************************
;;****************************************************************
;;****************           FLYCHECK             ****************
;;****************        (and flyspell)          ****************
;;****************************************************************
;;****************************************************************
;;****************************************************************

(defvar on_windows_nt)
(defvar on_darwin)
(defvar on_gnu_linux)
(defvar on_cygwin)

;;________________________________
;;    flycheck
;;________________________________

;; Install Flycheck using MELPA.
;; http://www.flycheck.org/manual/latest/index.html
(try-require 'flycheck)
;; Enable flycheck globally:
(add-hook 'after-init-hook #'global-flycheck-mode)

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(custom-set-variables
 '(flycheck-python-flake8-executable "/Users/me/.big_38_venv/bin/flake8")
 '(flycheck-python-pycompile-executable "/Users/me/.big_38_venv/bin/python")
 '(flycheck-python-pylint-executable "/Users/me/.big_38_venv/bin/python"))
;; Ref: https://stackoverflow.com/a/55000284/

;; JS support is work in progress.
;; Relevant functions for JavaScript:
;; (flycheck-select-checker 'javascript-eslint)
;; (flycheck-mode)
;; (flycheck-eslint-config-exists-p)

;; ESlint is basically unusable. Far too many contrived constraints.

;; ;;================================================================
;; ;; ================eslint================
;; ;; use web-mode for .jsx files
;; (add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
;; 
;; ;; disable jshint since we prefer eslint checking
;; (setq-default flycheck-disabled-checkers
;;   (append flycheck-disabled-checkers
;;     '(javascript-jshint)))
;; 
;; ;; use eslint with web-mode for jsx files
;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;; 
;; ;; customize flycheck temp file prefix
;; (setq-default flycheck-temp-prefix ".flycheck")
;; 
;; ;; disable json-jsonlist checking for json files
;; ;; (setq-default flycheck-disabled-checkers
;; ;;   (append flycheck-disabled-checkers
;; ;;     '(json-jsonlist)))
;; 
;; ;; https://github.com/purcell/exec-path-from-shell
;; ;; only need exec-path-from-shell on OSX
;; ;; this hopefully sets up path and other vars better
;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))
;; ;;================================================================

;;________________________________
;;    flyspell
;;________________________________

(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook 'flyspell-mode)

;;________________________________
;;    C++11
;;________________________________

(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-gcc-language-standard "c++11")))

(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-include-path
                           (list (expand-file-name "/usr/local/Qt/5.15.1/clang_64/lib/")))))
