;;****************************************************************
;;****************************************************************
;;****************************************************************
;;****************       ENVIRONMENT, PATHS       ****************
;;****************************************************************
;;****************************************************************
;;****************************************************************

(defvar sg-on_windows_nt)
(defvar sg-on_darwin)
(defvar sg-on_gnu_linux)
(defvar sg-on_cygwin)

;;________________________________________________________________
;;    Info
;;________________________________________________________________

;;________________________________
;; Adjust the info path.
;;________________________________

(cond (sg-on_windows_nt
       (setq Info-directory-list
             (cons 
              (expand-file-name "C:/cygwin17/usr/share/info/")
              Info-default-directory-list))
))

;; Other places to look for info files:
;; setenv INFOPATH /usr/gnu/info:/usr/local/info:/usr/TeX/info
;; /usr/share/info/

;;(cond (sg-on_darwin
;;       (push "/usr/local/share/info" Info-directory-list)
;;))

;; Is this really necessary? Shouldn't Info-default-directory-list,
;; which already includes "/usr/local/share/info", be sufficient?

;;________________________________
;; Load .info pages in info-mode
;;________________________________

(defun info-mode ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (kill-buffer (current-buffer))
    (info file-name)))
(add-to-list 'auto-mode-alist '("\\.info\\'" . info-mode))

;;________________________________________________________________
;;    Path
;;________________________________________________________________

(setq shell-file-name "bash")

(setq exec-path
      (append exec-path
              '("/usr/texbin"
                "/usr/local/bin"
                "/Applications/Ipe.app/Contents/MacOS/ipe"
                "/usr/local/ghostscript-8.71-macosx"
                "/usr/local/Qt/5.7/clang_64/bin/"
                "/opt/local/bin")))

(cond (sg-on_darwin
       (require 'exec-path-from-shell)
       (exec-path-from-shell-initialize)

       (setenv "DYLD_LIBRARY_PATH" "/usr/local/glow/glow_src/:/usr/local/ipe/build/lib/")
       (setenv "PKG_CONFIG_PATH" "/usr/local/Trolltech/Qt-4.8.0/lib/pkgconfig")

       ;; (setenv "ANDROID_NDK_ROOT" "/usr/local/android-ndk-r9d")
       ;; (setenv "ANDROID_SDK_ROOT" "/usr/local/adt-bundle-mac-x86_64/sdk")

       (setenv "ANDROID_HOME" "/usr/local/android/android-sdk-macosx/")
       (setenv "ANDROID_NDK_HOST" "darwin-x86_64")
       (setenv "ANDROID_NDK_PLATFORM" "android-12  ")
       (setenv "ANDROID_NDK_ROOT" "/usr/local/android/android-ndk-r10d/")
       (setenv "ANDROID_NDK_TOOLCHAIN_PREFIX" "arm-linux-androideabi  ")
       (setenv "ANDROID_NDK_TOOLCHAIN_VERSION" "4.8  ")
       (setenv "ANDROID_NDK_TOOLS_PREFIX" "arm-linux-androideabi  ")
       (setenv "ANDROID_SDK_ROOT" "/usr/local/android/android-sdk-macosx/")
       (setenv "ANDROID_API_VERSION" "android-12  ")
       (setenv "PYTHONIOENCODING" "utf_8")
))

(cond (sg-on_windows_nt
       (setenv "INCLUDE"
           (concat "C:/Program Files/Microsoft Visual Studio 9.0/VC/ATLMFC/INCLUDE;"
                       "C:/Program Files/Microsoft Visual Studio 9.0/VC/INCLUDE;"
                       "C:/Program Files//Microsoft SDKs/Windows/v6.0A/include"))

       (setenv "LIB"
           (concat "C:/Program Files/Microsoft Visual Studio 9.0/VC/ATLMFC/LIB;"
                       "C:/Program Files/Microsoft Visual Studio 9.0/VC/LIB;"
                       "C:/Program Files//Microsoft SDKs/Windows/v6.0A/lib"))


       (setenv "LIBPATH"
           (concat "C:/WINNT/Microsoft.NET/Framework/v3.5;C:/WINNT/Microsoft.NET/Framework/v2.0.50727;"
                       "C:/Program Files/Microsoft Visual Studio 9.0/VC/ATLMFC/LIB;"
                       "C:/Program Files/Microsoft Visual Studio 9.0/VC/LIB"))

       (setenv "PATH"
           (concat "C:/Program Files/Microsoft Visual Studio 9.0/Common7/IDE;"
                       "C:/Program Files/Microsoft Visual Studio 9.0/VC/BIN;"
                       "C:/Program Files/Microsoft Visual Studio 9.0/Common7/Tools;"
                       "C:/WINNT/Microsoft.NET/Framework/v3.5;"
                       "C:/WINNT/Microsoft.NET/Framework/v2.0.50727;"
                       "C:/Program Files/Microsoft Visual Studio 9.0/VC/VCPackages;"
                       "C:/Program Files//Microsoft SDKs/Windows/v6.0A/bin;"
                       "C:/Qt/4.7.1/bin;C:/WINNT/system32;C:/WINNT;C:/WINNT/system32/WBEM;"
                       "C:/Program Files/Common Files/OTG;C:/cygwin17/bin"))
))

(cond (sg-on_gnu_linux
       (setenv "PATH"
           (concat "/usr/kerberos/bin:"
                       "/usr/local/bin:"
                       "/bin:"
                       "/usr/bin:"
                       "/apps/home/ghali/bin:"
                       "/usr/local/Trolltech/Qt-4.7.1/bin:"
                       "."))
))

;;________________________________________________________________
;;    eshell path
;;________________________________________________________________

(cond (sg-on_windows_nt
       (add-hook 'eshell-mode-hook
                 '(lambda nil
                    (eshell/export "EPOCROOT=\\Paragon\\")
                    (let ((path))

                      (setq path (concat "C:/Program Files/Microsoft Visual Studio 9.0/Common7/IDE"
                                         ";C:/Program Files/Microsoft Visual Studio 9.0/VC/bin"
                                         ";C:/Program Files/Microsoft Visual Studio 9.0/VC/BIN"
                                         ";C:/Program Files/Microsoft Visual Studio 9.0/Common7/Tools"
                                         ";C:/WINNT/Microsoft.NET/Framework/v3.5"
                                         ";C:/WINNT/Microsoft.NET/Framework/v2.0.50727"
                                         ";C:/Program Files/Microsoft Visual Studio 9.0/VC/VCPackages"
                                         ";C:/Program Files//Microsoft SDKs/Windows/v6.0A/bin"
                                         ";C:/Qt/4.7.1/bin"
                                         ";C:/WINNT/system32"
                                         ";C:/WINNT"
                                         ";C:/WINNT/system32/WBEM"
                                         ";C:/Program Files/Common Files/OTG"
                                         ";C:/cygwin17/bin"))
                      (setenv "PATH" path))
                    (local-set-key "\C-u" 'eshell-kill-input))
                 )
       ))

(cond (sg-on_darwin
       (add-hook 'eshell-mode-hook
         '(lambda nil
            (eshell/export "EPOCROOT=\\Paragon\\")
            (let ((path))

              (setq path (concat "/Users/me/.rvm/bin/rvm"
                                         ";/Users/me/.rvm/gems/ruby-1.9.2-p180@rails3tutorial/bin"
                                         ";/opt/local/bin"
                                         ";/usr/local/adt-bundle-mac-x86_64/sdk/tools"
                                         ";/usr/local/adt-bundle-mac-x86_64/sdk/platform-tools"
                                         ))
              (setenv "PATH" path))
            (local-set-key "\C-u" 'eshell-kill-input))
         )
))

;;________________________________________________________________
;;    Qt & Visual Studio
;;________________________________________________________________

(cond (sg-on_windows_nt
       (setenv "QTDIR" "C:\Qt\4.7.1")
       (setenv "QMAKESPEC" "win32-msvc2008")
))

;; Setting QMAKESPEC avoids:
;; Warning: Generator: MSVC.NET: Found more than one version of
;; Visual Studio, but none in your path! Fallback to lowest
;; version (MSVC.NET 2008 (9.0), MSVC.NET 2003 (7.1))

;;________________________________________________________________
;;    SVN
;;________________________________________________________________

;; The executable is Cygwin's, but we can use it on bare windows.
;; (cond (sg-on_windows_nt
;;        (require 'psvn)
;;        (setq svn-status-svn-executable "C:/cygwin17/bin/svn.exe")
;; ))

;; svn issues a warning ("cannot set LC_CTYPE locale") if LANG is not set.
(setenv "LANG" "C")

;;________________________________________________________________
;;    diff
;;________________________________________________________________

;; Even though C:/cygwin17/bin is in the PATH, we need to specify the
;; complete path here; It is not enough to call "diff.exe".
;; 
;; Alternatively, add Cygwin17/bin to the windows-wide path
;; (Control Panel \ System \ Advanced)
(defvar ediff-diff-program)
(cond (sg-on_windows_nt
       (setq ediff-diff-program "C:/cygwin17/bin/diff.exe")
))

;; Let ediff handle various text encodings
(setq ediff-diff-options "--text")

;;________________________________________________________________
;;    Ask Cygwin to accept the windows directory separator quietly
;;________________________________________________________________
;; If we don't, emacs's ediff will be confused by the message
;;     cygwin warning:
;;       MS-DOS style path detected: C:\\Documents and Settings\\ghali\\...
;;       Preferred POSIX equivalent is: /cygdrive/c/Documents and Settings/ghali/...
;;       CYGWIN environment variable option "nodosfilewarning" turns off this warning.
;;       Consult the user's guide for more details about POSIX paths:
;;         http://cygwin.com/cygwin-ug-net/using.html#using-pathnames

(cond (sg-on_windows_nt
       (setenv "CYGWIN" "nodosfilewarning")
))
