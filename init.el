(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 and above!"))

;; Speed up startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 80000000)
(add-hook 'emacs-startup-hook
      (lambda ()
        "Restore defalut values after init."
        (setq file-name-handler-alist default-file-name-handler-alist)
        (setq gc-cons-threshold 800000)
        (if (boundp 'after-focus-change-function)
        (add-function :after after-focus-change-function
                  (lambda ()
                (unless (frame-focus-state)
                  (garbage-collect))))
          (add-hook 'focus-out-hook 'garbage-collect))))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "plugin" user-emacs-directory) load-path)
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path)
  (push (expand-file-name "lisp" user-emacs-directory) load-path))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
      (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; Constants
(require 'init-const)

;; Customization
(require 'init-custom)

;; Packages
;; Without this comment Emacs25 adds (package-initialize) here
(require 'init-package)
(require 'init-hydra)
;; Preferences
(require 'init-basic)
(require 'init-ui)
(require 'init-edit)
(require 'init-ivy)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-pyim)
(require 'init-dired)
(require 'init-highlight)
(require 'init-window)
(require 'init-eshell)
(require 'init-shell)

(require 'init-org)
(require 'init-utils)

(require 'init-vcs)
(require 'init-flycheck)
(require 'init-projectile)

(require 'init-emacs-lisp)
(require 'init-cmake)

(require 'init-c)
(require 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
