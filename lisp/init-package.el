
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

;;              `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
;;                ,(cons "melpa" (concat proto "://melpa.org/packages/"))))
;;             ('melpa-mirror
;;              `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
;;                ,(cons "melpa" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/"))))
;;             ('emacs-china
;;              `(,(cons "gnu"   (concat proto "://elpa.emacs-china.org/gnu/"))
;;                ,(cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/"))))
;;             ('netease
;;              `(,(cons "gnu"   (concat proto "://mirrors.163.com/elpa/gnu/"))
;;                ,(cons "melpa" (concat proto "://mirrors.163.com/elpa/melpa/"))))
;;             ('tuna
;;              `(,(cons "gnu"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
;;                ,(cons "melpa" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))
;;             (archives
;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

(use-package paradox
  :init
  (setq paradox-execute-asynchronously t)
  (setq paradox-github-token t)
  (setq paradox-display-star-count nil)
  (setq paradox-github-token  "a395c1cba60dcd145171fb6870f5550a3941e4b0")
  (defalias #'upgrade-packages #'paradox-upgrade-packages)

  ;; Replace default `list-packages'
  (defun my-paradox-enable (&rest _)
    "Enable paradox, overriding the default package-menu."
    (paradox-enable))
  (advice-add #'list-packages :before #'my-paradox-enable))

(provide 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
