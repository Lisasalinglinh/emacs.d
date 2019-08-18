;;; init-ivy.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Ivy configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package counsel
  :diminish ivy-mode counsel-mode
  :defines (projectile-completion-system magit-completing-read-function recentf-list)
  :bind (("C-s" . swiper-isearch)
         ("C-S-s" . swiper-all)

         ("C-c C-r" . ivy-resume)
         ("C-c v p" . ivy-push-view)
         ("C-c v o" . ivy-pop-view)
         ("C-c v ." . ivy-switch-view)

         :map counsel-mode-map
         ([remap swiper] . counsel-grep-or-swiper)
         ([remap dired] . counsel-dired)
         ("C-x C-r" . counsel-recentf)
         ("C-x j" . counsel-mark-ring)
         ("C-c c F" . counsel-faces)
         ("C-h F" . counsel-describe-face)
         ("C-c c L" . counsel-load-library)
         ("C-c c P" . counsel-package)
         ("C-c c a" . counsel-apropos)
         ("C-c c e" . counsel-colors-emacs)
         ("C-c c f" . counsel-find-library)
         ("C-c c g" . counsel-grep)
         ("C-c c h" . counsel-command-history)
         ("C-c c i" . counsel-git)
         ("C-c c j" . counsel-git-grep)
         ("C-c c l" . counsel-locate)
         ("C-c c m" . counsel-minibuffer-history)
         ("C-c c o" . counsel-outline)
         ("C-c c p" . counsel-pt)
         ("C-c c r" . counsel-rg)
         ("C-c c s" . counsel-ag)
         ("C-c c t" . counsel-load-theme)
         ("C-c c u" . counsel-unicode-char)
         ("C-c c w" . counsel-colors-web)
         ("C-c c z" . counsel-fzf)

         :map ivy-minibuffer-map
         ("C-w" . ivy-yank-word)
         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)

         :map swiper-map
         ("M-%" . swiper-query-replace))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :custom-face
  (ivy-current-match ((t (:background "#6e6e8b"))))
  :preface
  (defun ivy-format-function-pretty (cands)
    "Transform CANDS into a string for minibuffer"
    (ivy--format-function-generic
     (lambda (str)
       (concat (if (display-graphic-p)
                   (all-the-icons-faicon "hand-o-right" :height .85 :v-adjust .05 :face 'font-lock-constant-face))
               (propertize " " 'display `(space :align-to 3))
               (ivy--add-face str 'ivy-current-match)))
     (lambda (str)
       (concat (propertize " " 'display `(space :align-to 3)) str))
     cands
     "\n"))
  :custom
  ( enable-recursive-minibuffers t) ; Allow commands in minibuffers
  ( ivy-use-selectable-prompt t)
  ( ivy-use-virtual-buffers t)    ; Enable bookmarks and recentf
  ( ivy-height 10)
  ( ivy-count-format "(%d/%d) ")
  ( ivy-on-del-error-function nil)
  ( ivy-initial-inputs-alist nil)
  ( swiper-action-recenter t)
  ( counsel-find-file-at-point t)
  ( counsel-preselect-current-file t)

  :config
  (add-to-list 'ivy-format-functions-alist '(t . ivy-format-function-pretty))
  (add-to-list 'ivy-format-functions-alist '(counsel-describe-face . counsel--faces-format-function))
  ;; using ivy-format-function-arrow with counsel-yank-pop
  (advice-add
   'counsel--yank-pop-format-function
   :override
   (lambda (cand-pairs)
     (ivy--format-function-generic
      (lambda (str)
        (mapconcat
         (lambda (s)
           (ivy--add-face (concat (propertize "|>" 'face `(:foreground "green")) s) 'ivy-current-match))
         (split-string
          (counsel--yank-pop-truncate str) "\n" t)
         "\n"))
      (lambda (str)
        (counsel--yank-pop-truncate str))
      cand-pairs
      counsel-yank-pop-separator)))
  ;;this variable do not work if defined in :cunstom
  (setq counsel-yank-pop-separator
        (propertize "\n***************************************************************\n"
                    'face `(:foreground "#6272a4")))

  ;; Use faster search tools: ripgrep or the silver search
  (let ((cmd (cond ((executable-find "rg")
                    "rg -S --no-heading --line-number --color never '%s' %s")
                   ((executable-find "ag")
                    "ag -S --noheading --nocolor --nofilename --numbers '%s' %s")
                   (t counsel-grep-base-command))))
    (setq counsel-grep-base-command cmd))

  ;; Build abbreviated recent file list.
  (defun my-counsel-recentf ()
    "Find a file on `recentf-list'."
    (interactive)
    (require 'recentf)
    (recentf-mode)
    (ivy-read "Recentf: " (mapcar #'abbreviate-file-name recentf-list)
              :action (lambda (f)
                        (with-ivy-window
                          (find-file f)))
              :require-match t
              :caller 'counsel-recentf))
  (advice-add #'counsel-recentf :override #'my-counsel-recentf)

  ;; Pre-fill for commands
  ;; @see https://www.reddit.com/r/emacs/comments/b7g1px/withemacs_execute_commands_like_marty_mcfly/
  (setq my-ivy-fly-commands
        '(query-replace-regexp
          flush-lines
          keep-lines
          ivy-read
          swiper
          swiper-isearch
          counsel-grep
          counsel-ag
          counsel-rg
          counsel-pt))
  (defun my-ivy-fly-back-to-present ()
    ;; (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t)
    (cond ((and (memq last-command my-ivy-fly-commands)
                (equal (this-command-keys-vector) (kbd "M-p")))
           ;; repeat one time to get straight to the first history item
           (setq unread-command-events
                 (append unread-command-events
                         (listify-key-sequence (kbd "M-p")))))
          ((or (memq this-command '(self-insert-command
                                    yank
                                    ivy-yank-word
                                    counsel-yank-pop))
               (equal (this-command-keys-vector) (kbd "M-n")))
           (delete-region (point)
                          (point-max)))))

  (defun my-ivy-fly-time-travel ()
    (when (memq this-command my-ivy-fly-commands)
      (let* ((kbd (kbd "M-n"))
             (cmd (key-binding kbd))
             (future (and cmd
                          (with-temp-buffer
                            (when (ignore-errors
                                    (call-interactively cmd) t)
                              (buffer-string))))))
        (when future
          (save-excursion

            (insert (propertize (replace-regexp-in-string
                                 "\\\\_<" ""
                                 (replace-regexp-in-string
                                  "\\\\_>" ""
                                  future))
                                'face 'shadow)))
          (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t))
        (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t))))

  (add-hook 'minibuffer-setup-hook #'my-ivy-fly-time-travel)
  ;; Improve search experience of `swiper'
  ;; @see https://emacs-china.org/t/swiper-swiper-isearch/9007/12
  (defun my-swiper-toggle-counsel-rg ()
    "Toggle `counsel-rg' with current swiper input."
    (interactive)
    (let ((text (replace-regexp-in-string
                 "\n" ""
                 (replace-regexp-in-string
                  "\\\\_<" ""
                  (replace-regexp-in-string
                   "\\\\_>" ""
                   (replace-regexp-in-string "^.*Swiper: " ""
                                             (thing-at-point 'line t)))))))
      (ivy-quit-and-run
        (counsel-rg text default-directory))))
  (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg swiper-map)

  (with-eval-after-load 'rg
    (defun my-swiper-toggle-rg-dwim ()
      "Toggle `rg-dwim' with current swiper input."
      (interactive)
      (ivy-quit-and-run (rg-dwim default-directory)))
    (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim swiper-map)
    (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim ivy-minibuffer-map))

  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  ;; Integration with `magit'
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))

  ;; Enhance M-x
  (use-package amx
    :init (setq amx-history-length 20))
  ;; Better sorting and filtering
  (use-package prescient
    :commands prescient-persist-mode
    :init
    (setq prescient-filter-method '(literal regexp initialism fuzzy))
    (prescient-persist-mode 1))

  (use-package ivy-prescient
    :commands ivy-prescient-re-builder
    :custom-face (ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil))))
    :preface
    (defun ivy-prescient-non-fuzzy (str)
      (let ((prescient-filter-method '(literal regexp)))
        (ivy-prescient-re-builder str)))
    :init
    (setq ivy-prescient-enable-filtering t
          ivy-prescient-retain-classic-highlighting t
          ivy-re-builders-alist '((counsel-ag . ivy-prescient-non-fuzzy)
                                  (counsel-rg . ivy-prescient-non-fuzzy)
                                  (counsel-pt . ivy-prescient-non-fuzzy)
                                  (counsel-grep . ivy-prescient-non-fuzzy)
                                  (swiper . ivy-prescient-non-fuzzy)
                                  (swiper-isearch . ivy-prescient-non-fuzzy)
                                  (swiper-all . ivy-prescient-non-fuzzy)
                                  (t . ivy-prescient-re-builder)))
    (ivy-prescient-mode 1))

  ;; Additional key bindings for Ivy
  (use-package ivy-hydra
    :bind (:map ivy-minibuffer-map
           ("M-o" . ivy-dispatching-done-hydra)))
  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :init
    (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))
    (counsel-projectile-mode 1))

  ;; Integrate yasnippet
  (use-package ivy-yasnippet
    :commands ivy-yasnippet--preview
    :bind ("C-c C-y" . ivy-yasnippet)
    :config (advice-add #'ivy-yasnippet--preview :override #'ignore))
  (use-package ivy-posframe
    :init(ivy-posframe-mode 1)
    :custom-face
    (ivy-posframe ((t (:background "#282a36"))))
    (ivy-posframe-border ((t (:background "#6272a4"))))
    :config
    (setq ivy-posframe-display-functions-alist
          '((swiper . nil)
            (swiper-isearch . nil)
            (complete-symbol . ivy-posframe-display-at-point)
            (counsel-M-x . ivy-posframe-display-at-frame-bottom-left)
            (t . ivy-posframe-display-at-frame-center))))
  ;; More friendly display transformer for Ivy
  (use-package ivy-rich
    :defines (all-the-icons-icon-alist
              all-the-icons-dir-icon-alist
              bookmark-alist)
    :functions (all-the-icons-icon-for-file
                all-the-icons-icon-for-mode
                all-the-icons-icon-family
                all-the-icons-match-to-alist
                all-the-icons-faicon
                all-the-icons-octicon
                all-the-icons-dir-is-submodule)
    :preface
    (defun ivy-rich-bookmark-name (candidate)
      (car (assoc candidate bookmark-alist)))

    (defun ivy-rich-buffer-icon (candidate)
      "Display buffer icons in `ivy-rich'."
      (when (display-graphic-p)
        (let* ((buffer (get-buffer candidate))
               (buffer-file-name (buffer-file-name buffer))
               (major-mode (buffer-local-value 'major-mode buffer))
               (icon (if (and buffer-file-name
                              (all-the-icons-auto-mode-match?))
                         (all-the-icons-icon-for-file (file-name-nondirectory buffer-file-name) :v-adjust -0.05)
                       (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
          (if (symbolp icon)
              (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
            icon))))

    (defun ivy-rich-file-icon (candidate)
      "Display file icons in `ivy-rich'."
      (when (display-graphic-p)
        (let* ((path (file-local-name (concat ivy--directory candidate)))
               (file (file-name-nondirectory path))
               (icon (cond
                      ((file-directory-p path)
                       (cond
                        ((and (fboundp 'tramp-tramp-file-p)
                              (tramp-tramp-file-p default-directory))
                         (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01))
                        ((file-symlink-p path)
                         (all-the-icons-octicon "file-symlink-directory" :height 1.0 :v-adjust 0.01))
                        ((all-the-icons-dir-is-submodule path)
                         (all-the-icons-octicon "file-submodule" :height 1.0 :v-adjust 0.01))
                        ((file-exists-p (format "%s/.git" path))
                         (all-the-icons-octicon "repo" :height 1.1 :v-adjust 0.01))
                        (t (let ((matcher (all-the-icons-match-to-alist path all-the-icons-dir-icon-alist)))
                             (apply (car matcher) (list (cadr matcher) :v-adjust 0.01))))))
                      ((string-match "^/.*:$" path)
                       (all-the-icons-material "settings_remote" :height 1.0 :v-adjust -0.2))
                      ((not (string-empty-p file))
                       (all-the-icons-icon-for-file file :v-adjust -0.05)))))
          (if (symbolp icon)
              (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
            icon))))

    (defun ivy-rich-function-icon (_candidate)
      "Display function icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-purple)))

    (defun ivy-rich-variable-icon (_candidate)
      "Display variable icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "tag" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue)))

    (defun ivy-rich-symbol-icon (_candidate)
      "Display symbol icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-octicon "gear" :height 0.9 :v-adjust -0.05)))

    (defun ivy-rich-theme-icon (_candidate)
      "Display theme icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-material "palette" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue)))

    (defun ivy-rich-keybinding-icon (_candidate)
      "Display keybindings icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-material "keyboard" :height 1.0 :v-adjust -0.2)))

    (defun ivy-rich-library-icon (_candidate)
      "Display library icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-material "view_module" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue)))

    (defun ivy-rich-package-icon (_candidate)
      "Display package icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "archive" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-silver)))

    (when (display-graphic-p)
      (defun ivy-rich-bookmark-type-plus (candidate)
        (let ((filename (file-local-name (ivy-rich-bookmark-filename candidate))))
          (cond ((null filename)
                 (all-the-icons-material "block" :v-adjust -0.2 :face 'warning))  ; fixed #38
                ((file-remote-p filename)
                 (all-the-icons-material "wifi_tethering" :v-adjust -0.2 :face 'mode-line-buffer-id))
                ((not (file-exists-p filename))
                 (all-the-icons-material "block" :v-adjust -0.2 :face 'error))
                ((file-directory-p filename)
                 (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust -0.05))
                (t (all-the-icons-icon-for-file (file-name-nondirectory filename) :height 0.9 :v-adjust -0.05)))))
      (advice-add #'ivy-rich-bookmark-type :override #'ivy-rich-bookmark-type-plus))
    :hook ((ivy-mode . ivy-rich-mode)
           (ivy-rich-mode . (lambda ()
                              (setq ivy-virtual-abbreviate
                                    (or (and ivy-rich-mode 'abbreviate) 'name)))))
    :init
    ;; For better performance
    (setq ivy-rich-parse-remote-buffer nil)

    ;; Setting tab size to 1, to insert tabs as delimiters
    (add-hook 'minibuffer-setup-hook
              (lambda ()
                (setq tab-width 1)))

    (setq ivy-rich-display-transformers-list
          '(ivy-switch-buffer
            (:columns
             ((ivy-rich-buffer-icon)
              (ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand))
             :delimiter "\t")
            ivy-switch-buffer-other-window
            (:columns
             ((ivy-rich-buffer-icon)
              (ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand))
             :delimiter "\t")
            counsel-switch-buffer
            (:columns
             ((ivy-rich-buffer-icon)
              (ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand))
             :delimiter "\t")
            counsel-switch-buffer-other-window
            (:columns
             ((ivy-rich-buffer-icon)
              (ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand))
             :delimiter "\t")
            persp-switch-to-buffer
            (:columns
             ((ivy-rich-buffer-icon)
              (ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand))
             :delimiter "\t")
            counsel-M-x
            (:columns
             ((ivy-rich-function-icon)
              (counsel-M-x-transformer (:width 50))
              (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
            counsel-describe-function
            (:columns
             ((ivy-rich-function-icon)
              (counsel-describe-function-transformer (:width 50))
              (ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))
             :delimiter "\t")
            counsel-describe-variable
            (:columns
             ((ivy-rich-variable-icon)
              (counsel-describe-variable-transformer (:width 50))
              (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face)))
             :delimiter "\t")
            counsel-apropos
            (:columns
             ((ivy-rich-symbol-icon)
              (ivy-rich-candidate))
             :delimiter "\t")
            counsel-info-lookup-symbol
            (:columns
             ((ivy-rich-symbol-icon)
              (ivy-rich-candidate))
             :delimiter "\t")
            counsel-descbinds
            (:columns
             ((ivy-rich-keybinding-icon)
              (ivy-rich-candidate))
             :delimiter "\t")
            counsel-find-file
            (:columns
             ((ivy-rich-file-icon)
              (ivy-read-file-transformer))
             :delimiter "\t")
            counsel-file-jump
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate))
             :delimiter "\t")
            counsel-dired
            (:columns
             ((ivy-rich-file-icon)
              (ivy-read-file-transformer))
             :delimiter "\t")
            counsel-dired-jump
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate))
             :delimiter "\t")
            counsel-fzf
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate))
             :delimiter "\t")
            counsel-git
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate))
             :delimiter "\t")
            counsel-recentf
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate (:width 0.8))
              (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
             :delimiter "\t")
            counsel-bookmark
            (:columns
             ((ivy-rich-bookmark-type)
              (ivy-rich-bookmark-name (:width 40))
              (ivy-rich-bookmark-info))
             :delimiter "\t")
            counsel-package
            (:columns
             ((ivy-rich-package-icon)
              (ivy-rich-candidate))
             :delimiter "\t")
            counsel-find-library
            (:columns
             ((ivy-rich-library-icon)
              (ivy-rich-candidate))
             :delimiter "\t")
            counsel-load-library
            (:columns
             ((ivy-rich-library-icon)
              (ivy-rich-candidate))
             :delimiter "\t")
            counsel-load-theme
            (:columns
             ((ivy-rich-theme-icon)
              (ivy-rich-candidate))
             :delimiter "\t")
            counsel-projectile-switch-project
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate))
             :delimiter "\t")
            counsel-projectile-find-file
            (:columns
             ((ivy-rich-file-icon)
              (counsel-projectile-find-file-transformer))
             :delimiter "\t")
            counsel-projectile-find-dir
            (:columns
             ((ivy-rich-file-icon)
              (counsel-projectile-find-dir-transformer))
             :delimiter "\t")
            treemacs-projectile
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate))
             :delimiter "\t")))))

(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
