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
  :commands swiper-isearch
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

         ;; Find counsel commands quickly
         ("<f6>" . (lambda ()
                     (interactive)
                     (counsel-M-x "^counsel ")))

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
    (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t)
    (cond ((and (memq last-command my-ivy-fly-commands)
                (equal (this-command-keys-vector) (kbd "M-p")))
           ;; repeat one time to get straight to the first history item
           (setq unread-command-events
                 (append unread-command-events
                         (listify-key-sequence (kbd "M-p")))))
          ((memq this-command '(self-insert-command
                                yank
                                ivy-yank-word
                                counsel-yank-pop))
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
            (insert (propertize future 'face 'shadow)))
          (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t)))))

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
  ;; Enhance fuzzy matching
  (use-package flx
    :config (setq ivy-re-builders-alist
                  '((swiper . ivy--regex-plus)
                    (swiper-all . ivy--regex-plus)
                    (swiper-isearch . ivy--regex-plus)
                    (counsel-ag . ivy--regex-plus)
                    (counsel-rg . ivy--regex-plus)
                    (counsel-pt . ivy--regex-plus)
                    (counsel-ack . ivy--regex-plus)
                    (counsel-grep . ivy--regex-plus)
                    (t . ivy--regex-fuzzy))))


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

  ;; Select from xref candidates with Ivy
  (use-package ivy-xref
    :ensure t
    :init (if (< emacs-major-version 27)
              (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
            (setq xref-show-definitions-function #'ivy-xref-show-defs)))
  ;; Quick launch apps
  (cond
   (sys/linux-x-p
    (bind-key "C-<f6>" #'counsel-linux-app counsel-mode-map))
   (sys/macp
    (use-package counsel-osx-app
      :bind (:map counsel-mode-map
                  ("C-<f6>" . counsel-osx-app)))))

  ;; Display world clock using Ivy
  (use-package counsel-world-clock
    :bind (:map counsel-mode-map
                ("C-c c k" . counsel-world-clock)))

  ;; Tramp ivy interface
  (use-package counsel-tramp
    :bind (:map counsel-mode-map
                ("C-c c v" . counsel-tramp))))

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
(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
