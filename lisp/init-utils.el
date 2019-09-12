;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-

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
;; Some usefule Utilities.
;;

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Display available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :hook (after-init . which-key-mode)
  :config
  (use-package which-key-posframe
    :hook (which-key-mode . which-key-posframe-mode)
    :config
    (setq which-key-posframe-poshandler 'posframe-poshandler-point-top-left-corner)
    :custom-face
    (which-key-posframe ((t (:inherit default :background "dark slate gray"))))
    (which-key-posframe-border ((t (:inherit default :background "slate blue"))))))

;; Youdao Dictionay
(use-package youdao-dictionary
  :functions (posframe-show posframe-hide)
  :bind (("C-c y" . my-youdao-search-at-point)
         ("C-c Y" . youdao-dictionary-search-at-point))
  :config
  ;; Cache documents
  (setq url-automatic-caching t)

  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t)

  (with-eval-after-load 'posframe
    (defun youdao-dictionary-search-at-point-posframe ()
      "Search word at point and display result with posframe."
      (interactive)
      (let ((word (youdao-dictionary--region-or-word)))
        (if word
            (progn
              (with-current-buffer (get-buffer-create youdao-dictionary-buffer-name)
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (youdao-dictionary-mode)
                  (insert (youdao-dictionary--format-result word))
                  (goto-char (point-min))
                  (set (make-local-variable 'youdao-dictionary-current-buffer-word) word)))
              (posframe-show youdao-dictionary-buffer-name :position (point))
              (unwind-protect
                  (push (read-event) unread-command-events)
                (posframe-hide youdao-dictionary-buffer-name)))
          (message "Nothing to look up")))))

  (defun my-youdao-search-at-point ()
    (interactive)
    (if (display-graphic-p)
        (if (fboundp 'youdao-dictionary-search-at-point-posframe)
            (youdao-dictionary-search-at-point-posframe)
          (youdao-dictionary-search-at-point-tooltip))
      (youdao-dictionary-search-at-point))))

;; Search tools: `wgrep', `ag' and `rg'
(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

(use-package ag
  :defines projectile-command-map
  :init
  (with-eval-after-load 'projectile
    (bind-key "s S" #'ag-project projectile-command-map))
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
  (setq ag-reuse-window t)
  (use-package wgrep-ag))

;; `ripgrep'
(when (executable-find "rg")
  (use-package rg
    :defines projectile-command-map
    :hook (after-init . rg-enable-default-bindings)
    :config
    (setq rg-group-result t)
    (setq rg-show-columns t)

    (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

    (with-eval-after-load 'projectile
      (defalias 'projectile-ripgrep 'rg-project)
      (bind-key "s R" #'rg-project projectile-command-map))

    (with-eval-after-load 'counsel
      (bind-keys :map rg-global-map
                 ("c r" . counsel-rg)
                 ("c s" . counsel-ag)
                 ("c p" . counsel-pt)
                 ("c f" . counsel-fzf)))))


;; Persistent the scratch buffer
(use-package persistent-scratch
  :preface
  (defun my-save-buffer ()
    "Save scratch and other buffer."
    (interactive)
    (let ((scratch-name "*scratch*"))
      (if (string-equal (buffer-name) scratch-name)
          (progn
            (message "Saving %s..." scratch-name)
            (persistent-scratch-save)
            (message "Wrote %s" scratch-name))
        (save-buffer))))
  :hook (after-init . persistent-scratch-setup-default)
  :bind (:map lisp-interaction-mode-map
         ("C-x C-s" . my-save-buffer)))


;; Epub reader
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :preface
  (defun my-nov-setup ()
    (visual-line-mode 1)
    (face-remap-add-relative 'variable-pitch :family "Times New Roman" :height 1.5)
    (if (fboundp 'olivetti-mode) (olivetti-mode 1)))
  :hook (nov-mode . my-nov-setup))

;; Misc
(use-package copyit)                    ; copy path, url, etc.
(use-package diffview)                  ; side-by-side diff view
(use-package htmlize)                   ; covert to html
(use-package tldr)
(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
