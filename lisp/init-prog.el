;; init-prog.el --- Initialize programming configurations.	-*- lexical-binding: t -*-

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
;; General programming configurations.
;;

;;; Code:

;; Prettify Symbols
;; e.g. display “lambda” as “λ”
(use-package prog-mode
  :ensure nil
  :hook ((after-init . global-prettify-symbols-mode)
         (emacs-lisp-mode . (lambda () (push '("<=" . ?≤) prettify-symbols-alist)))))

;; Jump to definition via `ag'/`rg'/`grep'
;; (use-package dumb-jump
;;   :functions dumb-jump-hydra/body
;;   :bind (("M-g o" . dumb-jump-go-other-window)
;;          ("M-g j" . dumb-jump-go)
;;          ("M-g i" . dumb-jump-go-prompt)
;;          ("M-g x" . dumb-jump-go-prefer-external)
;;          ("M-g z" . dumb-jump-go-prefer-external-other-window))
;;   :hook (after-init . dumb-jump-mode)
;;   :config
;;   (setq dumb-jump-prefer-searcher 'rg)
;;   (with-eval-after-load 'ivy
;;     (setq dumb-jump-selector 'ivy))

;;   (defhydra dumb-jump-hydra (:color blue :columns 3)
;;     "Dumb Jump"
;;     ("j" dumb-jump-go "Go")
;;     ("o" dumb-jump-go-other-window "Other window")
;;     ("e" dumb-jump-go-prefer-external "Go external")
;;     ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
;;     ("i" dumb-jump-go-prompt "Prompt")
;;     ("l" dumb-jump-quick-look "Quick look")
;;     ("b" dumb-jump-back "Back")
;;     ("q" nil "quit"))
;;   (bind-key "C-M-j" #'dumb-jump-hydra/body dumb-jump-mode-map))

(use-package fish-mode
  :hook (fish-mode . (lambda ()
                       (add-hook 'before-save-hook
                                 #'fish_indent-before-save))))
(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here