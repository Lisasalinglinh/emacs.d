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
         (prog-mode . (lambda ()
                        (setq prettify-symbols-alist
                              '(("lambda" . ?λ)
                                ("->" . ?→)
                                ("->>" . ?↠)
                                ("=>" . ?⇒)
                                ("map" . ?↦)
                                ("/=" . ?≠)
                                ("!=" . ?≠)
                                ;; ("==" . ?≡)
                                ("<=" . ?≤)
                                (">=" . ?≥)
                                ("=<<" . (?= (Br . Bl) ?≪))
                                (">>=" . (?≫ (Br . Bl) ?=))
                                ("<=<" . ?↢)
                                (">=>" . ?↣)
                                ;; ("&&" . ?∧)
                                ;; ("||" . ?∨)
                                ("not" . ?¬))))))
  :init (setq prettify-symbols-unprettify-at-point 'right-edge))
;; Compilation Mode
(use-package compile
  :ensure nil
  :preface
  ;; ANSI Coloring
  ;; @see https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
  (defun my-colorize-compilation-buffer ()
    "ANSI coloring in compilation buffers."
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . my-colorize-compilation-buffer))

;; Jump to definition via `ag'/`rg'/`grep'
(use-package dumb-jump
  :functions dumb-jump-hydra/body
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :hook (after-init . dumb-jump-mode)
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  (with-eval-after-load 'ivy
    (setq dumb-jump-selector 'ivy))

  (defhydra hydra-dumb-jump (:color blue :hint none)
    "
^Jump^                            ^Other^
^^────────────────────────────────^^───────────────
_j_: Go                           _i_: Prompt
_o_: Go other window              _l_: Quick look
_e_: Go external                  _b_: Back
_x_: Go external other window
"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Go other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back")
    ("q" nil "quit"))
  (bind-key "C-M-j" #'hydra-dumb-jump/body dumb-jump-mode-map))

(use-package rmsbolt)

(use-package fish-mode
  :hook (fish-mode . (lambda ()
                       (add-hook 'before-save-hook
                                 #'fish_indent-before-save))))
(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
