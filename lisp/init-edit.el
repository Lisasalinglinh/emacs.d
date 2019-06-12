;; init-edit.el --- Initialize editing configurations.	-*- lexical-binding: t -*-

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
;; Editing configurations.
;;

;;; Code:

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Miscs
(setq initial-scratch-message nil)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save
(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again
;; (setq-default kill-whole-line t)           ; Kill line including '\n'
;; ;; overwrite selected text
;; (delete-selection-mode t)
;; ;; overwrite selected text
;; (delete-selection-mode t)

(setq-default major-mode 'text-mode)

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
          tab-width        4
          indent-tabs-mode nil)

;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Pass a URL to a WWW browser
(use-package browse-url
  :ensure nil
  :defines dired-mode-map
  :bind (("C-c C-z ." . browse-url-at-point)
     ("C-c C-z b" . browse-url-of-buffer)
     ("C-c C-z r" . browse-url-of-region)
     ("C-c C-z u" . browse-url)
     ("C-c C-z v" . browse-url-of-file))
  :init
  (with-eval-after-load 'dired
    (bind-key "C-c C-z f" #'browse-url-of-file dired-mode-map)))

;; Click to browse URL or to send to e-mail address
(use-package goto-addr
  :ensure nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

;; Quickly follow links
(use-package ace-link
  :bind (("M-o" . ace-link-addr))
  :hook (after-init . ace-link-setup-default))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish
  :hook ((after-init . global-aggressive-indent-mode)
         ;; FIXME: Disable in big files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(asm-mode web-mode html-mode css-mode robot-mode go-mode))
    (push mode aggressive-indent-excluded-modes))
  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)
  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (or (derived-mode-p 'c-mode)
             (derived-mode-p 'c++-mode)
             (derived-mode-p 'csharp-mode)
             (derived-mode-p 'java-mode)
             (derived-mode-p 'go-mode)
             (derived-mode-p 'swift-mode))
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

;; Show number of matches in mode-line while searching
(use-package anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
     ([remap query-replace-regexp] . anzu-query-replace-regexp)
     :map isearch-mode-map
     ([remap isearch-query-replace] . anzu-isearch-query-replace)
     ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2)) ;

;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :diminish
  :commands drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
    ;; (ediff-prepare-buffer . outline-show-all)
    ;; restore window layout when done
    (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package multiple-cursors
  :functions hydra-multiple-cursors
  :bind
  ("M-u" . hydra-multiple-cursors/body)
  :preface
  ;; insert specific serial number
  (defvar ladicle/mc/insert-numbers-hist nil)
  (defvar ladicle/mc/insert-numbers-inc 1)
  (defvar ladicle/mc/insert-numbers-pad "%01d")

  (defun ladicle/mc/insert-numbers (start inc pad)
    "Insert increasing numbers for each cursor specifically."
    (interactive
     (list (read-number "Start from: " 0)
           (read-number "Increment by: " 1)
           (read-string "Padding (%01d): " nil ladicle/mc/insert-numbers-hist "%01d")))
    (setq mc--insert-numbers-number start)
    (setq ladicle/mc/insert-numbers-inc inc)
    (setq ladicle/mc/insert-numbers-pad pad)
    (mc/for-each-cursor-ordered
     (mc/execute-command-for-fake-cursor
      'ladicle/mc--insert-number-and-increase
      cursor)))

  (defun ladicle/mc--insert-number-and-increase ()
    (interactive)
    (insert (format ladicle/mc/insert-numbers-pad mc--insert-numbers-number))
    (setq mc--insert-numbers-number (+ mc--insert-numbers-number ladicle/mc/insert-numbers-inc)))

  :config
  (with-eval-after-load 'hydra
    (defhydra hydra-multiple-cursors (:color pink :hint nil)
      "
                                                                        ╔════════╗
    Point^^^^^^             Misc^^            Insert                            ║ Cursor ║
  ──────────────────────────────────────────────────────────────────────╨────────╜
     _k_    _K_    _M-k_    [_l_] edit lines  [_i_] 0...
     ^↑^    ^↑^     ^↑^     [_m_] mark all    [_a_] letters
    mark^^ skip^^^ un-mk^   [_s_] sort        [_n_] numbers
     ^↓^    ^↓^     ^↓^
     _j_    _J_    _M-j_
  ╭──────────────────────────────────────────────────────────────────────────────╯
                           [_q_]: quit, [Click]: point
"
      ("l" mc/edit-lines :exit t)
      ("m" mc/mark-all-like-this :exit t)
      ("j" mc/mark-next-like-this)
      ("J" mc/skip-to-next-like-this)
      ("M-j" mc/unmark-next-like-this)
      ("k" mc/mark-previous-like-this)
      ("K" mc/skip-to-previous-like-this)
      ("M-k" mc/unmark-previous-like-this)
      ("s" mc/mark-all-in-region-regexp :exit t)
      ("i" mc/insert-numbers :exit t)
      ("a" mc/insert-letters :exit t)
      ("n" ladicle/mc/insert-numbers :exit t)
      ("<mouse-1>" mc/add-cursor-on-click)
      ;; Help with click recognition in this hydra
      ("<down-mouse-1>" ignore)
      ("<drag-mouse-1>" ignore)
      ("q" nil))))

(use-package avy
  :functions (hydra-avy hydra-viewer)
  :bind
  ("C-'"   . avy-resume)
  ("C-:"   . avy-goto-char-2-below)
  ("C-;"   . avy-goto-char)
  ("M-j"   . hydra-avy/body)
  ("C-M-v" . hydra-viewer/body)
  :preface
  ;; fixed cursor scroll-up
  (defun scroll-up-in-place (n)
    (interactive "p")
    (forward-line (- n))
    (scroll-down n))
  ;; fixed cursor scroll-down
  (defun scroll-down-in-place (n)
    (interactive "p")
    (forward-line n)
    (scroll-up n))
  ;; yank inner sexp
  (defun yank-inner-sexp ()
    (interactive)
    (backward-list)
    (mark-sexp)
    (copy-region-as-kill (region-beginning) (region-end)))
  :config
  (when (eq system-type 'darwin)
    (progn
      (global-set-key (kbd "C-:") 'avy-goto-char)
      (global-set-key (kbd "C-;") 'avy-goto-char-2-below)))

  (use-package avy-zap
    :bind
    ("M-z" . avy-zap-to-char-dwim)
    ("M-z" . avy-zap-up-to-char-dwim))

  (with-eval-after-load 'hydra
    (defhydra hydra-viewer (:color pink :hint nil)
      "
                                                                        ╔════════╗
   Char/Line^^^^^^  Word/Page^^^^^^^^  Line/Buff^^^^   Paren                              ║ Window ║
  ──────────────────────────────────────────────────────────────────────╨────────╜
       ^^_k_^^          ^^_u_^^          ^^_g_^^       _(_ ← _y_ → _)_
       ^^^↑^^^          ^^^↑^^^          ^^^↑^^^       _,_ ← _/_ → _._
   _h_ ← _d_ → _l_  _H_ ← _D_ → _L_  _a_ ← _K_ → _e_
       ^^^↓^^^          ^^^↓^^^          ^^^↓^
       ^^_j_^^          ^^_n_^^          ^^_G_
  ╭──────────────────────────────────────────────────────────────────────────────╯
                           [_q_]: quit, [_<SPC>_]: center
          "
      ("j" scroll-down-in-place)
      ("k" scroll-up-in-place)
      ("l" forward-char)
      ("d" delete-char)
      ("h" backward-char)
      ("L" forward-word)
      ("H" backward-word)
      ("u" scroll-up-command)
      ("n" scroll-down-command)
      ("D" delete-word-at-point)
      ("a" mwim-beginning-of-code-or-line)
      ("e" mwim-end-of-code-or-line)
      ("g" beginning-of-buffer)
      ("G" end-of-buffer)
      ("K" kill-whole-line)
      ("(" backward-list)
      (")" forward-list)
      ("y" yank-inner-sexp)
      ("." backward-forward-next-location)
      ("," backward-forward-previous-location)
      ("/" avy-goto-char :exit t)
      ("<SPC>" recenter-top-bottom)
      ("q" nil))

    (defhydra hydra-avy (:color pink :hint nil)
      "
                                                                        ╔════════╗
        ^^Goto^^        Kill^^        Yank^^        Move^^        Misc            ║  Jump  ║
  ──────────────────────────────────────────────────────────────────────╨────────╜
    _c_ ← char^^        [_k_] region  [_y_] region  [_m_] region  [_n_] line number
    _a_ ← char2 → _b_   [_K_] line    [_Y_] line    [_M_] line    [_v_] Goto viewer
    _w_ ← word  → _W_   [_z_] zap^^^^                             [_o_] Goto clock
    _l_ ← line  → _e_   ^^^^^                                     _,_ ← f!y → _._
  ╭──────────────────────────────────────────────────────────────────────────────╯
                      [_q_]: quit, [_i_]: imenu, [_<SPC>_]: resume
"
      ("c" avy-goto-char :exit t)
      ("a" avy-goto-char-2 :exit t)
      ("b" avy-goto-char-below :exit t)
      ("w" avy-goto-word-1 :exit t)
      ("W" avy-goto-word-1-below :exit t)
      ("l" avy-goto-line :exit t)
      ("e" avy-goto-end-of-line :exit t)
      ("M" avy-move-line)
      ("m" avy-move-region)
      ("K" avy-kill-whole-line)
      ("k" avy-kill-region)
      ("Y" avy-copy-line :exit t)
      ("y" avy-copy-region :exit t)
      ("n" goto-line :exit t)
      ("o" org-clock-jump-to-current-clock :exit t)
      ("z" avy-zap-to-char-dwim :exit t)
      ("v" hydra-viewer/body :exit t)
      ("<SPC>" avy-resume :exit t)
      ("o" org-clock-jump-to-current-clock :exit t)
      ("i" counsel-imenu :exit t)
      ("," flymake-goto-previous-error)
      ("." flymake-goto-next-error)
      ("q" nil))))

;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :defines desktop-minor-mode-table
  :bind (("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)
         :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
         :map esc-map ("C-;" . iedit-execute-last-modification)
         :map help-map ("C-;" . iedit-mode-toggle-on-function))
  :config
  ;; Avoid restoring `iedit-mode'
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-minor-mode-table
                 '(iedit-mode nil))))

;; Increase selected region by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region))


;; Smartly select region, rectangle, multi cursors
(use-package smart-region
  :hook (after-init . smart-region-on))

;; On-the-fly spell checker
(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "aspell")
  :hook (((text-mode outline-mode) . flyspell-mode)
         ;; (prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-," "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :init
  (setq flyspell-issue-message-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; Make bindings that stick around
(use-package hydra)

;; Framework for mode-specific buffer indexes
(use-package imenu
  :ensure nil
  :bind (("C-." . imenu)))

;; Move to the beginning/end of line or code
(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

;; Treat undo history as a tree
(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init (setq undo-tree-visualizer-timestamps t
              undo-tree-visualizer-diff t
              undo-tree-enable-undo-in-region nil
              undo-tree-auto-save-history nil
              undo-tree-history-directory-alist
              `(("." . ,(concat user-emacs-directory "undo-tree-hist/"))))
  :config
  ;; FIXME:  keep the diff window
  (make-variable-buffer-local 'undo-tree-visualizer-diff))
;; Goto last change
(use-package goto-chg
  :bind ("C-," . goto-last-change))

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))
;; ;; Open files as another user
;; (unless sys/win32p
;;   (use-package sudo-edit))


(defun bjm/kill-this-buffer()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'bjm/kill-this-buffer)

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Killed line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
