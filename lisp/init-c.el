;; init-c.el --- Initialize c configurations.	-*- lexical-binding: t -*-

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
;; C/C++ configuration.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :mode (("\\.h\\'" . c++-mode)
         ("\\.cc\\'" . c++-mode)
         ("\\.cpp\\'" . c++-mode))
  :bind (:map c-mode-base-map
         ("C-c c" . compile))
  :config
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (use-package modern-cpp-font-lock
    :diminish
    :init (modern-c++-font-lock-global-mode t))

  (use-package clang-format+
    :hook (c++-mode . clang-format+-mode))
  ;;https://eklitzke.org/smarter-emacs-clang-format
  ;; (use-package clang-format
  ;;   :bind(
  ;;         :map c++-mode-map
  ;;         ;; ("C-i" . 'clang-format)
  ;;         ("C-c i" . 'clang-format-region)
  ;;         ("C-c u" . 'clang-format-buffer))
  ;;   :config
  ;;   ;; style option: "llvm" "google" "chromium" "mozilla" "webkit"
  ;;   ;; clang.llvm.org/docs/ClangFormatStyleOptions.html
  ;;   (if (not(file-exists-p ".clang-format"))
  ;;       (setq clang-format-style-option "llvm")))
  (use-package flycheck-clang-analyzer
    :ensure t
    :after flycheck
    :config (flycheck-clang-analyzer-setup))
  (use-package irony
    :defines (irony-mode-map irony-server-w32-pipe-buffer-size)
    :hook (((c-mode c++-mode objc-mode) . irony-mode)
           (irony-mode . irony-cdb-autosetup-compile-options))
    :config
    ;; Windows performance tweaks
    (when (boundp 'w32-pipe-read-delay)
      (setq w32-pipe-read-delay 0))
    ;; Set the buffer size to 64K on Windows (from the original 4K)
    (when (boundp 'w32-pipe-buffer-size)
      (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

    (with-eval-after-load 'counsel
      (bind-keys :map irony-mode-map
        ([remap completion-at-point] . counsel-irony)
        ([remap complete-symbol] . counsel-irony)))

    (use-package irony-eldoc
      :hook (irony-mode . irony-eldoc))

    (with-eval-after-load 'company
      (use-package company-irony
        :defines company-backends
        :init (cl-pushnew 'company-irony company-backends))
      (use-package company-irony-c-headers
        :init (cl-pushnew 'company-irony-c-headers company-backends)))

    (with-eval-after-load 'flycheck
      (use-package flycheck-irony
        :hook (flycheck-mode . flycheck-irony-setup))))
  (require 'eassist)
  (define-key c-mode-base-map (kbd "<f4>") 'eassist-switch-h-cpp)
  (define-key c-mode-base-map (kbd "C-c r m") 'eassist-list-methods)

  ;;------------- Cpp compile

  (setq
   compilation-always-kill t
   compilation-scroll-output 'first-error)

  (defvar get-buffer-compile-command (lambda (file) (cons file 1)))
  (make-variable-buffer-local 'get-buffer-compile-command)

  (setq-default compile-command "")

  (defun compile-dwim (&optional arg)
    "Compile Do What I Mean.
    Compile using `compile-command'.
    When `compile-command' is empty prompt for its default value.
    With prefix C-u always prompt for the default value of
    `compile-command'.
    With prefix C-u C-u prompt for buffer local compile command with
    suggestion from `get-buffer-compile-command'.  An empty input removes
    the local compile command for the current buffer."
    (interactive "P")
    (cond
     ((and arg (> (car arg) 4))
      (let ((cmd (read-from-minibuffer
                  "Buffer local compile command: "
                  (funcall get-buffer-compile-command
                           (or (file-relative-name (buffer-file-name)) ""))
                  nil nil 'compile-history)))
        (cond ((equal cmd "")
               (kill-local-variable 'compile-command)
               (kill-local-variable 'compilation-directory))
              (t
               (set (make-local-variable 'compile-command) cmd)
               (set (make-local-variable 'compilation-directory)
                    default-directory))))
      (when (not (equal compile-command ""))
        ;; `compile' changes the default value of
        ;; compilation-directory but this is a buffer local
        ;; compilation
        (let ((dirbak (default-value 'compilation-directory)))
          (compile compile-command)
          (setq-default compilation-directory dirbak))))
     ((or (and arg (<= (car arg) 4))
          (equal compile-command ""))
      (setq-default compile-command (read-from-minibuffer
                                     "Compile command: "
                                     (if (equal compile-command "")
                                         "make " compile-command)
                                     nil nil 'compile-history))
      (setq-default compilation-directory default-directory)
      (when (not (equal (default-value 'compile-command) ""))
        (compile (default-value 'compile-command))))
     (t
      (recompile)))

    ;;set compilation window size
    (setq cur (selected-window))
    (setq w (get-buffer-window "*compilation*"))
    (select-window w)
    (setq h (window-height w))
    (shrink-window (-h 10))
    (select-window cur)
    )

  (defun my-c++-mode ()
    (setq get-buffer-compile-command
          (lambda (file)
            (cons (format "g++-9 -Wall -o %s %s && ./%s"
                          (file-name-sans-extension file)
                          file
                          (file-name-sans-extension file))
                  11))))
  (add-hook 'c++-mode-hook 'my-c++-mode)
  (define-key c-mode-base-map (kbd "C-r") 'compile-dwim)

  ;;------------------ debug

  (setq
   ;; use gdb-many-windows by default
   gdb-many-windows t
   ;;Non-nil means display source file containing the main routine at startup
   gdb-show-main t)

  (defvar gud-overlay
    (let* ((ov (make-overlay (point-min) (point-min))))
      (overlay-put ov 'face 'secondary-selection)
      ov)
    "Overlay variable for GUD highlighting.")

  (defadvice gud-display-line (after my-gud-highlight act)

    "Highlight current line."
    (let* ((ov gud-overlay)
           (bf (gud-find-file true-file)))
      (with-current-buffer bf
        (move-overlay ov (line-beginning-position) (line-beginning-position 2)
                      (current-buffer)))))

  (defun gud-kill-buffer ()
    (if (derived-mode-p 'gud-mode)
        (delete-overlay gud-overlay)))
  (add-hook 'kill-buffer-hook 'gud-kill-buffer)
  (define-key c-mode-base-map (kbd "<f5>") 'gdb)

  ;; set breakpoint
  (define-key c-mode-base-map (kbd "<f9>") 'gud-break)

  ;; set tmp breakpoint
  (define-key c-mode-base-map (kbd "C-<f9>") 'gud-tbreak)

  ;; remove breakpoint
  (define-key c-mode-base-map (kbd "M-<f9>") 'gud-remove)

  ;;Execute next single line code,without stepping into function
  (define-key c-mode-base-map (kbd "<f10>") 'gud-next)

  ;;the program will run untill it hits a breakpoint,terminates or gets a signal the debugger is checking for,or reaches the line on which the cursor currently sits
  (define-key c-mode-base-map (kbd "C-<f10>") 'gud-until)

  ;;Execute next single line code,will stepping into function
  (define-key c-mode-base-map (kbd "<f11>") 'gud-step)

  ;;execute a single machine code
  (define-key c-mode-base-map (kbd "C-<f11>") 'gud-stepi)

  ;;run the progrma untill the selected stack frame returns or stops for some other reason
  (define-key c-mode-base-map (kbd "<f12>") 'gud-finish)

  ;; (use-package function-args)
  (use-package srefactor
    :hook ((c-mode c++-mode objc-mode) . semantic-mode)
    :bind(
          :map c++-mode-map
          ("M-RET" . srefactor-refactor-at-point))))


(provide 'init-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-c.el ends here
