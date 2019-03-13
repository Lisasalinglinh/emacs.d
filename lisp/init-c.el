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
  :bind (:map c-mode-base-map
              ("C-c c" . compile))
  :config
  (use-package modern-cpp-font-lock
    :diminish
    :init (modern-c++-font-lock-global-mode t))

  ;;https://eklitzke.org/smarter-emacs-clang-format
  (use-package clang-format
    :bind(
          :map c++-mode-map
          ("C-i" . 'clang-format)
          ("C-c i" . 'clang-format-region)
          ("C-c u" . 'clang-format-buffer))
    :config
    ;; style option: "llvm" "google" "chromium" "mozilla" "webkit"
    ;; clang.llvm.org/docs/ClangFormatStyleOptions.html
    (setq clang-format-style-option "google")
    )

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

  ;; (use-package function-args)
  (use-package srefactor
    :hook ((c-mode c++-mode objc-mode) . semantic-mode)
    :bind(
          :map c++-mode-map
          ("M-RET" . srefactor-refactor-at-point))))


(provide 'init-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-c.el ends here
