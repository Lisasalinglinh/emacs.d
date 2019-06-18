;; init-cmake.el --- Initialize c configurations.	-*- lexical-binding: t -*-

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
;; CMake configuration.
;;

;;; Code:
;; https://github.com/dfrib/emacs_setup
;; C++

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rtags & cmake ide
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rtags
  :config
  ;; Set path to rtag executables.
  (setq rtags-path
        (expand-file-name "/usr/local/bin"))
  (setq rtags-autostart-diagnostics t)
  (setq rtags-use-multiple-cursors t)
  (rtags-diagnostics)
  ;; Timeout for reparse on onsaved buffers.
  (rtags-set-periodic-reparse-timeout 0.5)
  (defhydra rtags-hydra (:color blue :hint nil)
    "
  _._: find definition @ point      _E_: preprocess file             _S_: display summary                _a_: print source arguments
  _,_: find references @ point      _F_: fix it                      _T_: tag list                       _e_: reparse file
  _/_: find all references          _G_: guess function @ point      _U_: display summary as message     _h_: print class hierarchy
  _;_: find file                    _I_: imenu                       _V_: print enum value @ point       _l_: list results
  _<_: find reference               _K_: implement function          _X_: fix @ point                    _p_: dependency tree
  _>_: find definition              _L_: copy & print curr location  _Y_: cycle through diagnostics      _t_: references tree
  _A_: find func called by this func _M_: symbol info                _Z_: location stack visualize       _v_: find visuals @ point
  _B_: show tags buffer             _O_: goto offset                 _[_: location stack back            _m_: func list
  _C_: compile file                 _P_: dependency tree all         _]_: location stack forward
  _D_: diagnostics                  _R_: rename symbol               ___: asm file
  "
    ("." rtags-find-symbol-at-point)
    ("," rtags-find-references-at-point)
    ("/" rtags-find-all-references-at-point)
    (";" rtags-find-file)
    ("<" rtags-find-references)
    (">" rtags-find-symbol)
    ("A" rtags-find-functions-called-by-this-function)
    ("B" rtags-show-tags-buffer)
    ("C" cmake-ide-compile)
    ("D" rtags-diagnostics)
    ("E" rtags-preprocess-file)
    ("F" rtags-fixit)
    ("G" rtags-guess-function-at-point)
    ("I" rtags-imenu)
    ("K" srefactor-refactor-at-point)
    ("L" rtags-copy-and-print-current-location)
    ("M" rtags-symbol-info)
    ("O" rtags-goto-offset)
    ("P" rtags-dependency-tree-all)
    ("R" rtags-rename-symbol)
    ("S" rtags-display-summary)
    ("T" rtags-taglist)
    ("U" rtags-display-summary-as-message)
    ("V" rtags-print-enum-value-at-point)
    ("X" rtags-fix-fixit-at-point)
    ("Y" rtags-cycle-through-diagnostics)
    ("Z" rtags-location-stack-visualize)
    ("[" rtags-location-stack-back)
    ("]" rtags-location-stack-forward)
    ("_" rtags-asm-file)
    ("a" rtags-print-source-arguments)
    ("e" rtags-reparse-file)
    ("h" rtags-print-class-hierarchy)
    ("l" rtags-list-results)
    ("p" rtags-dependency-tree)
    ("t" rtags-references-tree)
    ("v" rtags-find-virtuals-at-point)
    ("m" eassist-list-methods))
  (global-set-key (kbd "<f7>") 'rtags-hydra/body)
  (rtags-enable-standard-keybindings))

;; CMake
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :init (setq cmake-tab-width 4))

;;http://parbo.github.io/blog/2016/05/10/configuring-emacs-for-cpp/
(use-package cmake-ide
  :ensure t
  :init
  (cmake-ide-setup)
  :config
  (put 'cmake-ide-build-dir 'safe-local-variable #'stringp))

;; Color mode line for errors.
(use-package flycheck-color-mode-line
  :after flycheck
  :config '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
(use-package cmake-font-lock
  :config
  (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
  (add-hook 'cmake-mode-hook 'cmake-font-lock-activate))
;;TEST-------------------------------------------------------------------------------------
(use-package ivy-rtags
  ;; :require ivy rtags
  :config
  (progn
    (setq rtags-display-result-backend 'ivy)))
(use-package company-rtags
  ;; :require company rtags
  :config
  (progn
    (setq rtags-autostart-diagnostics t)
    (rtags-diagnostics)
    (setq rtags-completions-enabled t)
    (push 'company-rtags company-backendsx)))

;; Flycheck rtags.
(use-package flycheck-rtags
  ;; :require flycheck rtags
  :config
  (defun my-flycheck-rtags-setup ()
    (flycheck-select-checker 'rtags)
    (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
    (setq-local flycheck-check-syntax-automatically nil)
    (rtags-set-periodic-reparse-timeout 2.0)) ;;run flycheck 2s after being idle
  (add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
  (add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)
  (add-hook 'objc-mode-hook #'my-flycheck-rtags-setup))

;; Flycheck-plantuml/
(use-package flycheck-plantuml
  :after flycheck
  :config (flycheck-plantuml-setup))

(provide 'init-cmake)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-cmake.el ends here
