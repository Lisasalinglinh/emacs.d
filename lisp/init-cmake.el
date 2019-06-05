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
;; (use-package c++-mode
;;   :after rtags
;;   :bind (:map c++-mode-map
;;               ("<home>" . 'rtags-find-symbol-at-point)
;;               ("<prior>" . 'rtags-location-stack-back)
;;               ("<next>" . 'rtags-location-stack-forward)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rtags & cmake ide
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rtags
  :config
  ;; Set path to rtag executables.
  (setq rtags-path
        (expand-file-name "/usr/local/bin"))
  ;;
  ;; Start the rdm process unless the process is already running.
  ;; --> Launch rdm externally and prior to Emacs instead.
  ;; (rtags-start-process-unless-running)
  ;;
  ;; Enable rtags-diagnostics.
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  ;;
  ;; disable prelude's use of C-c r, as this is the rtags keyboard prefix

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
  ;; :bind (:map c++-mode-map
  ;;             ("r" . rtags-hydra/body))

  ;; Rtags standard keybindings ([M-. on symbol to go to bindings]).
  (rtags-enable-standard-keybindings)
  ;; (define-key c-mode-base-map (kbd "M-.")
  ;;   (function rtags-find-symbol-at-point))
  ;; (define-key c-mode-base-map (kbd "M-,")
  ;;   (function rtags-find-references-at-point))
  ;;
  ;; Enable completions in with rtags & company mode
  ;; -> use irony for completions
  ;;(setq rtags-completions-enabled t)
  ;;(require 'company)
  ;;(global-company-mode)
  ;;(push 'company-rtags company-backends) ; Add company-rtags to company-backends
  ;;
  )
;; CMake
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :init (setq cmake-tab-width 4))

;;http://parbo.github.io/blog/2016/05/10/configuring-emacs-for-cpp/
(use-package cmake-ide
  :after rtags
  :ensure t
  :init
  ;; (require 'semantic/bovine/gcc)
  ;; (setq cmake-ide-flags-c++ (append '("-std=c++11")
  ;;                                   (mapcar (lambda (path) (concat "-I" path)) (semantic-gcc-get-include-paths "c++"))))
  ;; (setq cmake-ide-flags-c (append (mapcar (lambda (path) (concat "-I" path)) (semantic-gcc-get-include-paths "c"))))
  ;; set path to project build directory
  ;; (setq cmake-ide-build-dir
  ;;       (expand-file-name "~/cpp/build"))
  ;; CURRENTLY: hardcode to build dir of default project
  ;; TODO: fix via .dir-locals.el
  ;;
  ;; invoke cmake-ide setup
  (cmake-ide-setup))

;; Color mode line for errors.
(use-package flycheck-color-mode-line
  :after flycheck
  :config '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
(use-package cmake-font-lock
  :config
  (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
  (add-hook 'cmake-mode-hook 'cmake-font-lock-activate))
;; Flycheck rtags.
(use-package flycheck-rtags
  :after rtags
  :config
  (defun my-flycheck-rtags-setup ()
    (flycheck-select-checker 'rtags)
    (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
    (setq-local flycheck-check-syntax-automatically nil))
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
