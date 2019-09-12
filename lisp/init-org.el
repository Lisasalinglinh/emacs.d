;; init-org.el --- Initialize org configurations.	-*- lexical-binding: t -*-

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
;; Org configurations.
;;

;;; Code:
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-C.html
;;https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-gnuplot.html
;; http://gnuplot.sourceforge.net/demo/
(eval-when-compile
  (require 'init-const))

(use-package org
  :ensure t
  :commands org-try-structure-completion
  :functions hydra-org-template/body
  :custom-face  (org-ellipsis ((t (:foreground nil))))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb))
  :hook (org-indent-mode . (lambda()
                             (diminish 'org-indent-mode)
                             ;; WORKAROUND: Prevent text moving around while using brackets
                             ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                             (make-variable-buffer-local 'show-paren-mode)
                             (setq show-paren-mode nil)))
  :config
  (setq org-agenda-files '("~/org")
        org-todo-keywords '((sequence "TODO(T)" "DOING(I)" "HANGUP(H)" "|" "DONE(D)" "CANCEL(C)"))
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-startup-indented t
        org-ascii-headline-spacing (quote (1 . 1))
        org-pretty-entities t
        org-hide-emphasis-markers t)

  (setq org-plantuml-jar-path
        (expand-file-name "/home/ttt/.emacs.d/plugin/plantuml.jar"))

  ;; Enable plantuml-mode for PlantUML files
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
  (add-to-list 'org-export-backends 'md)
  (add-hook 'org-mode-hook
            (lambda ()
              (set-fill-column 120)))
  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-startup-truncated nil)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (dot . t)
                               (plantuml . t)
                               (gnuplot . t)))

  ;; ob-sh renamed to ob-shell since 26.1.
  (if emacs/>=26p
      (cl-pushnew '(shell . t) load-language-list)
    (cl-pushnew '(sh . t) load-language-list))

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-list))

  (use-package ob-rust
    :init (cl-pushnew '(rust . t) load-language-list))

  (use-package ob-ipython
    :if (executable-find "jupyter")     ; DO NOT remove
    :init (cl-pushnew '(ipython . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  ;; Rich text clipboard
  (use-package org-rich-yank
    :bind (:map org-mode-map
           ("C-M-y" . org-rich-yank)))

  ;; Table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  ;; plantuml
  (use-package plantuml-mode
    :config
    (setq plantuml-jar-path "/home/ttt/.emacs.d/plugin/plantuml.jar")
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
    (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))

  (eval-and-compile
    (defun hot-expand (str &optional mod)
      "Expand org template."
      (let (text)
        (when (region-active-p)
          (setq text (buffer-substring (region-beginning) (region-end)))
          (delete-region (region-beginning) (region-end)))
        (insert str)
        (org-try-structure-completion)
        (when mod (insert mod) (forward-line))
        (when text (insert text)))))

  (defhydra hydra-org-template (:color blue :hint nil)
    "
  _c_++     qu_o_te     _e_macs-lisp    _L_aTeX:
  _l_atex   _E_xample   _r_uby          _i_ndex:
  _a_scii   _v_erse     p_y_thon        _I_NCLUDE:
  _s_rc     _g_o        _p_erl          _H_TML:
  _h_tml    plant_u_ml  _S_HELL         _A_SCII:
  _C_       gunpol_t_   _P_erl          ce_n_ter
  ^ ^       ^ ^         ^ ^
  "
    ("s" (hot-expand "<s"))
    ("E" (hot-expand "<e"))
    ("o" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("n" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("e" (hot-expand "<s" "emacs-lisp -n -r :exports both :results value verbatim"))
    ("r" (hot-expand "<s" "ruby"))
    ("c" (hot-expand "<s" "C++ -n -r :includes <iostream> :flags -Wall :main no :exports both :results value verbatim"))
    ("C" (hot-expand "<s" "C -n -r :exports both :results value verbatim"))
    ("y" (hot-expand "<s" "python :results value verbatim"))
    ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)"))
    ("p" (hot-expand "<s" "perl"))
    ("S" (hot-expand "<s" "sh :results value verbatim"))
    ("u" (hot-expand "<s" "plantuml :file figures/CHANGE.png :exports results\n@startuml\n@enduml"))
    ("P" (progn
           (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
           (hot-expand "<s" "perl")))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("t" (hot-expand "<s" "gnuplot :var data= :file output.png :exports results"))

    ("<" self-insert-command "ins")
    ("q" nil "quit"))
  (bind-key "<"
            (lambda () (interactive)
              (if (or (region-active-p) (looking-back "^\s*" 1))
                  (hydra-org-template/body)
                (self-insert-command 1)))
            org-mode-map)
  )

;; ;; publish .org to .html
;; ;; M-x org-publish-project
(setq org-html-htmlize-output-type 'css
      org-fontify-whole-heading-line t
      org-publish-project-alist
      '(
        ;; These are the main web files
        ;; options @ https://www.gnu.org/software/emacs/manual/html_node/org/Publishing-options.html#Publishing-options
        ("org-notes"
         :base-directory "/home/ttt/OrgNotes/" ;; Change this to your local dir
         :base-extension "org"
         :publishing-directory "/home/ttt/gtcp2305.github.io/"
         :recursive t
         :org-publish-use-timestamps-flag nil ;;https://www.gnu.org/software/emacs/manual/html_node/org/Uploading-files.html#Uploading-files
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-sitemap t
         :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
         :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
         :sitemap-sort-files alphabetically ;;按照字母表顺序排列文件
         :section-numbers t
         :with-date t
         :table-of-contents t
         :html-preamble t
         :html-head-include-default-style nil ;Disable the default css style
         :html-head-include-scripts nil ;Disable the default javascript snippet
         :html-link-home "../index.html"    ; Just the default for this project.
         :html-link-up "../sitemap.html"
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://gtcp2305.github.io/css/worg.min.css\"/>")
        ;; These are static files (images, pdf, etc)
        ("org-static"
         :base-directory "/home/ttt/OrgNotes/" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|txt\\|asc"
         :publishing-directory "/home/ttt/gtcp2305.github.io/"
         :recursive t
         :publishing-function org-publish-attachment)

        ("blog" :components ("org-notes" "org-static"))))

;; ;; publish .org to slide
;; (setq org-export-with-smart-quotes t)
;; (setq org-html-mathjax-options
;;       '((path "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
;;         (scale "100")
;;         (align "center")
;;         (indent "2em")
;;         (mathml nil))
;;       org-html-mathjax-template
;;       "<script type=\"text/javascript\" src=\"%PATH\"></script>"
;;       org-html-table-default-attributes
;;       '(:border "0" :cellspacing "0" :cellpadding "6" :rules "none" :frame "none")
;;       org-confirm-babel-evaluate nil)

;; (require 'ox-reveal)
;; (setq org-reveal-root "file:///home/ttt/.emacs.d/plugin/reveal.js/"
;;       Org-Reveal-title-slide nil)

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
