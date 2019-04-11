;; init-dired.el --- Initialize dired configurations.	-*- lexical-binding: t -*-

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
;; Directory configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

;; Directory operations
(use-package dired
  :ensure nil
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes (quote always) ;;"always"means no asking
        dired-recursive-copies (quote always) ;;top means ask once
        wdired-allow-to-change-permissions t
        ;;go to dired, then Alt+x split-window-below, then go to another dired dir. Now, when you press C to copy, the other dir in the split pane will be default destination
        dired-dwim-target t)

  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file

  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

  ;; Using `insert-directory-program'
  (setq ls-lisp-use-insert-directory-program t)

  ;; Show directory first
  (setq dired-listing-switches "-alh --group-directories-first")
  ;; Colourful dired
  (use-package diredfl
    :init (diredfl-global-mode 1))
  ;; Quick sort dired buffers via hydra
  (use-package dired-quick-sort
    :bind (:map dired-mode-map
                ("S" . hydra-dired-quick-sort/body)))

  ;; Shows icons
  (use-package all-the-icons-dired
    :after all-the-icons
    :diminish
    :custom-face (all-the-icons-dired-dir-face ((t (:foreground nil))))
    :hook (dired-mode . all-the-icons-dired-mode)
    :config
    (defun my-all-the-icons-dired--display ()
      "Display the icons of files without colors in a dired buffer."
      (when (and (not all-the-icons-dired-displayed) dired-subdir-alist)
        (setq-local all-the-icons-dired-displayed t)
        (let ((inhibit-read-only t)
              (remote-p (and (fboundp 'tramp-tramp-file-p)
                             (tramp-tramp-file-p default-directory))))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (dired-move-to-filename nil)
                (let ((file (dired-get-filename 'verbatim t)))
                  (unless (member file '("." ".."))
                    (let ((filename (dired-get-filename nil t)))
                      (if (file-directory-p filename)
                          (let* ((matcher (all-the-icons-match-to-alist file all-the-icons-dir-icon-alist))
                                 (icon (cond
                                        (remote-p
                                         (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
                                        ((file-symlink-p filename)
                                         (all-the-icons-octicon "file-symlink-directory" :height 0.9 :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
                                        ((all-the-icons-dir-is-submodule filename)
                                         (all-the-icons-octicon "file-submodule" :height 0.9 :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
                                        ((file-exists-p (format "%s/.git" filename))
                                         (all-the-icons-octicon "repo" :height 0.9 :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
                                        (t (apply (car matcher) (list (cadr matcher) :height 0.9 :face 'all-the-icons-dired-dir-face :v-adjust all-the-icons-dired-v-adjust))))))
                            (insert (concat icon " ")))
                        (insert (concat (all-the-icons-icon-for-file file :height 0.9 :v-adjust all-the-icons-dired-v-adjust) " ")))))))
              (forward-line 1))))))
    (advice-add #'all-the-icons-dired--display :override #'my-all-the-icons-dired--display))

  ;; Extra Dired functionality
  (use-package dired-aux :ensure nil)
  (use-package dired-k
    :config
    (define-key dired-mode-map (kbd "K") 'dired-k))
  (use-package dired-x
    :ensure nil
    :demand
    :config
    (let ((cmd (cond
                (sys/mac-x-p "open")
                (sys/linux-x-p "xdg-open")
                (sys/win32p "start")
                (t ""))))
      (setq dired-guess-shell-alist-user
            `(("\\.pdf\\'" ,cmd)
              ("\\.docx\\'" ,cmd)
              ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
              ("\\.\\(?:xcf\\)\\'" ,cmd)
              ("\\.csv\\'" ,cmd)
              ("\\.tex\\'" ,cmd)
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
              ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
              ("\\.html?\\'" ,cmd)
              ("\\.md\\'" ,cmd))))

    (setq dired-omit-files
          (concat dired-omit-files
                  "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))))

(provide 'init-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
