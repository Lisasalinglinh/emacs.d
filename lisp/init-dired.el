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
  (setq dired-recursive-deletes (quote top) ;;"always"means no asking
        dired-recursive-copies (quote top) ;;top means ask once
        wdired-allow-to-change-permissions t
        ;;go to dired, then Alt+x split-window-below, then go to another dired dir. Now, when you press C to copy, the other dir in the split pane will be default destination
        dired-dwim-target t)

  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file

  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

  ;; Colourful dired
  (use-package diredfl
    :init (diredfl-global-mode 1))

  ;; Shows icons
  (use-package all-the-icons-dired
    :after all-the-icons
    :diminish
    :custom-face (all-the-icons-dired-dir-face ((t `(:foreground ,(face-background 'default)))))
    :hook (dired-mode . all-the-icons-dired-mode)
    :config
    (defun my-all-the-icons-dired--display ()
      "Display the icons of files without colors in a dired buffer."
      (when (and (not all-the-icons-dired-displayed) dired-subdir-alist)
        (setq-local all-the-icons-dired-displayed t)
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (dired-move-to-filename nil)
                (unless (member (dired-get-filename 'verbatim t)
                                '("." ".."))
                  (let* ((file (dired-get-filename nil t))
                         (icon (if (file-directory-p file)
                                   (cond
                                    ((and (fboundp 'tramp-tramp-file-p)
                                          (tramp-tramp-file-p default-directory))
                                     (all-the-icons-octicon "file-directory" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face :height 0.92))
                                    ((file-symlink-p file)
                                     (all-the-icons-octicon "file-symlink-directory" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face :height 0.92))
                                    ((all-the-icons-dir-is-submodule file)
                                     (all-the-icons-octicon "file-submodule" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face :height 0.92))
                                    ((file-exists-p (format "%s/.git" file))
                                     (all-the-icons-octicon "repo" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face :height 0.92))
                                    (t (let ((matcher (all-the-icons-match-to-alist file all-the-icons-dir-icon-alist)))
                                         (apply (car matcher) (list (cadr matcher) :face 'all-the-icons-dired-dir-face :v-adjust all-the-icons-dired-v-adjust :height 0.92)))))
                                 (all-the-icons-icon-for-file file :face 'all-the-icons-dired-dir-face :v-adjust all-the-icons-dired-v-adjust :height 0.92))))
                    (insert (concat icon " ")))))
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
