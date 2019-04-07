(use-package pyim
  :defer 2
  :ensure nil
  :demand t
  :config
  (setq default-input-method "pyim"
        ;; (global-set-key (kbd "C-\\") 'toggle-input-method)
        pyim-page-tooltip 'posframe
        pyim-default-scheme 'quanpin
        pyim-page-style 'vertical
        pyim-page-length 9)

  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template)

                pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  ;;pyim-greatdict
  (require 'pyim-greatdict)
  (pyim-greatdict-enable)
  (setq pyim-dicts
        '((:name "greatdict" :file (expand-file-name "~/.emacs.d/plugin/plantuml.jar"))))
  :custom-face
  (pyim-page ((t (:inherit default :background "#333333" :foreground "yellow"))))
  :bind
  ("M-j" . pyim-convert-string-at-point)
  ("M-f" . pyim-forward-word)
  ("M-b" . pyim-backward-word))

(provide 'init-pyim)
