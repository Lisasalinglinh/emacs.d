(eval-when-compile
  (require 'init-custom))

(use-package hydra
  :commands (hydra-default-pre
	         hydra-keyboard-quit
	         hydra--call-interactively-remap-maybe
	         hydra-show-hint
	         hydra-set-transient-map))

(use-package pretty-hydra
  :functions set-package-archives
  :bind ("<f6>" . toggles-hydra/body)
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
	                                  &key face height v-adjust)
	"Add an icon in the hydra title."
	(let ((face (or face `(:foreground ,(face-background 'highlight))))
	      (height (or height 1.0))
	      (v-adjust (or v-adjust 0.0)))
	  (concat
	   (when (and (display-graphic-p) icon-type icon-name)
	     (let ((f (intern (format "all-the-icons-%s" icon-type))))
	       (when (fboundp f)
	         (concat
	          (apply f (list icon-name :face face :height height :v-adjust v-adjust))
	          " "))))
	   (propertize title 'face face))))

  ;; Global toggles
  (pretty-hydra-define toggles-hydra (:title (pretty-hydra-title "Toggles" 'faicon "toggle-on")
	                                  :color amaranth :quit-key "q")
	("Basic"
	 (("n" display-line-numbers-mode "line number" :toggle t)
	  ("N" linum-mode "legacy line number" :toggle t)
	  ("a" aggressive-indent-mode "aggressive indent" :toggle t)
	  ("h" hungry-delete-mode "hungry delete" :toggle t)
	  ("e" electric-pair-mode "electric pair" :toggle t)
	  ("P" flyspell-mode "spell check" :toggle t)
	  ("S" prettify-symbols-mode "pretty symbol" :toggle t)
	  ("L" page-break-lines-mode "page break lines" :toggle t))
	 "Highlight"
	 (("l" global-hl-line-mode "line" :toggle t)
	  ("p" show-paren-mode "paren" :toggle t)
	  ("s" symbol-overlay-mode "symbol" :toggle t)
	  ("r" rainbow-mode "rainbow" :toggle t)
	  ("w" (setq show-trailing-whitespace (not show-trailing-whitespace))
	   "whitespace" :toggle show-trailing-whitespace)
	  ("R" rainbow-delimiters-mode "delimiter" :toggle t)
	  ("i" highlight-indent-guides-mode "indent" :toggle t)
	  ("t" hl-todo-mode "todo" :toggle t))
	 "Coding"
	 (("f" flycheck-mode "flycheck" :toggle t)
	  ("F" flymake-mode "flymake" :toggle t)
	  ("o" origami-mode "folding" :toggle t)
	  ("O" hs-minor-mode "hideshow" :toggle t)
	  ("w" subword-mode "subword" :toggle t)
	  ("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
	  ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))
	 "Version Control"
	 (("v" diff-hl-mode "gutter" :toggle t)
	  ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
	  ("m" diff-hl-margin-mode "margin gutter" :toggle t)
	  ("E" diff-hl-dired-mode "dired gutter" :toggle t)))))

(provide 'init-hydra)
