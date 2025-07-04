(defun efs/org-font-setup ()
  (font-lock-add-keywords 'org-mode
    '(("^ *\\([-]\\) "
       (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (add-hook 'org-mode-hook 'visual-line-mode)
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)))
    (set-face-attribute (car face) nil
                        :font "FiraCode Nerd Font" :weight 'regular :height (* 2 (cdr face))))
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook ((org-mode . efs/org-mode-setup)
         (org-mode . efs/org-font-setup))
  :config
  (setq org-ellipsis " ▾")
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp :tangle yes"))
  (add-to-list 'org-structure-template-alist '("py" . "src python :tangle yes :results output"))
  (add-to-list 'org-structure-template-alist '("ein" . "src ein-python :session localhost")))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))
   ;;(ein . t)))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'efs/org-babel-tangle-config nil 'make-it-local)))

(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/Projects/Code/emacs-from-scratch/Emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(custom-set-faces
 '(org-block-begin-line ((t (:box (:line-width 3 :color "purple")
                              :foreground "purple" :background "#46005f" :extend t))))
 '(org-block ((t (:background "#301934" :extend t))))
 '(org-block-end-line ((t (:box (:line-width 3 :color "purple")
                            :foreground "purple" :background "#46005f" :extend t)))))

