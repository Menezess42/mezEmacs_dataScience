;; Setting custom file location
(setq custom-file (concat user-emacs-directory "./configs/custom_2.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; GUI basic config
(setq custom-config "./configs/custom_2.el")
(setq inhibit-startup-message t)
(setq visual-line-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(setq visible-bell 1)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(electric-pair-mode t)
(setq-default indent-tabs-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)

;; Font config
(set-face-attribute 'default nil :font "FiraCode nerdfont" :height 150)
(set-face-attribute 'fixed-pitch nil :font "FiraCode nerdfont" :height 150)
(set-face-attribute 'variable-pitch nil :font "FiraCode nerdfont" :height 150)

;; theme config
(load-file "~/.emacs.d/configs/dwarfSoftware_theme.el")

;; Initializaiton pkgs source
    (require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))


;; Auto load and install packages with use-package setup
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))

;; (package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))


;; General keys config
(dolist (package '(use-package))
   (unless (package-installed-p package)
       (package-install package)))

    (package-initialize)
    (unless package-archive-contents
    (package-refresh-contents))

;; Evil mode config
(use-package evil
	     :ensure t
    :demand t
    :bind (("<escape>" . keyboard-escape-quit))
    :init
    (setq evil-want-keybinding nil)
    :config
    (evil-mode 1))

(use-package evil-collection
	     :ensure t
:after evil
:config
(setq evil-want-integration t)
(evil-collection-init))

;; Company mode
(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-selection-wrap-around t)
  (setq company-idle-delay 0.60)
  (setq company-minimum-prefix-length 1)
  (setq company-require-match nil)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (setq company-tooltip-limit 10)
  (setq company-tooltip-minimum-width 50)
  (setq company-tooltip-minimum-height 10)
  (setq company-dabbrev-downcase nil))
; Habilitar o company-mode globalmente
(add-hook 'after-init-hook 'global-company-mode)

(defun my-company-tab-or-complete ()
  (interactive)
  (if (eq last-command 'company-complete-selection)
      (company-complete-selection)
    (if (eq company-common (car company-candidates))
        (company-complete-selection)
      (company-select-next))))

(defun my-company-backtab-or-complete ()
  (interactive)
  (if (eq last-command 'company-complete-selection)
      (company-complete-selection)
    (if (eq company-common (car (last company-candidates)))
        (company-complete-selection)
      (company-select-previous))))

(define-key company-active-map (kbd "TAB") 'my-company-tab-or-complete)
(define-key company-active-map (kbd "<tab>") 'my-company-tab-or-complete)
(define-key company-active-map (kbd "S-TAB") 'my-company-backtab-or-complete)
(define-key company-active-map (kbd "<backtab>") 'my-company-backtab-or-complete)

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (company-quickhelp-mode 1))

;; DASHBOARD
(load-file "~/.emacs.d/configs/dashboard_config.el")


;; Config for q! don't kill emacs
(defun kill-current-buffer ()

  "kill the current buffer."
  (interactive)
  (kill-buffer (buffer-name)))
(global-set-key [remap evil-quit] 'kill-current-buffer)

(defun my-quit-emacs ()
  "Quit Emacs, or close current frame if there are multiple frames."
  (interactive)
  (if (eq window-system 'x)
      (if (cdr (frame-list))
          (delete-frame)
        (message "Can't quit Emacs with only one graphical frame"))
    (kill-emacs)))

(global-set-key [remap evil-quit] 'kill-current-buffer)
(global-set-key [remap evil-save-and-quit] 'my-quit-emacs)

;; With key config
(use-package which-key
:ensure t
  :commands (which-key-mode)
  :hook ((after-init . which-key-mode)
	 (pre-command . which-key-mode))
  :config
  (setq which-key-idle-delay 1)
  :diminish which-key-mode)


;; Config ivy
(use-package ivy
:ensure t
    :diminish
    :bind (:map ivy-minibuffer-map
		("TAB" . ivy-alt-done)
		("C-l" . ivy-alt-done)
		("C-j" . ivy-next-line)
		("C-k" . ivy-previous-line)
		:map ivy-switch-buffer-map
		("C-k" . ivy-previous-line)
		("C-l" . ivy-done)
		("C-d" . ivy-switch-buffer-kill)
		:map ivy-reverse-i-search-map
		("C-k" . ivy-previous-line)
		("C-d" . ivy-reverse-i-search-kill))
    :config
    (ivy-mode 1))

(require 'ivy-rich)
(ivy-rich-mode 1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

;; Config counsel
(use-package counsel
:ensure t
  :bind (("C-x C-b" . 'counsel-switch-buffer)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

;; More config for ivy after config and install counsel
; (use-package ivy-prescient
; :ensure t
;   :after counsel
;   :custom
;   (ivy-prescient-enable-filtering nil)
;   :config
;   (ivy-prescient-mode 1))

(require 'ivy-posframe)

(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))

;; Ajustando as cores do ivy-posframe
(set-face-attribute 'ivy-posframe nil :foreground "#e5dccb" :background "#003e4d") ;; Fundo azul escuro e texto claro

;; Escondendo o minibuffer
(setq ivy-posframe-hide-minibuffer t)

;; Ativando o ivy-posframe
(ivy-posframe-mode 1)


;; helpful config
(use-package helpful
	     :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-descbinds-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; autosave
(use-package no-littering
:ensure t)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))


;;;;; center window and line ;;;;;;
(use-package centered-window
  :ensure t
  :config
  (centered-window-mode 0))

(use-package centered-cursor-mode
:ensure t
  :demand
  :config
  (global-centered-cursor-mode))


;;;;; Raimbow delimiters  ;;;;;;;;
(use-package rainbow-mode
:ensure t
  :hook ((prog-mode . rainbow-mode)
	 (after-init . rainbow-mode))
  :config
  (setq rainbow-identfiers-choose-face-function 'rainbow-identifers-cie-l*a*b*-choose-face
	rainbow-identifiers-cie-l*a*b*-lightness 45
	rainbow-identifiers-cie-l*a*b*-saturation 70
	rainbow-identifiers-rgb-face t))

(use-package rainbow-delimiters
:ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)
	 (org-mode . rainbow-delimiters-mode))
  :custom-face
  (rainbow-delimiters-depth-1-face ((t (:foreground "#92c48f"))))
  (rainbow-delimiters-depth-2-face ((t (:foreground "#f0a000"))))
  (rainbow-delimiters-depth-3-face ((t (:foreground "#ffdf00"))))
  (rainbow-delimiters-depth-4-face ((t (:foreground "#40ff00"))))
  (rainbow-delimiters-depth-5-face ((t (:foreground "#00f0a0"))))
  (rainbow-delimiters-depth-6-face ((t (:foreground "#0080ff"))))
  (rainbow-delimiters-depth-7-face ((t (:foreground "#bf00ff"))))
  (rainbow-delimiters-depth-8-face ((t (:foreground "#ff00bf"))))
  (rainbow-delimiters-depth-9-face ((t (:foreground "#ff0080")))))

;;;;; org mode ;;;;;

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
                        :font "FiraCode Nerd Font" :weight 'regular :height (* 1 (cdr face))))
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
  (add-to-list 'org-structure-template-alist '("ein" . "src ein-python :session localhost"))
(add-to-list 'org-structure-template-alist
             '("jp" . "src jupyter-python :session notebook :async t")))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (jupyter . t)))  ;; ob-jupyter funcionando

;;;;;; Execução de blocos Babel Python + Jupyter ;;;;;

;; Configurações específicas para blocos Python padrão
(setq org-babel-python-command "python3")

(setq org-babel-default-header-args:python
      '((:results . "output")
        (:session . "py")))

;; Configurações específicas para blocos Jupyter Python
(setq org-babel-default-header-args:jupyter-python
      '((:kernel . "org-venv")  ;; esse nome deve bater com o que foi definido
        (:session . "notebook")
        (:async . "yes")
        (:exports . "both")
        (:results . "value")))

;; Garante que o pacote `ob-jupyter` está disponível após instalação
(use-package jupyter
  :ensure t
  :defer t
  :config
  (require 'jupyter-export)) ;; ativa o suporte a ipynb
(with-eval-after-load 'jupyter
  (require 'jupyter-export))  ;; esse módulo precisa estar presente na instalação


(custom-set-faces
 '(org-block-begin-line ((t (:box (:line-width 1 :color "#5e81ac")  ;; azul acinzentado
                              :foreground "#81a1c1" :background "#2e3c48" :extend t))))
 '(org-block ((t (:background "#22303c" :extend t)))) ;; mesmo do fundo, mas sem sumir
 '(org-block-end-line ((t (:box (:line-width 1 :color "#5e81ac")
                            :foreground "#81a1c1" :background "#2e3c48" :extend t)))))
;
; (custom-set-faces
;  '(org-block-begin-line ((t (:box (:line-width 3 :color "purple")
;                               :foreground "purple" :background "#46005f" :extend t))))
;  '(org-block ((t (:background "#301934" :extend t))))
;  '(org-block-end-line ((t (:box (:line-width 3 :color "purple")
;                             :foreground "purple" :background "#46005f" :extend t)))))

;;;;; Doom modeline ;;;;
(use-package all-the-icons 
	     :ensure t)
(use-package nerd-icons
	     :ensure t)
(use-package doom-modeline
	     :ensure t
:init (doom-modeline-mode 1)
:custom ((doom-modeline-height 15)))

;;;;; Python Config ;;;;
(defun my-python-config ()
  (setq pythonConfig-file "~/.emacs.d/configs/python_config.el")
  (load pythonConfig-file))
(add-hook 'python-mode-hook #'my-python-config)


;;;; Disable linenumber for some parts;;;;;
(defun my-disable-line-numbers ()
  "Disable line numbers."
  (display-line-numbers-mode 0))
(add-hook 'python-mode-hook #'my-python-config)
(add-hook 'org-mode-hook 'my-disable-line-numbers)
(add-hook 'term-mode-hook 'my-disable-line-numbers)
(add-hook 'shell-mode-hook 'my-disable-line-numbers)
(add-hook 'eshell-mode-hook 'my-disable-line-numbers)
(add-hook 'dashboard-mode-hook 'my-disable-line-numbers)
(add-hook 'vterm-mode-hook 'my-disable-line-numbers)

;;;; Config for direnv ;;;;;
(use-package direnv
  :ensure t
  :config
  (direnv-mode)
  ;; Ativar direnv automaticamente ao entrar em diretórios com .envrc
  (defun my/direnv-allow()
    (when (locate-dominating-file default-directory ".envrc")
      (direnv-update-directory-environment)))
  ;; Adiciona o hook para ativar apenas quando necessário
  (add-hook 'find-file-hook 'my/direnv-allow)
  (add-hook 'dired-mode-hook 'my/direnv-allow))


;;;;; Config for nix language ;;;;;
;; Carregar nix-mode apenas quando arquivos .nix são abertos
(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'")
  :defer t)

;; Carregar nix-drv-mode apenas quando arquivos .drv são abertos
(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'"
  :defer t)

;; Carregar comandos nix-shell apenas quando chamados
(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build)
  :defer t)

;; Carregar nix-repl apenas quando o comando é chamado
(use-package nix-repl
  :ensure nix-mode
  :commands (nix-repl)
  :defer t)

;; Configuração do atalho para ivy-nixos-options
(global-set-key (kbd "C-c C-S-n") 'ivy-nixos-options)

(setq ivy-nixos-options-default 2)

;; Adicionar backend do company para NixOS, mas apenas ao entrar no nix-mode
(use-package company
  :config
  (add-hook 'nix-mode-hook
            (lambda ()
              (add-to-list 'company-backends 'company-nixos-options))))



;;; Config para fazer o org src block funcionar como IDE ;;;;
; (define-key org-mode-map (kbd "C-c c") #'org-edit-special)
(defun mez/org-src-exit-or-edit ()
  "Se estiver em um buffer de edição de código fonte do Org, salva e sai. Senão, entra no bloco."
  (interactive)
  (if (string-prefix-p "*Org Src" (buffer-name))
      (progn
        (org-edit-src-save)
        (org-edit-src-exit))
    (org-edit-special)))
(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "C-c c") #'mez/org-src-exit-or-edit)))

(add-hook 'org-src-mode-hook
          (lambda () (local-set-key (kbd "C-c c") #'mez/org-src-exit-or-edit)))
; (setq org-src-window-setup 'split-window-right)  ;; Abre à direita

; (setq org-src-fontify-natively t)
; (setq org-src-tab-acts-natively t)
; (setq org-hide-block-startup nil)

(add-to-list 'load-path "~/.emacs.d/site-lisp/ox-ipynb")
(require 'ox-ipynb)


(defun mez/export-org-to-ipynb ()
  "Exporta o arquivo org atual para Jupyter Notebook (.ipynb) via Pandoc."
  (interactive)
  (when (and buffer-file-name
             (string-match-p "\\.org\\'" buffer-file-name))
    (let* ((org-file buffer-file-name)
           (base (file-name-sans-extension org-file))
           (md-file (concat base ".md"))
           (ipynb-file (concat base ".ipynb"))
           (cmd1 (format "pandoc -s %s -t markdown -o %s" org-file md-file))
           (cmd2 (format "pandoc -s %s -o %s" md-file ipynb-file)))
      (message "Exportando para Markdown...")
      (shell-command cmd1)
      (message "Exportando para Jupyter Notebook...")
      (shell-command cmd2)
      (message "✅ Exportado para %s" ipynb-file))))


(defun mez/convert-ipynb-to-org (file)
  "Converte um arquivo .ipynb para .org usando Pandoc."
  (interactive "fArquivo .ipynb: ")
  (let* ((base (file-name-sans-extension file))
         (md-file (concat base ".md"))
         (org-file (concat base ".org"))
         (cmd1 (format "pandoc -s %s -t markdown -o %s" file md-file))
         (cmd2 (format "pandoc -s %s -t org -o %s" md-file org-file)))
    (message "Convertendo %s para Markdown..." file)
    (shell-command cmd1)
    (message "Convertendo para Org...")
    (shell-command cmd2)
    (message "✅ Exportado para %s" org-file)))


;; Substituir completamente o comportamento de S-TAB no Org-mode
(defun mez/org-override-shifttab ()
  "Redefinir S-TAB no Org-mode para promover itens (desindentar) em vez de fazer folding."
  (when (eq major-mode 'org-mode)
    ;; Override na keymap principal
    (define-key org-mode-map (kbd "<S-tab>") #'org-shiftleft)
    ;; Override no mapa especial de keys com modificadores
    (define-key org-mode-map (kbd "S-<iso-lefttab>") #'org-shiftleft)

    ;; Se quiser manter o folding original, coloque em C-c <tab>
    (define-key org-mode-map (kbd "C-c <tab>") #'org-shifttab)))

(add-hook 'org-mode-hook #'mez/org-override-shifttab)
