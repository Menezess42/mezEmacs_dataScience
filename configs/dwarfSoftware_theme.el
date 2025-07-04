(custom-set-faces
 ;; Fundo e texto principal
 '(default ((t (:background "#22303c" :foreground "#d8dee9"))))

 ;; Comentários
 '(font-lock-comment-face ((t (:foreground "#56636f"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#56636f"))))

 ;; Strings
 '(font-lock-string-face ((t (:foreground "#a3be8c"))))

 ;; Nome de funções
 '(font-lock-function-name-face ((t (:foreground "#d08770"))))

 ;; Variáveis
 '(font-lock-variable-name-face ((t (:foreground "#81a1c1"))))

 ;; Palavras-chave
 '(font-lock-keyword-face ((t (:foreground "#bf616a"))))

 ;; Tipos
 '(font-lock-type-face ((t (:foreground "#ebcb8b"))))

 ;; Constantes e built-in
 '(font-lock-constant-face ((t (:foreground "#88c0d0"))))
 '(font-lock-builtin-face ((t (:foreground "#88c0d0"))))

 ;; Flycheck
 `(flycheck-warning ((t (:underline (:style wave :color "#ebcb8b"))))) ;; amarelo desaturado
 `(flycheck-error ((t (:underline (:style wave :color "#bf616a")))))    ;; vermelho suave

 ;; Número das linhas
 `(line-number ((t (:foreground "#2e3c48"))))
 `(line-number-current-line ((t (:foreground "#d8dee9" :background "#2e3c48"))))

 ;; Linha atual destacada
 '(hl-line ((t (:background "#2e3c48"))))
 '(hl-line ((t (:box (:line-width 3 :color "#b48ead"))))) ;; roxo suave

 ;; Cursor
 '(cursor ((t (:background "#e5e9f0")))) ;; branco suave

 ;; Mode-line ativo e inativo
 '(mode-line ((t (:background "#2e3c48"))))
 '(mode-line-inactive ((t (:background "#2e3c48"))))
 '(mode-line ((t (:box (:line-width 3 :color "#b48ead")))))
 '(mode-line-inactive ((t (:box (:line-width 3 :color "#b48ead")))))

 ;; Borda e divisórias da janela
 '(vertical-border ((t (:foreground "#5e81ac")))) ;; azul escuro
 '(window-divider ((t (:foreground "#5e81ac" :background "#5e81ac"))))
 '(window-divider-first-pixel ((t (:foreground "#5e81ac" :background "#5e81ac"))))
 '(window-divider-last-pixel ((t (:foreground "#5e81ac" :background "#5e81ac"))))
)

;; Cursor piscando em diferentes cores da paleta
(defvar blink-cursor-colors (list "#a3be8c" "#81a1c1" "#bf616a" "#ebcb8b")
  "No piscar, o cursor irá alternar entre as cores dessa lista.")

(setq blink-cursor-count 0)
(defun blink-cursor-timer-function ()
  "Altera a cor do cursor em cada piscar, de acordo com `blink-cursor-colors`."
  (when (not (internal-show-cursor-p))
    (when (>= blink-cursor-count (length blink-cursor-colors))
      (setq blink-cursor-count 0))
    (set-cursor-color (nth blink-cursor-count blink-cursor-colors))
    (setq blink-cursor-count (+ 1 blink-cursor-count)))
  (internal-show-cursor nil (not (internal-show-cursor-p))))
