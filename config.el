;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
 
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

; Load ef-themes
(require 'ef-themes)

; General Config

(setq lsp-file-watch-threshold 5000)
(setq user-full-name "Gershon Koks"
      user-mail-address "gershon@hyvemobile.co.za")

 (setq doom-font (font-spec :family "Hasklug Nerd Font" :size 17 :weight 'regular)
       doom-variable-pitch-font (font-spec :family "Hasklug Nerd Font" :size 17))

;; Load a specific theme
(ef-themes-select 'ef-dream)
;;(setq doom-theme 'doom-opera)
;;(setq doom-theme 'doom-flatwhite)

(setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)

(setq org-directory "~/org/")

;; MODE LINE
(setq doom-modeline-hud nil)

;; Key Bindings
(map! :leader
      :desc "Spell check buffer" "s b" #'flyspell-buffer)

(map! :leader
      :desc "Symbols" "c S" #'consult-lsp-file-symbols)

(map! :leader
      :desc "Find File" "." #'dired-create-empty-file)

;; Spell Check
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(defun +flyspell-check-entire-buffer ()
  "Force flyspell to check the entire buffer after enabling."
  (when (and (bound-and-true-p flyspell-mode)
             (fboundp 'flyspell-buffer))
    (flyspell-buffer)))

(add-hook 'flyspell-mode-hook #'+flyspell-check-entire-buffer)

(setq flycheck-check-syntax-automatically '(mode-enabled save new-line idle-change))

;; Configure Post-Frame mini buffer
(use-package! vertico-posframe
  :after vertico
  :init
  (defun my/vertico-maybe-enable-posframe ()
    "Enable vertico-posframe if completion count exceeds threshold."
    (let ((threshold 1)) ;; you can change this number
      (if (and (boundp 'vertico--count)
               (> vertico--count threshold))
          (vertico-posframe-mode 1)
        (vertico-posframe-mode -1))))
  
  ;; Hook into Vertico update cycle
  (add-hook 'vertico--update-hook #'my/vertico-maybe-enable-posframe)

  ;; Ensure posframe is off by default
  (vertico-posframe-mode -1)

  :custom
  (vertico-posframe-poshandler #'posframe-poshandler-frame-top-right-corner)
  (vertico-posframe-border-width 1)
  (vertico-posframe-width 70)
  (vertico-posframe-height 20))

;; Hooks

(add-hook! 'go-mode-hook
  (add-hook 'before-save-hook #'lsp-format-buffer nil 'local)
  (add-hook 'before-save-hook #'lsp-organize-imports nil 'local))

;; Package Config

(use-package org
  :config
  (setq org-ellipsis " ")
  (setq org-agenda-files
        '("~/Documents/Hyve/WeeklyNotes.org"))
)

(use-package hl-todo
  :hook (org-mode . hl-todo-mode)
)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package! lsp-treemacs
  :after lsp)

(use-package! typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config (setq typescript-indent-level 2))

(use-package! php-mode
  :mode "\\.php\\'"
  :hook (php-mode . lsp-deferred))

(use-package! odin-mode
  :mode "\\.odin\\'"
  :hook ((odin-mode . lsp-deferred))
  :config
  (after! lsp-mode
    (add-to-list 'lsp-language-id-configuration '(odin-mode . "odin"))
    (lsp-register-client
      (make-lsp-client
        :new-connection (lsp-stdio-connection "/usr/bin/ols")
        :major-modes '(odin-mode)
        :server-id 'ols))))
