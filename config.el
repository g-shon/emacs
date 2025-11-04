;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
 
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

; Load ef-themes
(require 'ef-themes)

; General Config

(setq lsp-file-watch-threshold 5000)
(setq user-full-name "Gershon Koks"
      user-mail-address "gershon@hyvemobile.co.za")

 ;;(setq doom-font (font-spec :family "Hasklug Nerd Font" :size 17 :weight 'regular)
 ;;      doom-variable-pitch-font (font-spec :family "Hasklug Nerd Font" :size 17))

 (setq doom-font (font-spec :family "Aporetic Sans Mono" :size 17)
       doom-variable-pitch-font (font-spec :family "Aporetic Sans Mono" :size 17))

;; (setq doom-font (font-spec :family "Source Code Pro" :size 17 :weight 'semi-bold)
;;       doom-variable-pitch-font (font-spec :family "Source Code Pro" :size 17 :weight 'semi-bold))

;; (setq doom-font (font-spec :family "Anonymous Pro" :size 18 :weight 'semi-bold)
;;       doom-variable-pitch-font (font-spec :family "Anonymous Pro" :size 18 :weight 'semi-bold))

(global-set-key (kbd "C-c a") 'copy-from-above-command)

;; Load a specific theme
 
(use-package doric-themes
  :ensure nil
  :demand t
  :load-path "~/.config/emacs/themes/doric-themes")

;;(doric-themes-select 'doric-obsidian)
;;(doric-themes-select 'doric-water)
;;(ef-themes-select 'ef-eagle)
(ef-themes-select 'ef-dream)
;;(ef-themes-select 'ef-light)
;;(ef-themes-select 'ef-kassio)
;;(ef-themes-select 'ef-elea-dark)
;;(setq doom-theme 'doom-opera)
;;(setq doom-theme 'doom-nord-aurora)
;;(load-theme 'everforest-hard-t dark)  
;;(setq doom-theme 'modus-operandi-deuteranopia) 
;;(setq doom-theme 'nord)

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
;;(use-package! vertico-posframe
;;  :after vertico
;;  :init
;;  ;; Enable vertico-posframe by default
;;  (vertico-posframe-mode 1)
;;  :custom
;;  ;; Position the posframe at the center of the frame
;;  (vertico-posframe-poshandler #'posframe-poshandler-frame-center)
;;  ;; Customize the appearance
;;  (vertico-posframe-border-width 1)
;;  (vertico-posframe-width 100)
;;  (vertico-posframe-height 20)
;;  ;; Optional: Add fringes for better aesthetics
;;  (vertico-posframe-parameters
;;   '((left-fringe . 8)
;;     (right-fringe . 8))))

(after! projectile
  (dolist (dir '("node_modules" ".next" "dist"))
    (add-to-list 'projectile-globally-ignored-directories dir))
  (add-to-list 'projectile-globally-ignored-files "*.log"))

;; Configure Drag stuff
(use-package! drag-stuff
  :config
  (drag-stuff-global-mode 1)  ; Enable globally
  (drag-stuff-define-keys)) ; Optional: sets up M-j / M-k bindings


;; Configure Golden Ratio
(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config
  (golden-ratio-mode 1)
  ;; Add Evil window movement commands to trigger golden-ratio
  (dolist (cmd '(evil-window-left
                 evil-window-right
                 evil-window-up
                 evil-window-down
                 evil-window-next
                 evil-window-prev
                 evil-window-split
                 evil-window-vsplit
                 evil-window-new
                 evil-window-delete))
    (add-to-list 'golden-ratio-extra-commands cmd))
  ;; Exclude specific modes if needed
  (setq golden-ratio-exclude-modes '(occur-mode)))


;; Hooks
;; (add-hook! go-mode #'lsp!)
;; (add-hook! go-mode-hook
;;   (add-hook 'before-save-hook #'lsp-format-buffer nil 'local)
;;   (add-hook 'before-save-hook #'lsp-organize-imports nil 'local))

(after! go-mode
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'lsp-format-buffer nil t)
              (add-hook 'before-save-hook #'lsp-organize-imports nil t))))

;; Package Config

(use-package org
  :config
  (setq org-ellipsis " ")
  (setq org-agenda-files
        '("~/Documents/Hyve/WeeklyNotes.org"))
  (setq org-priority-highest 1
      org-priority-lowest 5
      org-priority-default 3)
)

(use-package hl-todo
  :hook (org-mode . hl-todo-mode)
)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package! denote
  :ensure t
  :custom
  (denote-dictory "~/Documents/Notes")
  (denote-known-keywords '("hyve" "dealsquest" "general")))

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

;; Configure Databases
(setq auth-sources '("~/.netrc"))
(setq sql-product 'postgres)

(setq sql-connection-alist
      '((dealsquest-local
         (sql-product 'postgres)
         (sql-server "localhost")
         (sql-user "admin")
         (sql-database "ddw")
         (sql-password "secret")
         (sql-port 5432))))

(after! lsp-clangd
  (setq lsp-clangd-binary "clangd" ;; or full path to clangd
        lsp-clangd-version "20"
        lsp-clients-clangd-args '("--header-insertion=never"
                                  "--clang-tidy"
                                  "--completion-style=detailed")))

(use-package! meson-mode
  :mode ("meson\\.build\\'" . meson-mode)
  :hook (meson-mode . rainbow-delimiters-mode))

(use-package! protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))

(after! protobuf-mode
  (add-hook 'protobuf-mode-hook #'lsp!))
