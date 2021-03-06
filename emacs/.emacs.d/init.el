(setq tramp-ssh-controlmaster-options "")

(require 'package)
(package-initialize)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))

(defvar my-packages '(better-defaults
                      exec-path-from-shell
                      company
                      alchemist
                      elm-mode
                      erlang
                      helm
                      evil
                      evil-leader
                      evil-surround
                      flycheck
                      flycheck-elm
                      neotree
                      powerline
                      powerline-evil
                      projectile
                      helm-projectile
                      monokai-theme))

(defun my-missing-packages ()
  (let (missing-packages)
    (dolist (package my-packages (reverse missing-packages))
      (or (package-installed-p package)
          (push package missing-packages)))))

(defun ensure-my-packages ()
  (let ((missing (my-missing-packages)))
    (when missing
      ;; Check for new packages (package versions)
      (package-refresh-contents)
      ;; Install the missing packages
      (mapc (lambda (package)
              (when (not (package-installed-p package))
                (package-install package)))
            missing)
      ;; Close the compilation log.
      (let ((compile-window (get-buffer-window "*Compile-Log*")))
        (if compile-window
          (delete-window compile-window))))))

(ensure-my-packages)

;; Fix PATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; Evil setup

(require 'evil)
(require 'evil-leader)

(evil-mode 1)
(global-evil-leader-mode)

;; General
(require 'whitespace)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Company
(add-hook 'after-init-hook 'global-company-mode)

;; Helm setup
(helm-mode t)

;; Flycheck
(global-flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))

;; Powerline
(require 'powerline)
(require 'powerline-evil)
(powerline-evil-vim-color-theme)

;; Erlang
(setq erlang-indent-level 2)
(require 'erlang-start)

;; Elm
(setq elm-format-on-save t)
(setq elm-sort-imports-on-save t)
(setq elm-tags-on-save t)

;; Elixir
(require 'alchemist)
(add-to-list 'company-backends 'company-elm)

;; Shortcuts

(evil-define-key 'normal global-map
  "\C-p" 'helm-mini
  "\M-x" 'helm-M-x)

(define-key evil-motion-state-map (kbd "SPC") 'evil-ex)
(define-key evil-normal-state-map (kbd "[ SPC") 'evil-open-above)
(define-key evil-normal-state-map (kbd "] SPC") 'evil-open-below)
(evil-leader/set-leader ",")

(evil-leader/set-key
  "e" 'eval-last-sexp
  "3" 'neotree-toggle
  "1" 'helm-projectile-find-file
  "sp" 'helm-projectile-switch-project)

(evil-leader/set-key-for-mode 'elm-mode
  "t" 'elm-compile-add-annotations
  "ci" 'elm-compile-clean-imports
  "si" 'elm-sort-imports)

(evil-leader/set-key-for-mode 'erlang-mode
  "a" 'erlang-align-arrows)

;; Projectile

(projectile-global-mode)
(helm-projectile-on)

(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;; Visual

(defun init-visual ()
  (global-linum-mode t)

  (load-theme 'monokai t)
  (set-face-attribute 'default nil :font "Monaco-11")

  (when (memq window-system '(mac ns))
    (fringe-mode '(20 . 0)))

  (if (< (length command-line-args) 2)
      (setq initial-buffer-choice (car (helm-recentf)))))

(init-visual)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (powerline-evil neotree monokai-theme helm-projectile flycheck exec-path-from-shell evil-surround evil-leader erlang elm-mode better-defaults alchemist))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
