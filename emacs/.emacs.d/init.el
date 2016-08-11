(setq tramp-ssh-controlmaster-options "")

(require 'package)
(package-initialize)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))

(defvar my-packages '(better-defaults
                      exec-path-from-shell
                      erlang
                      elm-mode
                      helm
                      evil
                      evil-leader
                      evil-surround
                      flycheck
                      projectile
                      helm-projectile
                      gruvbox-theme))

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

;; Helm setup

(require 'helm-config)
(helm-mode t)
(require 'helm-projectile)

;; Flycheck
(global-flycheck-mode)

;; Erlang
(setq erlang-indent-level 2)
(require 'erlang-start)

;; Elm
(setq elm-format-on-save t)
(setq elm-tags-on-save t)

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
  "f" 'helm-projectile-find-file)

;; Visual

(global-linum-mode t)
(load-theme 'gruvbox t)
