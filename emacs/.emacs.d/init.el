(require 'package)
(package-initialize)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))

(defvar my-packages '(better-defaults
                      evil
                      evil-leader
                      evil-surround
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

;; Evil setup

(require 'evil)
(evil-mode 1)

;; Visual
(global-linum-mode t)
(load-theme 'gruvbox t)
