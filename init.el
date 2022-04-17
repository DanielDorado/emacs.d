;;; init.el --- Emacs init file
;;  Author: Daniel Dorado
;;; Commentary:
;;; My Emacs config
;;; Code:
(defvar file-name-handler-alist-original file-name-handler-alist)

(setq file-name-handler-alist nil
      site-run-file nil)

(add-hook 'emacs-startup-hook ; hook run after loading init files
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-original)))

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(setq package-enable-at-startup nil)
(package-initialize)

;; Setting up the package manager. Install if missing.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t))

;; Load main config file "config.org"
(require 'org)
(windmove-default-keybindings) ;; move between windows with the 【Shift+→】/ 【Shift+←】/ 【Shift+↓】 / 【Shift+
;; Make windmove work in Org mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(setq org-log-done 'time)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   ))
(setq org-adapt-indentation nil)
(setq org-confirm-babel-evaluate nil)
(org-babel-load-file (expand-file-name (concat user-emacs-directory "config.org")))

(provide 'init)
;;; init.el ends here
(put 'downcase-region 'disabled nil)
