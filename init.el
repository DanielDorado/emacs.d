;; (add-to-list 'load-path "~/.emacs.d/elisp/")

;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
;; (ac-config-default)

;;
;; Apparence
;;
(when window-system
;;  (add-to-list 'default-frame-alist '(background-color . "black"))
;  (add-to-list 'default-frame-alist '(foreground-color . "wheat")))
;; (set-face-attribute 'default nil :font "Droid Sans Mono-12"))
;; (modify-frame-parameters nil '((wait-for-wm . nil))
)

(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(global-font-lock-mode t)
;; (define-key global-map [(control tab)] 'dabbrev-expand)
;; (set-foreground-color "wheat")
;; (set-background-color "grey22")
;; (set-cursor-color "grey80")


;; (menu-bar-mode 0)
(tool-bar-mode 0)

;;
;; Added By Custom
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(current-language-environment "utf-8")
 '(default-input-method "rfc1345")
 '(global-font-lock-mode t nil (font-lock))
 '(inferior-lisp-program "lein repl")
 '(ruby-indent-level 4)
 '(show-paren-mode t nil (paren)))

(fset 'jsmessages
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217765 return 33 C-home 134217765 32 61 32 34 return 61 return 33 C-home 134217765 40 backspace 34 59 return return 33] 0 "%d")) arg)))

;;
;; Melpa, Package Archive for Emacs Lisp
;; I before used Marmelade but some guy said that Melpa is better.
;;
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;
;; Below scripts gurantees all packages in package-list are installed when Emacs 
;; is started.
;;
(require 'cl)
;; Guarantee all packages are installed on start
(defvar packages-list
  '(clojure-mode
    paredit
    autopair
    jedi
    flymake-python-pyflakes
    rsense
    zenburn-theme)
  "List of packages needs to be installed at launch")
(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))
(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))


;;
;; PYTHON
;;
;; I test **elpy** but I don't like it. Install a lot of packages
;; autmatically.
;;
;; (elpy-enable)
;;
;; (setenv "PYTHONPATH" "/home/dani/git/hmi/hmi_common")
;; Standard Jedi.el setting
;; (add-hook 'python-mode-hook 'autopair-mode)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
 ; Method signature in minibuffer. To avoid two tooltips: this and the
 ; auto-complete one
(setq jedi:tooltip-method nil)
(setq jedi:use-shortcuts t)

(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
;; default is "pyflakes" "flake8" flake8 includes pyflakes and pep8
(setq flymake-python-pyflakes-executable "flake8")
;; You can pass extra arguments to the checker program.
; (setq flymake-python-pyflakes-extra-arguments '("--ignore=W806"))

(eval-after-load 'python '(define-key python-mode-map [f1] 'jedi:show-doc))
(add-hook 'python-mode-hook
              (lambda ()
                (define-key python-mode-map [f1] 'jedi:show-doc)))

;;
;; Groovy
;;
;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
(add-to-list 'load-path "~/.emacs.d/")

;;
;; Clojure
;;
; TODO: Fix this
;    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
;   (add-hook 'scheme-mode-hook     'enable-paredit-mode)
(add-hook 'clojure-mode-hook    'paredit-mode)
(add-hook 'slime-repl-mode-hook    'paredit-mode)
(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)



;;
;; Ruby
;;
;; cl is required by rsense
(require 'cl)
;; RSense
(setq rsense-home (expand-file-name "~/opt/rsense"))
(require 'rsense)
 
;; Rsense + Autocomplete
(add-hook 'ruby-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-rsense-method)
            (add-to-list 'ac-sources 'ac-source-rsense-constant)))


;;
;; Markdown
;; 
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;
;; C-Etags
;;
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command 
   (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

;;
;; Keys
;;
(global-set-key [f5] 'recompile)
(global-set-key "\C-xg" 'goto-line)
(put 'upcase-region 'disabled nil)



;; QML

(add-to-list 'auto-mode-alist '("\\.qml\\'" . js-mode))

;; column-marker

 (require 'whitespace)
 (setq whitespace-style '(face empty tabs lines-tail trailing))
 (global-whitespace-mode t)

;; auto-fill-mode
(set-fill-column 80)
(auto-fill-mode)

;; ZEN Burn theme. Colors. Fonts...
(load-theme 'zenburn t)
