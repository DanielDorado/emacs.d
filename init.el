;; (add-to-list 'load-path "~/.emacs.d/elisp/")

;;
;; Apparence
;;
(when window-system
;;  (add-to-list 'default-frame-alist '(background-color . "black"))
;  (add-to-list 'default-frame-alist '(foreground-color . "wheat")))
;;  (set-face-attribute 'default nil :font "Droid Sans Mono-12")
;; (modify-frame-parameters nil '((wait-for-wm . nil))
)

;; prevent silly initial splash screen
(setq inhibit-splash-screen t)

(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(global-font-lock-mode t)
;; (define-key global-map [(control tab)] 'dabbrev-expand)
;; (set-foreground-color "wheat")
;; (set-background-color "grey22")
;; (set-cursor-color "grey80")


;; (menu-bar-mode 0)
(tool-bar-mode 0)

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
    pyvenv
    rsense
    zenburn-theme
    markdown-mode
    yaml-mode
    nginx-mode
    go-mode
    auto-complete
    go-autocomplete)

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
;; Auto complete
;;

(require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
;; (setq ac-dictionary-files (list (concat user-emacs-directory ".dict")))
(ac-config-default)


;;
;; PYTHON
;;
;; I test **elpy** but I don't like it. Install a lot of packages
;; autmatically.
;;
;; (elpy-enable)
;;
;; (setenv "PYTHONPATH" "/var/opt/spark-2.0.0-bin-hadoop2.7/python/lib/py4j-0.10.1-src.zip:/var/opt/spark-2.0.0-bin-hadoop2.7/python/lib/pyspark.zip")
(setenv "PYTHONPATH" "/var/opt/spark-2.0.0-bin-hadoop2.7/python/lib")
;; Standard Jedi.el setting
;; (add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
 ; Method signature in minibuffer. To avoid two tooltips: this and the
 ; auto-complete one
(setq jedi:tooltip-method nil)
(setq jedi:use-shortcuts t)

;; (setq jedi:server-args
;;       '("--sys-path" "/var/opt/spark-2.0.0-bin-hadoop2.7/python/lib/py4j-0.10.1-src.zip"
;;         "--sys-path" "/var/opt/spark-2.0.0-bin-hadoop2.7/python/lib/pyspark.zip"
;;         "--log-level" "DEBUG"))

(add-hook 'pyvenv-post-activate-hooks 'jedi:stop-server)
(add-hook 'pyvenv-post-activate-hooks 'jedi:start-server)

(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
;; default is "pyflakes" "flake8" flake8 includes pyflakes and pep8
(setq flymake-python-pyflakes-executable "flake8")
;; You can pass extra arguments to the checker program.
(setq flymake-python-pyflakes-extra-arguments '("--ignore=W501"))

(add-hook 'python-mode-hook
              (lambda ()
                (define-key python-mode-map [f1] 'jedi:show-doc)))

(setq python-check-command "flake8")

;; trying ipython tab completion: that works :)

(when (executable-find "ipython")
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion #SILENT-234abD3"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))#SILENT-234abD3"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s''')) #SILENT-234abD3"
   )
)

(defun clean-history ()
  "Clean #SILENT in buffer"
  (interactive)
  (mark-whole-buffer)
  (flush-lines "SILENT-234abD3")
  (end-of-buffer))

(add-hook 'inferior-python-mode-hook
              (lambda ()
                (define-key inferior-python-mode-map [f2] 'clean-history)))

;;
;; Groovy
;;
;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
(add-to-list 'load-path "~/.emacs.d/elisp")

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
;; Golang
;;
(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving                                                    
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "source ~/bin/search_main.sh && go build -v && go test -v && go vet"))
  ; Godef jump key binding
  ; Godef jump key binding                                                      
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'pop-tag-mark)
  (auto-complete-mode 1)
  )

(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook 'my-go-mode-hook)

;;(with-eval-after-load 'go-mode
(require 'go-autocomplete)
;;)

;;
;; Markdown
;; 
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))

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
;;  (setq whitespace-style '(face empty tabs lines-tail trailing))
;; (global-whitespace-mode t)

;; auto-fill-mode
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'python-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 79)

;; ZEN Burn theme. Colors. Fonts...
(load-theme 'zenburn t)
(put 'downcase-region 'disabled nil)


;; (setq-default indent-tabs-mode nil)


;;
;; PATH and environment variables
;;

;; PATH environment
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;; Other VARS
;; golang GOPATH
;; (setenv "GOPATH" "/home/dani/go/")
(setenv "GOPATH" "/home/dani/src/paasdepl")

