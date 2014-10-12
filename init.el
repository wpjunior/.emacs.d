(add-to-list 'load-path "~/.emacs.d")

;; Setup packages
(require 'setup-package)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(ac-js2
     ag
     column-marker
     cython-mode
     debian-changelog-mode
     dockerfile-mode
     dropdown-list
     enh-ruby-mode
     erlang
     es-mode
     expand-region
     feature-mode
     find-file-in-repository
     flycheck
     git-blame
     gitignore-mode
     go-mode
     grizzl
     haml-mode
     helm
     jedi
     auto-complete
     jinja2-mode
     json-mode
     less-css-mode
     lua-mode
     magit
     git-rebase-mode
     git-commit-mode
     markdown-mode
     multiple-cursors
     php-mode
     projectile
     py-autopep8
     pyflakes
     python-environment
     python-mode
     rainbow-mode
     ruby-mode
     js2-mode
     smartparens
     solarized-theme
     web-mode
     yasnippet)))


(setq user-mail-address "wilsonpjunior@gmail.com")
(column-number-mode 0)
(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))
;; No backup files
(setq make-backup-files nil)

;; splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;; No f*cking bell
(setq ring-bell-function 'ignore)

;; delete trailing whitespaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Mac specific stuff
(when (eq system-type 'darwin)
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  ;; sets fn-delete to be right-delete
  (global-set-key [kp-delete] 'delete-char)
  (defadvice yes-or-no-p (around prevent-dialog activate)
    "Prevent yes-or-no-p from activating a dialog"
    (let ((use-dialog-box nil))
      ad-do-it))
  (defadvice y-or-n-p (around prevent-dialog-yorn activate)
    "Prevent y-or-n-p from activating a dialog"
    (let ((use-dialog-box nil))
      ad-do-it))
  (menu-bar-mode 1))

;; ecb
(setq ecb-tip-of-the-day nil)

;; scrool
(setq scroll-conservatively 1000)

;; No f*cking bell
(setq ring-bell-function 'ignore)

;; encoding
(setq current-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq current-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; disable arrow keys
;;(global-unset-key (kbd "<left>") )
;;(global-unset-key (kbd "<right>") )
;;(global-unset-key (kbd "<up>") )
;;(global-unset-key [(kbd "<down>")] )


(defun duplicate()
 "Duplicate it."
 (interactive)
 (let (
       (beg (line-beginning-position))
       (end (line-end-position)))
   (copy-region-as-kill beg end)
   (beginning-of-line)
   (forward-line 1)
   (yank)
   (newline)
   (forward-line -1)))

(global-set-key "\M-s\M-s" 'duplicate)

;; print code in .pdf
(defun print-to-pdf ()
  (interactive)
  (ps-spool-buffer-with-faces)
  (switch-to-buffer "*PostScript*")
  (write-file "/tmp/tmp.ps")
  (kill-buffer "tmp.ps")
  (setq cmd (concat "ps2pdf14 /tmp/tmp.ps " (buffer-name) ".pdf"))
  (shell-command cmd)
  (shell-command "rm /tmp/tmp.ps")
  (message (concat "Saved to:  " (buffer-name) ".pdf"))
  )

(require 'php-mode)
(require 'lua-mode)

;; Whitespaces
(setq show-trailing-whitespace t)
;;; Also highlight parens
(setq show-paren-delay 0 show-paren-style 'parenthesis)
(show-paren-mode 1)

;; no bars
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; highlight mark region
(transient-mark-mode 1)

;; gdb
(setq gdb-many-windows 1)

;; js2
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; show line numbers
(require 'linum)
(global-linum-mode 1)

;; scroll smoothly
(setq scroll-conservatively 10000)

(put 'upcase-region 'disabled nil)

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; better use the default C-x *left* and C-x *right*
(global-set-key (kbd "A-n") 'next-buffer)
(global-set-key (kbd "A-p") 'previous-buffer)
(global-set-key (kbd "A-f") 'next-multiframe-window)
(global-set-key (kbd "A-b") 'previous-multiframe-window)
(global-set-key (kbd "A-o") 'make-frame)

; clipboard
(setq x-select-enable-clipboard t)

;; comments
(global-set-key [(ctrl c) (c)] 'comment-region)
(global-set-key [(ctrl c) (d)] 'uncomment-region)

;; scrolling
(defun gcm-scroll-down ()
  (interactive)
  (scroll-up 1))
(defun gcm-scroll-up ()
  (interactive)
  (scroll-down 1))

(global-set-key [(meta n)] 'gcm-scroll-down)
(global-set-key [(meta p)] 'gcm-scroll-up)

(global-set-key (kbd "A-SPC") 'projectile-find-file)
(global-set-key (kbd "A-S-SPC") 'projectile-ag)
(global-set-key (kbd "A-m") 'mc/mark-next-like-this)

(setq feature-default-language "pt")

;; point to cucumber languages.yml or gherkin i18n.yml to use
;; exactly the same localization your cucumber uses
(setq feature-default-i18n-file "~/.emacs.d/i18n.yml")

;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))


;; Set the number to the number of columns to use.
(setq-default fill-column 79)

;; Show line number in the mode line.
(line-number-mode 1)

;; Show column number in the mode line.
(column-number-mode 1)

(require 'column-marker)
(set-face-background 'column-marker-1 "red")
(add-hook 'python-mode-hook
          (lambda () (interactive)
            (column-marker-1 fill-column)))

(setq flycheck-highlighting-mode 'lines)

(require 'py-autopep8)
(add-hook 'before-save-hook 'py-autopep8-before-save)

(defun git () (interactive) (magit-status "."))
(defun git-blame () (interactive) (mo-git-blame-current))

(global-set-key (kbd "A-g") 'git)
(global-set-key (kbd "A-d") 'git-blame)

(add-hook 'python-mode-hook 'jedi:setup)

(defun web-mode-hook ()
  "Hooks for web mode."
  (setq web-mode-markup-indent-offset 2)
  (web-mode-set-engine "django"))

(add-hook 'web-mode-hook 'web-mode-hook)

;; Loading YAS personal snippets
(yas-global-mode)
(yas/load-directory "~/.emacs.d/yasnippets")


;; autocomplete setup
(require 'auto-complete-config)

(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;; projectile to search fast mode file
(projectile-global-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq projectile-completion-system 'grizzl)
(smartparens-global-mode)

(color-theme-initialize)
(require 'solarized-dark-theme)

;; expand region plugin
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(maximize-frame)
(server-mode)
