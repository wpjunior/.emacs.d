;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setenv "PATH" (concat "$HOME/go/bin:/usr/local/bin:" (getenv "PATH") ))
(setenv "GOPATH" "/home/wilson/go")

(add-to-list 'exec-path "~/go/bin")
(add-to-list 'exec-path "/usr/local/bin")

(add-to-list 'load-path "~/.emacs.d")
;; Setup packages
(require 'setup-package)
(require 'iso-transl)


;; setup modes
;; (package-required 'cython-mode)
;; (package-required 'debian-changelog-mode)
;; (package-required 'dockerfile-mode)
;; (package-required 'erlang)
;; (package-required 'es-mode)
;; (package-required 'feature-mode)
(package-required 'go-mode)
;;(package-required 'go-autocomplete)
(package-required 'haml-mode)
(package-required 'jinja2-mode)
;; (package-required 'js2-mode)
;; (package-required 'rjsx-mode)
(package-required 'json-mode)
(package-required 'less-css-mode)
(package-required 'lua-mode)
(package-required 'markdown-mode)
(package-required 'puppet-mode)
(package-required 'python-mode)
(package-required 'rainbow-mode)
(package-required 'ruby-mode)
(package-required 'web-mode)

;; tools
(package-required 'ac-js2)
(package-required 'ag)
(package-required 'auto-complete)
;;(package-required 'column-marker)
;;(package-required 'dropdown-list)
(package-required 'enh-ruby-mode)
(package-required 'expand-region)
(package-required 'find-file-in-repository)
(package-required 'flycheck)
(package-required 'flycheck-pyflakes)
(package-required 'grizzl)
(package-required 'helm)
(package-required 'jedi)
(package-required 'maxframe)
(package-required 'move-text)
(package-required 'multiple-cursors)
(package-required 'projectile)
(package-required 'py-autopep8)
;;(package-required 'pyflakes)
(package-required 'python-environment)
(package-required 'smartparens)
(package-required 'yasnippet)
(package-required 'git-gutter)
(package-required 'use-package)
(package-required 'material-theme)


;;(setq default-frame-alist '((undecorated . t)))

(setq-default mode-line-format
       (list ""
        'mode-line-modified
        'mode-line-frame-identification
        'mode-line-buffer-identification
        ;; Note that this is evaluated while making the list.
        ;; It makes a mode line construct which is just a string.
        'default-directory
        '(line-number-mode " L%l")
        '(column-number-mode " C%c")
        ))

;; If you enable global minor mode
(global-git-gutter-mode t)

;; If you would like to use git-gutter.el and linum-mode
;;(git-gutter:linum-setup)


;; theme
;; (package-required 'solarized-theme)
(load-theme 'material t)

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

;; auto revert files
(global-auto-revert-mode t)
(menu-bar-mode 0)
(blink-cursor-mode 0)

;; Mac specific stuff
(when (eq system-type 'darwin)
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
;; (defun print-to-pdf ()
;;   (interactive)
;;   (ps-spool-buffer-with-faces)
;;   (switch-to-buffer "*PostScript*")
;;   (write-file "/tmp/tmp.ps")
;;   (kill-buffer "tmp.ps")
;;   (setq cmd (concat "ps2pdf14 /tmp/tmp.ps " (buffer-name) ".pdf"))
;;   (shell-command cmd)
;;   (shell-command "rm /tmp/tmp.ps")
;;   (message (concat "Saved to:  " (buffer-name) ".pdf"))
;;   )

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
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
(setq js2-basic-offset 2)

;; show line numbers
;;(require 'linum)
;;(global-linum-mode 1)

;; scroll smoothly
(setq scroll-conservatively 10000)

(put 'upcase-region 'disabled nil)

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

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

;; (global-set-key [(meta n)] 'gcm-scroll-down)
;; (global-set-key [(meta p)] 'gcm-scroll-up)


(global-set-key (kbd "s-SPC") 'projectile-find-file)
(global-set-key (kbd "s-S-SPC") 'projectile-ag)
(global-set-key (kbd "s-m") 'mc/mark-next-like-this)

(setq feature-default-language "pt")

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

;; (require 'column-marker)
;; (set-face-background 'column-marker-1 "red")
;; (add-hook 'python-mode-hook
;;           (lambda () (interactive)
;;             (column-marker-1 fill-column)))

(setq flycheck-highlighting-mode 'lines)

;;(require 'py-autopep8)
;;(setq py-autopep8-options '("--max-line-length=119"))
;;(add-hook 'before-save-hook 'py-autopep8-before-save)

;;(add-hook 'python-mode-hook 'jedi:setup)

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

;;(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

(ac-config-default)


;; projectile to search fast mode file
(projectile-global-mode)
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'after-init-hook 'hs-minor-mode)
(setq projectile-completion-system 'grizzl)
(setq ag-highlight-search t)
(setq ag-reuse-window 't)
(smartparens-global-mode)

;; ruby configuration
(setq ruby-deep-indent-paren nil)
(setq enh-ruby-deep-indent-paren nil)

(load-library "hideshow")
(global-set-key (kbd "A-h") 'toggle-hiding)
(global-set-key (kbd "A-s") 'toggle-selective-display)
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)
(add-hook 'js2-mode-hook        'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

(add-hook 'before-save-hook 'whitespace-cleanup)


(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (auto-complete-mode 1)
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;;(load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
  (local-set-key (kbd "M-.") 'godef-jump))

;;(with-eval-after-load 'go-mode
;;  (require 'go-autocomplete))

(add-hook 'go-mode-hook 'my-go-mode-hook)

;; expand region plugin
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;(require 'solarized-dark-theme)

(require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; remove trailing whitespace after saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

;;(add-hook 'typescript-mode-hook #'setup-tide-mode)

(server-mode)
(global-flycheck-mode)
;;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;;  '(custom-safe-themes
;;    (quote
;;     ("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
;;  '(js-indent-level 2)
;;  '(package-selected-packages
;;    (quote
;;     (material-theme fish-mode yaml-tomato yaml-mode editorconfig solarized-theme yasnippet web-mode smartparens rainbow-mode python-mode python-environment pyflakes py-autopep8 puppet-mode projectile multiple-cursors move-text maxframe markdown-mode lua-mode less-css-mode json-mode jinja2-mode helm haml-mode grizzl go-mode flycheck-pyflakes find-file-in-repository feature-mode expand-region es-mode erlang enh-ruby-mode dropdown-list dockerfile-mode debian-changelog-mode cython-mode column-marker ag ac-js2))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#263238"))
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-safe-themes
   '("7922b14d8971cce37ddb5e487dbc18da5444c47f766178e5a4e72f90437c0711" "0e8bac1e87493f6954faf5a62e1356ec9365bd5c33398af3e83cfdf662ad955f" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default))
 '(fci-rule-color "#37474f")
 '(hl-sexp-background-color "#1c1f26")
 '(package-selected-packages
   '(terraform-mode protobuf-mode snazzy-theme yaml-mode hackernews all-the-icons-dired git-commit git-blamed d-mode yasnippet-snippets tide yasnippet web-mode smartparens rjsx-mode rainbow-mode python-mode python-environment pyflakes py-autopep8 puppet-mode projectile multiple-cursors move-text maxframe material-theme markdown-mode lua-mode json-mode jinja2-mode helm haml-mode grizzl go-mode git-gutter flycheck-pyflakes find-file-in-repository feature-mode expand-region es-mode erlang enh-ruby-mode dockerfile-mode debian-changelog-mode cython-mode ag ac-js2))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "SRC" :slant normal :weight normal :height 162 :width normal)))))
