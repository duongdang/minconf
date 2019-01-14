(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives
	       '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (package-initialize))

(add-to-list 'load-path "~/.emacs.d/lisp")

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

(setq package-list '(auto-complete ensime better-defaults material-theme elpy py-autopep8 color-theme groovy-mode ws-butler fill-column-indicator flycheck magit))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
	(package-install package)))

;; auto-complete stuff
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(defun ac-common-setup ()
  (setq ac-sources (append ac-sources '(ac-source-gtags))))

(ido-mode t)

;; (require 'color-theme)
;; (load-theme 'wombat)

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

(put 'narrow-to-region 'disabled nil)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'java-mode-hook 'ensime-scala-mode-hook)

(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
	 (1+ (current-column))))))

(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
	      (hs-toggle-hiding)
	    (error t))
	  (hs-show-all))
    (toggle-selective-display column)))

(load-library "hideshow")
(global-set-key (kbd "C-c h") 'hs-toggle-hiding)
(global-set-key (kbd "C-c s") 'toggle-selective-display)

(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

(menu-bar-mode nil)
(tool-bar-mode nil)

;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
; (global-linum-mode t) ;; enable line numbers globally

;; (elpy-enable)
;; (require 'py-autopep8)
;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;; (add-hook 'python-mode-hook 'anaconda-mode)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setq-default fill-column 100)

(add-hook 'prog-mode-hook #'hs-minor-mode)

(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)

(setq smerge-command-prefix "\C-cv")

(global-set-key (kbd "C-x g") 'magit-status)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun xah-clean-empty-lines ()
  "Replace repeated blank lines to just 1.
Works on whole buffer or text selection, respects `narrow-to-region'.
URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
Version 2017-09-22"
  (interactive)
  (let ($begin $end)
    (if (region-active-p)
	(setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
	(narrow-to-region $begin $end)
	(progn
	  (goto-char (point-min))
	  (while (re-search-forward "\n\n\n+" nil "move")
	    (replace-match "\n\n")))))))

(require 'fill-column-indicator)
(add-hook 'after-change-major-mode-hook 'fci-mode)

(add-hook 'prog-mode-hook
	  (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
;; (add-hook 'prog-mode-hook
;;  	  (lambda () (add-to-list 'write-file-functions 'xah-clean-empty-lines)))

(require 'blacken)
(defvar blacken-line-length 99)
(add-hook 'python-mode-hook 'blacken-mode)

(global-flycheck-mode)
(column-number-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit yasnippet-snippets yaml-mode ws-butler pylint py-autopep8 material-theme groovy-mode flymake-python-pyflakes flymake-cursor flycheck fill-column-indicator ensime elpy dracula-theme color-theme better-defaults auto-complete anaconda-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
