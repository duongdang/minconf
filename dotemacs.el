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

(setq package-list '(auto-complete ensime better-defaults material-theme elpy py-autopep8 color-theme anaconda-mode))

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

(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

(elpy-enable)
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'python-mode-hook 'anaconda-mode)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
