;; Shawn's init.el. First load package libraries

(require 'package) ;; You might already have this line
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(add-to-list 'package-archives
	                  '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)

(package-initialize) ;; You might already have this line

(setq inhibit-startup-message t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company mode
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-c C-i") 'counsel-company)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil mode

(add-to-list 'load-path "~/.emacs.d/evil")
(setq evil-want-C-i-jump nil)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq evil-want-C-u-scroll t)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-key "e" 'find-file)
(evil-leader/set-key-for-mode 'python-mode "s" 'elpy-shell-send-region-or-buffer) 
(evil-leader/set-key-for-mode 'python-mode "d" 'elpy-goto-definition) 

(require 'evil)
(evil-mode 1)
(require 'evil-surround)
(global-evil-surround-mode 1)
(require 'powerline)
(display-time-mode t)

(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

(global-linum-mode t)

;; esc quits
(defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
	(setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy, counsel, swiper

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(setq ivy-wrap t)

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
(define-key ivy-switch-buffer-map [escape] 'minibuffer-keyboard-quit)
(define-key ivy-mode-map [escape] 'minibuffer-keyboard-quit)
(define-key ivy-occur-mode-map [escape] 'minibuffer-keyboard-quit)
(define-key ivy-occur-grep-mode-map [escape] 'minibuffer-keyboard-quit)

(defun ivy-yank-action (x)
  (kill-new x))

(defun ivy-copy-to-buffer-action (x)
  (with-ivy-window
    (insert x)))

(defun counsel-find-file-in-my-work-directory ()
  (interactive)
    (counsel-find-file "/home/shawn/Desktop/research/dessau/video_feedback/"))
(global-set-key (kbd "C-x w C-f") 'counsel-find-file-in-my-work-directory)

(ivy-set-actions
 t
 '(("i" ivy-copy-to-buffer-action "insert")
   ("y" ivy-yank-action "yank")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit

(require 'evil-magit)
(global-set-key (kbd "C-x g") 'magit-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org mode

(require 'org)

(add-to-list 'load-path "~/.emacs.d/plugins/evil-org")
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(textobjects insert navigation additional shift todo))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(add-hook 'org-mode-hook
   (lambda ()
     (setq-local yas/trigger-key [tab])
	(define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))

(defun org-completion-symbols ()
  (when (looking-back "=[a-zA-Z]+")
    (let (cands)
      (save-match-data
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward "=\\([a-zA-Z]+\\)=" nil t)
	    (cl-pushnew
	     (match-string-no-properties 0) cands :test 'equal))
	  cands))
      (when cands
	(list (match-beginning 0) (match-end 0) cands)))))

(defun ora-cap-filesystem ()
  (let (path)
    (when (setq path (ffap-string-at-point))
      (let ((compl
	     (all-completions path #'read-file-name-internal)))
	(when compl
	  (let ((offset (ivy-completion-common-length (car compl))))
	                (list (- (point) offset) (point) compl)))))))

(setq completion-at-point-functions
      '(org-completion-symbols
	ora-cap-filesystem
	org-completion-refs))


(setq org-capture-templates
  (quote (("t" "todo" entry (file (concat org-directory "/gtd.org"))
	   "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
	  ("n" "note" entry (file (concat org-directory "/gtd.org"))
	   "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
	  ("j" "Journal" entry (file+datetree (concat org-directory "/diary.org"))
	   "* %?\n%U\n" :clock-in t :clock-resume t)
	  )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; material theme

(load-theme 'material t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elpy

(elpy-enable)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(company-idle-delay 0.3)
 '(company-tooltip-idle-delay 0.3)
 '(custom-safe-themes
   (quote
    ("93f9654f91d31e9a9ec6ea2fcffcfcab38353a9588673f2b750e591f704cd633" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(elpy-rpc-python-command "python3")
 '(global-font-lock-mode t)
 '(python-shell-interpreter "python3")
 '(pyvenv-workon "video_feedback"))
(setq elpy-rpc-backend "jedi")


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "white"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "chartreuse3")))))
(put 'narrow-to-page 'disabled nil)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(setq flycheck-check-syntax-automatically '(save mode-enable))

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(defvar my-packages
  '(paredit
    clojure-mode
    cider
    rainbow-delimiters
    tagedit))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


(add-to-list 'load-path "~/.emacs.d/customizations")
(load "editing.el")
(load "shell-integration.el")


;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")
(load "setup-clojure.el")
(load "setup-js.el")

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
