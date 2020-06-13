;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(setq load-path (cons "~/.emacs.d/myload" load-path))
;;(require 'initchart)
;;(initchart-record-execution-time-of load file)
;;(initchart-record-execution-time-of require feature)

(package-initialize)

(load "~/.emacs.d/load-packages.el")

(setq default-directory "~/")
(setq-default auto-save-default t)

;;change window with C-t
(define-key global-map (kbd "C-t") 'other-window)

;;toggle-truncate-lines with C-c
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;;backspace with C-h
(keyboard-translate ?\C-h ?\C-?)

;;set tab width
(setq-default tab-width 3)

;; kill line and remove "\n" when cursor is at head
(defun kill-line-twice (&optional numlines)
  "Acts like normal kill except kills entire line if at beginning"
  (interactive "p")
  (cond ((or (= (current-column) 0)
             (> numlines 1))
         (kill-line numlines))
        (t (kill-line))))
(global-set-key "\C-k" 'kill-line-twice)

;;; mode line customization
;; show colum in mode line
(column-number-mode t)

;; show colum and line num in the region
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines,%d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ""))

;; (add-to-list 'default-mode-line-format
;;              '(:eval (count-lines-and-chars)))

;; disable tab
(setq-default indent-tabs-mode nil)

;; Language.
(set-language-environment 'Japanese)

;; Coding system.
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; do not make lock, backup files
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq delete-auto-save-files t)

;; underline for current line
(setq hl-line-face 'underline)
(global-hl-line-mode)

;; not creating backup file
(setq make-backup-files nil)

;; show line
;;(global-linum-mode t)

;; hilight parent
(show-paren-mode 1)

;; yes no for y,n 
(fset 'yes-or-no-p 'y-or-n-p)

;; show current function name
(which-function-mode 1)

(global-set-key (kbd "C-c s") 'shell)
(global-set-key (kbd "C-c i") 'helm-imenu)
(global-set-key (kbd "C-c p") 'helm-projectile)
(global-set-key (kbd "C-c g") 'helm-git-grep)
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-c y") 'helm-show-kill-ring)
(define-key global-map (kbd "M-x") 'helm-M-x)
(define-key global-map (kbd "C-x b") 'helm-mini)


(global-set-key "\C-h" 'delete-backward-char)

;;enable auto-complete
(global-auto-complete-mode t)

;; enable auto-complete even in text mode
(add-to-list 'ac-modes 'text-mode)

;; triger auto-complete with tab
(ac-set-trigger-key "TAB")

;; enable to select prediction with C-p and C-n
(setq ac-use-menu-map t)

;; enable fuzzy auto complete
(setq ac-use-fuzzy t) 

;; change line mode with 
(define-key global-map (kbd "C-c n") 'global-linum-mode)

;;
;;moccur
;;

;; enable AND search with moccur
(setq moccur-split-word t)

;;run the moccur with M-o
(define-key global-map (kbd "M-o") 'occur-by-moccur)

;; point-undo
(when (require 'point-undo nil t)
  (define-key global-map [f5] 'point-undo)
  (define-key global-map [f6] 'point-redo))

;;
;;magit
;;
(require 'magit)
(setq-default magit-auto-revert-mode nil)
(setq vc-handled-backends '())
(eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
(define-key global-map (kbd "C-x m") 'magit-status)
(define-key global-map (kbd "C-c l") 'magit-blame)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-added ((t (:background "black" :foreground "green"))))
 '(magit-diff-added-highlight ((t (:background "white" :foreground "green"))))
 '(magit-diff-removed ((t (:background "black" :foreground "blue"))))
 '(magit-diff-removed-hightlight ((t (:background "white" :foreground "blue"))))
 '(magit-hash ((t (:foreground "red")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (esup browse-kill-ring yasnippet yaml-mode undohist scala-mode python-mode point-undo nyan-mode moccur-edit markdown-mode magit json-mode highlight-symbol helm-pydoc helm-projectile helm-gtags helm-git-grep groovy-mode gradle-mode go-mode fuzzy flycheck exec-path-from-shell dockerfile-mode coffee-mode auto-complete anzu))))

(add-to-list 'auto-mode-alist '("\.gradle\'" .groovy-mode))

;; for multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c w") 'mc/mark-all-symbols-like-this)
