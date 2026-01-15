;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(setq load-path (cons "~/.emacs.d/myload" load-path))
;;(require 'initchart)
;;(initchart-record-execution-time-of load file)
;;(initchart-record-execution-time-of require feature)

(defconst LOCALE_LANG "ja_JP.UTF-8")
(setenv "LANG" LOCALE_LANG)
(setenv "LC_CTYPE" LOCALE_LANG)
(setenv "LC_ALL" LOCALE_LANG)

(package-initialize)

(load "~/.emacs.d/load-packages.el")

(setq default-directory "~/")
(setq-default auto-save-default t)

;;Change window with C-t
(define-key global-map (kbd "C-t") 'other-window)

;;toggle-truncate-lines with C-c
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;;backspace with C-h
(keyboard-translate ?\C-h ?\C-?)

;;set tab width
(setq-default tab-width 4)

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

;; Coding system & IME (macOS IME, hardware 英数/かな)
(defun lf/setup-locale-and-ime ()
  (set-locale-environment LOCALE_LANG)
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8-unix)
  (setq default-buffer-file-coding-system 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (set-terminal-coding-system 'utf-8-unix)
  (setq default-input-method nil)
  (global-unset-key (kbd "C-\\")))
(lf/setup-locale-and-ime)
(add-hook 'emacs-startup-hook #'lf/setup-locale-and-ime)
(add-hook
 'after-make-frame-functions
 (lambda (frame)
   ;; For daemon frames, apply locale/coding settings per new frame
   (with-selected-frame frame
     (lf/setup-locale-and-ime))))

;; Prevent accidental exit-recursive-edit noise on macOS terminal JIS keys
(when (and (eq system-type 'darwin)
           (not (display-graphic-p)))
  (define-key global-map (kbd "C-M-c") #'ignore))

;; do not make lock, backup files
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq delete-auto-save-files t)

;; underline for current line
(setq hl-line-face 'underline)
(global-hl-line-mode)

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
;;(global-auto-complete-mode t)

;; enable auto-complete even in text mode
;;(add-to-list 'ac-modes 'text-mode)

;; triger auto-complete with tab
;;(ac-set-trigger-key "TAB")

;; enable to select prediction with C-p and C-n
;;(setq ac-use-menu-map t)

;; enable fuzzy auto complete
;;(setq ac-use-fuzzy t) 

;; toggle global line numbers with C-c n
(define-key global-map (kbd "C-c n") #'global-display-line-numbers-mode)

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
 '(custom-safe-themes
   '("669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626"
     default))
 '(package-selected-packages
   '(anzu auto-complete browse-kill-ring coffee-mode dockerfile-mode esup
          exec-path-from-shell flycheck fuzzy go-mode gradle-mode
          groovy-mode helm-git-grep helm-gtags helm-projectile
          helm-pydoc highlight-symbol json-mode magit markdown-mode
          moccur-edit mozc nyan-mode point-undo python-mode scala-mode
          undohist yaml-mode yasnippet)))

(add-to-list 'auto-mode-alist '("\.gradle\'" .groovy-mode))

;; for multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c w") 'mc/mark-all-symbols-like-this)


(autoload 'blank-mode           "blank-mode" "Toggle blank visualization."        t)
(autoload 'blank-toggle-options "blank-mode" "Toggle local `blank-mode' options." t)

;;
;; whitespace
;;
;; (require 'whitespace)
;; (setq whitespace-style '(face 
;;                          trailing
;;                          tabs
;; ;;                         empty
;;                          space-mark
;;                          tab-mark
;;                          ))

;; (setq whitespace-display-mappings
;;       '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

;; (global-whitespace-mode 1 )

(defun powerline-my-theme ()
  "Setup the my mode-line."
  (interactive)
  (setq powerline-current-separator 'utf-8)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'mode-line-1-fg 'mode-line-2-fg))
                          (face2 (if active 'mode-line-1-arrow 'mode-line-2-arrow))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (lhs (list (powerline-raw " " face1)
                                     (powerline-major-mode face1)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-buffer-id nil )
                                     (powerline-raw " [ ")
                                     (powerline-raw mode-line-mule-info nil)
                                     (powerline-raw "%*")
                                     (powerline-raw " |")
                                     (powerline-process nil)
                                     (powerline-vc)
                                     (powerline-raw " ]")
                                     ))
                          (rhs (list (powerline-raw "%4l")
                                     (powerline-raw ":")
                                     (powerline-raw "%2c")
                                     (powerline-raw " | ")                                  
                                     (powerline-raw "%6p")
                                     (powerline-raw " ")
                                     )))
                     (concat (powerline-render lhs)
                             (powerline-fill nil (powerline-width rhs)) 
                             (powerline-render rhs)))))))

(defun make/set-face (face-name fg-color bg-color weight)
  (make-face face-name)
  (set-face-attribute face-name nil
                      :foreground fg-color :background bg-color :box nil :weight weight))
(make/set-face 'mode-line-1-fg "#282C34" "#EF8300" 'bold)
(make/set-face 'mode-line-2-fg "#AAAAAA" "#2F343D" 'bold)
(make/set-face 'mode-line-1-arrow  "#AAAAAA" "#3E4451" 'bold)
(make/set-face 'mode-line-2-arrow  "#AAAAAA" "#3E4451" 'bold)

(powerline-my-theme)
;(load-theme 'atom-one-dark t)
