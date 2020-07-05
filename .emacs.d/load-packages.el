(require 'cl)

;;
;;add package
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defvar required-packages
  '(
    anzu
    auto-complete
    coffee-mode
    color-moccur
    dockerfile-mode
    exec-path-from-shell
    fuzzy
    flycheck
    go-mode
    gradle-mode
    groovy-mode
    helm
    helm-gtags
    helm-projectile
    helm-pydoc
    helm-git-grep
    highlight-symbol
    json-mode
    browse-kill-ring
    magit
    markdown-mode
;;    moccur-edit
    nyan-mode
    python-mode
    scala-mode
    undohist
    yaml-mode
    yasnippet
    multiple-cursors
    powerline
    atom-one-dark-theme
    ))

(defun is-all-packages-installed ()
  (loop for p in required-packages
    when (not (package-installed-p p)) do (return nil)
      finally (return t)))

(unless (is-all-packages-installed)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))
