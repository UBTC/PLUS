;; !/usr/bin/env emacs
;; -*- coding:utf-8 -*-

;; PULSE  """""""""""""""""""""""""""""""""""""""""""""""""""""""
;;
;; PULSE --- m.w.'s Emacs configurations (3G)
;;
;; Based on Aaron Bedra's emacs.d
;;     https://github.com/abedra/emacs.d
;; Steve Purcell's emacs.d and Bin Chen's fork
;;     https://github.com/purcell/emacs.d
;;     https://github.com/redguardtoo/emacs.d
;; and a lot of  other internet resources...
;;
;; COPYRIGHT, Mogei Wang, 2010-2016
;; https://github.com/ubtc/PULSE
;;
;; """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


;;----------------------------------------------------------------------------
;; Environment
;;----------------------------------------------------------------------------
;; Emacs will normally pick user infomation up automatically, but
;; this way I can be sure the right information is always present.
(setq user-full-name "Mogei Wang")
(setq user-mail-address "mogeiwang@gmail.com")

;; There are plenty of things installed outside of the default PATH.
;; This allows me to establish additional PATH information.
(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/usr/bin:/bin" (getenv "PATH")))
(setenv "GOPATH" (concat (getenv "HOME") "/goWork"))
(add-to-list 'exec-path (concat (getenv "GOPATH") "/bin"))

;; Record the start time
(setq emacs-load-start-time (current-time))
(setq debug-on-error t)

;; Emacs lisp is really only a subset of common lisp, and I need to have some of the
;; additional functionality to make the configuration and its dependencies work properly.
(require 'cl) ; common-lisp always

;; The extra customs
(setq custom-el (expand-file-name "custom.el" user-emacs-directory))
(setq custom-org (expand-file-name "custom.org" user-emacs-directory))

;; Test architectures
(setq *win32* (eq system-type 'windows-nt))
(setq *cygwin* (eq system-type 'cygwin))
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)))
(setq *macbook-pro-support-enabled* t)

;; By default Emacs will initiate garbage-collection every 0.76 MB allocated
;; (gc-cons-threshold == 800000). We increase it to 16MB.
(defun my-optimize-gc (NUM PER)
  (setq-default gc-cons-threshold (* 1024 1024 NUM)
                gc-cons-percentage PER))
(my-optimize-gc 16 0.5)

;; Package Management
;; Emacs 24+ includes the Emacs Lisp Package Archive (ELPA) by default.
;; ELPA doesn’t include everything necessary, extra repositories are added.
(load "package")
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(setq package-archive-enable-alist '(("melpa" deft magit)))

;; Load intsalled packages in site-lisp
(defvar sitelisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path sitelisp-dir)
(if (fboundp 'normal-top-level-add-to-load-path)
    (let (default-directory sitelisp-dir)
      (progn
        (setq load-path
              (append
               (loop for dir in sitelisp-dir
                     unless (string-match "^\\." dir)
                     collecting (expand-file-name dir))
               load-path)))))

;; FUNCTION: Check & install elpa packages
(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version) t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(defvar default-packages '( evil
                            session
                            dired+
                            smex
                            idomenu
                            yasnippet
                            flymake
                            flyspell-lazy
                            company
                            vlf
                            org
                            elpy
                            color-theme
                            monokai-theme
                            browse-kill-ring
                            haskell-mode
                            window-numbering
                            pandoc-mode
                            gnuplot-mode
                            go-mode
                            go-autocomplete
                            go-eldoc
                            ebib
                            markdown-mode
                            ess
                            julia-mode
                            ace-jump-mode
                            auto-complete
                            autopair
                            column-enforce-mode
                            minimap
                            multiple-cursors
                            popup
                            powerline
                            switch-window
                            undo-tree
                            yaml-mode
                            ac-slime
                            writegood-mode
                            deft
                            graphviz-dot-mode
                            marmalade
                            paredit
  ) "Default packages")

(loop for pkg in default-packages collecting(require-package pkg))


;;----------------------------------------------------------------------------
;; Interface
;;----------------------------------------------------------------------------
;; Start-up loads.
(load-library "hideshow")
(autoload 'ivy-read "ivy")
(require 'browse-kill-ring)

;; Since I end up using org-mode most of the time, set the default mode accordingly.
(setq initial-major-mode 'org-mode)

;; A warmly welcome.
(setq-default initial-scratch-message
              (concat ";; Welcome to PULSE powered Emacs, " (or user-login-name "") ". Happy hacking!\n\n"))

;; Effective emacs item 7; no scrollbar, no toolbar (no menubar?)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Text marking, and system clipboard processing.
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Cursor settings, and the current line.
(delete-selection-mode t)
(blink-cursor-mode -1)
(global-hl-line-mode t)
(global-linum-mode t)
(column-number-mode 1)

;; More highlights.
(autoload 'highlight-symbol "highlight-symbol" "" t)
(autoload 'highlight-symbol-next "highlight-symbol" "" t)
(autoload 'highlight-symbol-prev "highlight-symbol" "" t)
(autoload 'highlight-symbol-nav-mode "highlight-symbol" "" t)
(autoload 'highlight-symbol-query-replace "highlight-symbol" "" t)

;; Spacing.
(setq-default line-spacing 0.2)
(setq-default truncate-lines nil)
(setq-default truncate-partial-width-windows nil)
(setq whitespace-style '(tab-mark face trailing space-before-tab space-after-tab))
(global-whitespace-mode t)
(setq-default show-trailing-whitespace t)

;; Set scrolls.
(setq scroll-step 1)
(setq scroll-margin 10)
(setq scroll-conservatively 100)
(setq-default grep-scroll-output t)
(setq-default compilation-scroll-output t)

;; Action echos.
(setq-default case-fold-search t)
(setq-default grep-highlight-matches t)
(setq-default mouse-yank-at-point t)
(setq-default set-mark-command-repeat-pop t)
(setq-default save-interprogram-paste-before-kill t)
(setq tab-always-indent 'complete)
(setq echo-keystrokes 0.1)
(show-paren-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Action modes.
(auto-compression-mode 1)
(icomplete-mode 1)
(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(global-undo-tree-mode)
(setq next-line-add-newlines nil)
(defalias 'list-buffers 'ibuffer)

;; Find-file-in-project (ffip).
(autoload 'find-file-in-project "find-file-in-project" "" t)
(autoload 'find-file-in-project-by-selected "find-file-in-project" "" t)
(autoload 'ffip-get-project-root-directory "find-file-in-project" "" t)

;; Show no preview, no duplicate in killring.
(setq browse-kill-ring-display-duplicates nil)
(setq browse-kill-ring-show-preview nil)
(browse-kill-ring-default-keybindings)

;; Always use gdb-many-windows.
(setq gdb-many-windows t)
(setq gdb-show-main t)

;; Display Settings.
(setq frame-title-format '("%b" " - PULSE powered Emacs"))
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines) (toggle-indicate-empty-lines))
(setq-default buffers-menu-max-size 30)

;; Sub-windows
(require 'window-numbering)
(window-numbering-mode 1)
(winner-mode t)
(windmove-default-keybindings)
(setq-default ediff-split-window-function 'split-window-horizontally)
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

;; Powerline, toolbar.
(powerline-center-theme)
(setq powerline-default-separator 'wave)
(setq-default tooltip-delay 1.5)

;; Time display.
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

;; Indentation.
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)

;; Autosaves.
(setq auto-save-mode t)
(setq version-control t)
(setq delete-old-versions t)
(setq vc-make-backup-files t)
(setq kept-old-versions 5)
(setq kept-new-versions 5)
(setq backup-directory-alist (quote (("" . "~/.emacs.d/backup"))))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Custom sets.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((lentic-init . lentic-orgel-org-init))))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(window-numbering-face ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold))) t))


;;----------------------------------------------------------------------------
;; Miscellaneous key binding stuff
;;----------------------------------------------------------------------------
;; a no-op function to bind to if you want to set a keystroke to null
(defun void () "this is a no-op" (interactive))

(global-set-key (kbd "C-=") 'hs-hide-block)
(global-set-key (kbd "C--") 'hs-show-block)
(global-set-key (kbd "C-+") 'hs-hide-all)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key [C-tab] 'hippie-expand)
(global-set-key [backtab] "\C-q\t") ;; S-tab quotes a tab
;; (global-set-key (kbd "C-x C-a") 'find-function)

;; M-x without meta
;; (global-set-key (kbd "C-x ") 'execute-extended-command)

;; Use regex to search by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(global-set-key (kbd "M-/") 'undo-tree-visualize)
(global-set-key (kbd "C-M-z") 'switch-window)
(global-set-key (kbd "C-,") 'ace-jump-mode)

(global-set-key (kbd "C-}") 'mc/mark-next-like-this)
(global-set-key (kbd "C-{") 'mc/mark-previous-like-this)

;; Fn keys.
(global-set-key [f1]  'split-window-horizontally)
(global-set-key [f2]  'split-window-vertically)
(global-set-key [f3]  'next-multiframe-window)
(global-set-key [f4]  'delete-window)
(global-set-key [f5]  'compile)
(global-set-key [f6]  'ispell-buffer)
(global-set-key [f7]  'comment-or-uncomment-region)
(global-set-key [f8]  'speedbar)
(global-set-key [f9]  'eshell)


;;----------------------------------------------------------------------------
;; Evil related
;;----------------------------------------------------------------------------
(setq evil-symbol-word-search t)
(add-to-list 'load-path "~/.emacs.d/site-lisp/evil/lib")
(setq evil-default-cursor t)
(require 'evil)
(evil-mode 1)

; Make horizontal movement cross lines
(setq-default evil-cross-lines t)

;; Make movement keys work like they should
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;; vimrc
(autoload 'vimrc-mode "vimrc-mode")
(add-to-list 'auto-mode-alist '("\\.?vim\\(rc\\)?$" . vimrc-mode))

;;----------------------------------------------------------------------------
;; Configure session
;;----------------------------------------------------------------------------
;; Load the last edited files
(require 'saveplace)
(setq-default save-place t)

(setq desktop-path '("~/.emacs.d"))
(load "desktop")
(desktop-save-mode)
(setq session-save-file (expand-file-name "~/.emacs.d/.session"))
(add-hook 'after-init-hook 'session-initialize)

;; recentf-mode
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-saved-items 1000
      recentf-exclude '("/tmp/" "/ssh:" "/sudo:" "/home/[a-z]\+/\\."))
(recentf-mode 1)

;; save a bunch of variables to the desktop file:
(setq desktop-globals-to-save
      (append '((extended-command-history . 128)
                (file-name-history        . 128)
                (ido-last-directory-list  . 128)
                (ido-work-directory-list  . 128)
                (ido-work-file-list       . 128)
                (grep-history             . 128)
                (compile-history          . 128)
                (minibuffer-history       . 128)
                (query-replace-history    . 128)
                (read-expression-history  . 128)
                (regexp-history           . 128)
                (regexp-search-ring       . 128)
                (search-ring              . 128)
                (comint-input-ring        . 128)
                (shell-command-history    . 128)
                (evil-ex                  . 128)
                desktop-missing-file-warning
                register-alist)))

;;----------------------------------------------------------------------------
;; Configure dired
;;----------------------------------------------------------------------------
(setq-default dired-details-hidden-string "")
(require 'dired+)
(define-key dired-mode-map "(" 'dired-details-toggle)
(define-key dired-mode-map ")" 'dired-details-toggle)
(define-key dired-mode-map "/" 'dired-isearch-filenames)
(define-key dired-mode-map "\\" 'diredext-exec-git-command-in-shell)
(setq dired-recursive-deletes 'always)
(define-key dired-mode-map [mouse-2] 'dired-find-file)
(dolist (file `(((if *unix* "zathura" "open") "pdf" "dvi" "pdf.gz" "ps" "eps")
                ("unrar x" "rar")
                ((my-guess-mplayer-path) "avi" "mpg" "rmvb" "rm" "flv" "wmv" "mkv" "mp4" "m4v" "webm")
                ((concat (my-guess-mplayer-path) " -playlist") "list" "pls")
                ((if *unix* "feh" "open") "gif" "jpeg" "jpg" "tif" "png" )
                ("7z x" "7z")
                ("djview" "djvu")
                ("firefox" "xml" "xhtml" "html" "htm" "mht")))
(add-to-list 'dired-guess-shell-alist-default
            (list (concat "\\." (regexp-opt (cdr file) t) "$") (car file))))

;;----------------------------------------------------------------------------
;; Org Settings
;;----------------------------------------------------------------------------
(require 'org)
(require 'ob)
(require 'org-install)
(require 'org-habit)
(require 'ob-tangle)

;; Enable logging when tasks are complete.
(setq org-log-done t
      org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))
(add-hook 'org-mode-hook (lambda () (flyspell-mode)))
(add-hook 'org-mode-hook (lambda () (writegood-mode)))

;; Config org-agenda
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-show-log t
      org-agenda-todo-ignore-scheduled t
      org-agenda-todo-ignore-deadlines t)
;; Fuck the GFW to use Dropbox
(setq org-agenda-files (list "~/Dropbox/org/personal.org"
                             "~/Dropbox/org/groupons.org"))

(add-to-list 'org-modules "org-habit")
(setq org-habit-preceding-days 7
      org-habit-following-days 1
      org-habit-graph-column 80
      org-habit-show-habits-only-for-today t
      org-habit-show-all-today t)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t
     org-completion-use-ido t
     org-edit-src-content-indentation 0
     org-edit-timestamp-down-means-later t
     org-agenda-start-on-weekday nil
     org-agenda-span 14
     org-agenda-include-diary t
     org-agenda-window-setup 'current-window
     org-fast-tag-selection-single-key 'expert
     org-export-kill-product-buffer-when-displayed t
     ;; org v7
     org-export-odt-preferred-output-format "doc"
     ;; org v8
     org-odt-preferred-output-format "doc"
     org-tags-column 80
     ;; org-startup-indented t
     ;; org 8.2.6 has some performance issue. Here is the workaround.
     ;; http://punchagan.muse-amuse.in/posts/how-i-learnt-to-use-emacs-profiler.html
     org-agenda-inhibit-startup t ;; ~50x speedup
     org-agenda-use-tag-inheritance nil ;; 3-4x speedup
     )
;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; babel-exec
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (fortran . t)
   (gnuplot . t)
   (C . t)
   (R . t)
   (latex . t)
   (java . t)
   (js . t)
   (lisp . t)
   (emacs-lisp . t)
   (matlab . t)
   (octave . t)
   (sh . t)
   (perl . t)
   (ruby . t)
   ))

;; No prompts when runing codes
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'org-display-inline-images)
(setq org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

(add-hook 'org-babel-after-execute-hook
          (lambda () (condition-case nil (org-display-inline-images) (error nil)))
          'append)

(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

;;----------------------------------------------------------------------------
;; Deft
;;----------------------------------------------------------------------------
(setq deft-directory "~/Dropbox/deft")
(setq deft-use-filename-as-title t)
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)

;;----------------------------------------------------------------------------
;; Smex
;;----------------------------------------------------------------------------
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-Z") 'smex-major-mode-commands)

;;----------------------------------------------------------------------------
;; Ido
;;----------------------------------------------------------------------------
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-virtual-buffers t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq ido-use-faces nil)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length 0)
;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)

;;----------------------------------------------------------------------------
;; Autopair
;;----------------------------------------------------------------------------
(require 'autopair)
(autopair-global-mode)

;;----------------------------------------------------------------------------
;; Configure lisp
;;----------------------------------------------------------------------------
(require 'lisp-mode)
(setq lisp-modes '(lisp-mode emacs-lisp-mode common-lisp-mode scheme-mode))

(defvar lisp-power-map (make-keymap))
(define-minor-mode lisp-power-mode "Fix keybindings; add power."
  :lighter " (power)"
  :keymap lisp-power-map
  (paredit-mode t))
(define-key lisp-power-map [delete] 'paredit-forward-delete)
(define-key lisp-power-map [backspace] 'paredit-backward-delete)
(defun engage-lisp-power () (lisp-power-mode t))

(dolist (mode lisp-modes) (add-hook (intern (format "%s-hook" mode)) #'engage-lisp-power))

(setq inferior-lisp-program "clisp")
(setq scheme-program-name "racket")

(let* ((lispy-hooks '(lisp-mode-hook inferior-lisp-mode-hook lisp-interaction-mode-hook))))

(turn-on-eldoc-mode)
(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))


;;----------------------------------------------------------------------------
;; Auto-complete
;;----------------------------------------------------------------------------
(require 'auto-complete-config)
(ac-config-default)

(require 'company)
(add-hook 'prog-mode-hook 'global-company-mode)
(add-hook 'cmake-mode-hook 'global-company-mode)
(if (fboundp 'evil-declare-change-repeat)
    (mapc #'evil-declare-change-repeat
          '(company-complete-common
            company-select-next
            company-select-previous
            company-complete-selection
            company-complete-number
            )))

;;----------------------------------------------------------------------------
;; Indentation and buffer cleanup
;;----------------------------------------------------------------------------
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

;; Clear buffer
(put 'erase-buffer 'disabled nil)
(defalias 'clear 'erase-buffer)

;;----------------------------------------------------------------------------
;; Configure misc modes
;;----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.bash_profile\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash_history\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.fish\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc.local\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))

;; yaml
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (writegood-mode t)
            (flyspell-mode t)))
(setq markdown-command "pandoc --smart -f markdown -t pdf")
(autoload 'markdown-mode "markdown-mode" "Mode for editing Markdown documents" t)

;;----------------------------------------------------------------------------
;; Configure VLF
;;----------------------------------------------------------------------------
(require 'vlf)
(setq vlf-batch-size 10000000)

(defun vlf-extract-part-of-file (file from to)
  "returns bytes in file from from to to."
  (let ((size (vlf-file-size file)))
    (if (or (> from size)
            (> to size))
        (error "from or to is larger that the file size"))
    (with-temp-buffer
      (shell-command
       (format "head --bytes %d %s | tail --bytes %d"
           to file (+ (- to from) 1)) t)
      (buffer-substring (point-min) (point-max)))))

;;----------------------------------------------------------------------------
;; Configure terminal
;;----------------------------------------------------------------------------
;; http://stackoverflow.com/questions/2886184/copy-paste-in-emacs-ansi-term-shell/2886539#2886539
(defun ash-term-hooks ()
  ;; dabbrev-expand in term
  (define-key term-raw-escape-map "/"
    (lambda ()
      (interactive)
      (let ((beg (point)))
        (dabbrev-expand nil)
        (kill-region beg (point)))
      (term-send-raw-string (substring-no-properties (current-kill 0)))))
  ;; yank in term (bound to C-c C-y)
  (define-key term-raw-escape-map "\C-y"
    (lambda ()
      (interactive)
      (term-send-raw-string (current-kill 0)))))
(add-hook 'term-mode-hook 'ash-term-hooks)

;; http://emacs-journey.blogspot.com.au/2012/06/improving-ansi-term.html
;; kill the buffer when terminal is exited
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; always use bash
(defvar my-term-shell "/bin/zsh")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; utf8
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)

(shell)

;;----------------------------------------------------------------------------
;; Configure python
;;----------------------------------------------------------------------------
(autoload 'doctest-mode "doctest-mode" "Python doctest editing mode." t)
(setq interpreter-mode-alist (cons '("python" . python-mode) interpreter-mode-alist))
(defun python-mode-hook-setup ()
  (unless (is-buffer-file-temp)
    ;; run command `pip install jedi flake8 importmagic` in shell,
    ;; or just check https://github.com/jorgenschaefer/elpy
    (elpy-mode 1)
    ;; http://emacs.stackexchange.com/questions/3322/python-auto-indent-problem/3338#3338
    ;; emacs 24.4 only
    (setq electric-indent-chars (delq ?: electric-indent-chars))
    ))
(add-hook 'python-mode-hook 'python-mode-hook-setup)

;;----------------------------------------------------------------------------
;; Configure golang...
;;----------------------------------------------------------------------------
;; install package
(require 'go-mode)
(require 'go-autocomplete)

(defun go-mode-create-imenu-index ()
"Create and return an imenu index alist. Unlike the default
alist created by go-mode, this method creates an alist where
items follow a style that is consistent with other prog-modes."
  (let* ((patterns '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)))
         (type-index (imenu--generic-function patterns))
         (func-index))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward go-func-meth-regexp (point-max) t)
        (let* ((var (match-string-no-properties 1))
               (func (match-string-no-properties 2))
               (name (if var
                         (concat (substring var 0 -1) "." func)
                       func))
               (beg (match-beginning 0))
               (marker (copy-marker beg))
               (item (cons name marker)))
          (setq func-index (cons item func-index)))))
    (nconc type-index (list (cons "func" func-index)))))

(defun go-mode-create-flat-imenu-index ()
  "Return a flat imenu index alist. See `go-mode-create-imenu-index'."
  (apply 'nconc (mapcar 'cdr (go-mode-create-imenu-index))))

(add-hook 'go-mode-hook
          (lambda ()
            (go-eldoc-setup)
            ;; C-c p runs gofmt on the buffer
            (define-key go-mode-map (kbd "C-c p") 'gofmt)
            ;; adjust fill-column
            (setq-local fill-column 120)
            ;; use flat imenu index
            (setq-local imenu-create-index-function
                        #'go-mode-create-flat-imenu-index)))

;; run gofmt before saving file
(add-hook 'before-save-hook 'gofmt-before-save)

;; use goimports if available
(when (executable-find "goimports") (setq gofmt-command "goimports"))

;;----------------------------------------------------------------------------
;; Configure ESS and Julia
;;----------------------------------------------------------------------------
(require 'ess-site)
(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . R-mode))
;; https://github.com/kyleam/emacs.d/blob/master/lisp/init-ess.el

(setq ess-smart-S-assign-key ";")
(setq ess-use-ido nil)

(define-abbrev-table 'ess-mode-abbrev-table
  '(("true" "TRUE")
    ("false" "FALSE"))
  :system t)

(dolist (hook '(ess-mode-hook inferior-ess-mode-hook))
  (add-hook hook (lambda ()
                   (setq local-abbrev-table ess-mode-abbrev-table)))
  (add-hook hook 'abbrev-mode))

;; (require 'julia-shell)
(defun my-julia-mode-hooks () (require 'julia-shell-mode))
(add-hook 'julia-mode-hook 'my-julia-mode-hooks)
;; (define-key julia-mode-map (kbd "C-c C-c") 'julia-shell-run-region-or-line)
;; (define-key julia-mode-map (kbd "C-c C-s") 'julia-shell-save-and-go)

;;----------------------------------------------------------------------------
;; Configure latex...
;;----------------------------------------------------------------------------
;;; LaTeX
;; https://github.com/CestDiego/.emacs.d/blob/master/user-lisp/setup-latex.el
;; https://github.com/xyguo/emacs.d/blob/master/lisp/init-auctex.el
;; https://github.com/xiaohanyu/oh-my-emacs/blob/master/modules/ome-tex.org

;; (add-to-list 'load-path "~/.emacs.d/lisp/auctex")
;; (load "~/.emacs.d/site-lisp/auctex/auctex.el" nil t t)
;; (when (locate-library "auctex") (setq reftex-plug-into-AUCTeX t))

(global-set-key "\C-cb" 'ebib)

(setq TeX-default-mode 'LaTeX-mode) ; Default mode for .tex files
(setq TeX-force-default-mode t) ; Force This mode Always, it is MANDATORY for my sake
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ; Activar reftex con AucTeX
;; (setq reftex-plug-into-AUCTeX t)
;; (setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))

;; Set Up flyspell-babel and ispell-multi
(autoload 'flyspell-babel-setup "flyspell-babel")
(add-hook 'latex-mode-hook 'flyspell-babel-setup)
(add-hook 'LaTeX-mode-hook (lambda () (set-input-method "latin-1-prefix")))
(add-hook 'LaTeX-mode-hook (lambda () (flyspell-mode 1)))

;; AutoSaving and parsing for some other Tex Packages
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; This Makes using include or input easier
(setq-default TeX-master nil)

(mapc (lambda (mode)
      (add-hook 'LaTeX-mode-hook mode))
      (list 'auto-fill-mode
            'LaTeX-math-mode
            'turn-on-reftex
            'TeX-fold-mode
            'linum-mode
            'visual-line-mode
            'flyspell-mode
            'auto-complete-mode
            'autopair-mode
            'TeX-source-correlate-mode
            'outline-minor-mode))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq TeX-auto-untabify t     ; remove all tabs before saving
                  TeX-engine 'xetex       ; use xelatex default
                  TeX-show-compilation nil) ; display compilation windows
            (TeX-global-PDF-mode t)       ; PDF mode enable, not plain
            (setq TeX-save-query nil)
            (imenu-add-menubar-index)
            ;;(define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)
            ))

;; Mac OS X fallback to the "open" program as the default viewer for all types of files.
(cond
 ;; settings for Linux
 (*linux*
  (cond
   ((executable-find "okular")
    (setq TeX-view-program-selection
          '((output-pdf "Okular")
            (output-dvi "Okular"))))
   ((executable-find "evince")
    (setq TeX-view-program-selection
          '((output-pdf "Evince")
            (output-dvi "Evince"))))
   ((executable-find "zathura")
    (setq TeX-view-program-selection
          '((output-pdf "zathura")
            (output-dvi "zathura"))))
   ((executable-find "mupdf")
    (setq TeX-view-program-selection
          '((output-pdf "mupdf")
            (output-dvi "mupdf"))))
   (t
    (setq TeX-view-program-selection
          '((output-pdf "xdg-open")
            (output-dvi "xdg-open")))))
   ))

;; configuration for TeX-fold-mode
;; add entries you want to be fold, or comment that needn't to be fold.
(setq TeX-fold-env-spec-list
      (quote (("[figure]" ("figure"))
              ("[table]" ("table"))
              ("[itemize]" ("itemize"))
              ("[description]" ("description"))
              ("[tabular]" ("tabular"))
              ("[frame]" ("frame"))
              ("[array]" ("array"))
              ("[code]" ("lstlisting"))
              ("[eqnarray]" ("eqnarray"))
              )))

;; To turn on CDLaTeX Minor Mode for all TeX files with AUCTeX LaTeX mode
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
(setq cdlatex-env-alist
   '(("axiom" "\\begin{axiom}\nAUTOLABEL\n?\n\\end{axiom}\n" nil)
     ("theorem" "\\begin{theorem}\nAUTOLABEL\n?\n\\end{theorem}\n" nil)))

(setq cdlatex-command-alist
   '(("axm" "Insert axiom env"   "" cdlatex-environment ("axiom") t nil)
     ("thr" "Insert theorem env" "" cdlatex-environment ("theorem") t nil)))

(setq cdlatex-math-symbol-alist
   '((?< ("\\leftarrow" "\\Leftarrow" "\\longleftarrow" "\\Longleftarrow"))
     (?> ("\\rightarrow" "\\Rightarrow" "\\longrightarrow" "\\Longrightarrow"))))

;; insert latex math symbols
;; (global-set-key (kbd "C-c m") 'latex-math-preview-insert-mathematical-symbol)

;;----------------------------------------------------------------------------
;; Configure pandoc mode
;;----------------------------------------------------------------------------
(add-hook 'markdown-mode-hook 'conditionally-turn-on-pandoc)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

;;----------------------------------------------------------------------------
;; Configure gnuplot...
;;----------------------------------------------------------------------------
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

(setq gnuplot-basic-offset 2)
(setq gnuplot-context-sensitive-mode t)
(setq gnuplot-inline-imge-mode t)

;;----------------------------------------------------------------------------
;; Configure haskell
;;----------------------------------------------------------------------------
(require 'haskell-mode)
(setq haskell-font-lock-symbols t)
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs\\'" . literate-haskell-mode))
(add-hook 'haskell-mode-hook
          (lambda ()
            (turn-on-haskell-doc-mode)
            (turn-on-haskell-indent)))

;;----------------------------------------------------------------------------
;; Configure yasnippet
;;----------------------------------------------------------------------------
(require 'yasnippet)
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)

;;----------------------------------------------------------------------------
;; Configure flymake
;;----------------------------------------------------------------------------
(require 'flymake)
;; I want to see at most the first 4 errors for a line
(setq flymake-number-of-errors-to-display 4)
;; Let's run 2 checks at once instead.
(setq flymake-max-parallel-syntax-checks 2)
(setq flymake-gui-warnings-enabled nil)

;;----------------------------------------------------------------------------
;; Configure flyspell
;;----------------------------------------------------------------------------
(require 'flyspell-lazy)
(flyspell-mode 1)
(flyspell-prog-mode)
(flyspell-lazy-mode 1)
;; better performance:
(setq flyspell-issue-message-flag nil)
(setq-default ispell-list-command "list")

(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  ;; just reset dictionary to the safe one "en_US" for hunspell.
  ;; if we need use different dictionary, we specify it in command line arguments
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
 (t (setq ispell-program-name nil)
    (message "You need install either aspell or hunspell for ispell")))

;; ispell-cmd-args is useless, it's the list of *extra* command line arguments
;; we will append to the ispell process when ispell-send-string()
;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)))

(defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)))

;;----------------------------------------------------------------------------
;; Color and theme things
;;----------------------------------------------------------------------------
(require 'color-theme)
(require 'monokai-theme)
;; work around color theme bug
;; https://plus.google.com/106672400078851000780/posts/KhTgscKE8PM
(defadvice load-theme (before disable-themes-first activate)
  ;; diable all themes
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;;----------------------------------------------------------------------------
;; Language and location
;;----------------------------------------------------------------------------
;; configure locales (setting them too early doesn't work in X)
(defun sanityinc/utf8-locale-p (v)
  "Return whether locale string V relates to a utf-8 locale."
  (and v (string-match "utf-8" v)))

(defun locale-is-utf8-p ()
  "Return t if the \"locale\" command or environment variables prefer utf-8."
  (or (sanityinc/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (sanityinc/utf8-locale-p (getenv "LC_ALL"))
      (sanityinc/utf8-locale-p (getenv "LC_CTYPE"))
      (sanityinc/utf8-locale-p (getenv "LANG"))))

(when (or window-system (locale-is-utf8-p))
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (unless (eq system-type 'windows-nt)
    (set-selection-coding-system 'utf-8))
  (prefer-coding-system 'utf-8))


;;----------------------------------------------------------------------------
;; Appendix
;;----------------------------------------------------------------------------
;; The extra customs will be autoloaded.
(when (and (not (file-exists-p custom-el)) (file-exists-p custom-org)) (org-babel-tangle-file custom-org))
(when (file-exists-p custom-el) (load custom-el))

;; All done
(when (require 'time-date nil t) (message "Emacs startup time: %d seconds." (time-to-seconds (time-since emacs-load-start-time))))
(message "\nInitialization finished. Welcome to PULSE powered %s, %s!\n" (invocation-name) (user-login-name))
