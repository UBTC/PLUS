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
;; User infomation
(setq user-full-name "Mogei Wang")
(setq user-mail-address "mogeiwang@gmail.com")

;; Pathes
(setenv "PATH" (concat "/usr/local/bin:/opt:/usr/bin:/bin" (getenv "PATH")))
;; (setenv "GOPATH" (concat (getenv "HOME") "/goWork"))
;; (add-to-list 'exec-path (concat (getenv "GOPATH") "/bin"))

;; Architectures related
(setq *win32* (eq system-type 'windows-nt))
(setq *cygwin* (eq system-type 'cygwin))
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)))
(setq *macbook-pro-support-enabled* t)


;;----------------------------------------------------------------------------
;; Default globals
;;----------------------------------------------------------------------------
;; Common-lisp always
(require 'cl)

;; Since I end up using org-mode most of the time, set the default mode accordingly.
(setq initial-major-mode 'org-mode)

;; Text marking
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Record the start time
(require 'time-date nil t)
(setq emacs-load-start-time (current-time))
(setq debug-on-error t)

;; Increase garbage-collection threshold
(setq-default gc-cons-threshold (* 1024 1024 16))
(setq-default gc-cons-percentage 0.5)

;; WWW
(setq browse-url-mozilla-program "firefox")

;; UTF
(set-language-environment "utf-8")


;;----------------------------------------------------------------------------
;; Functions
;;----------------------------------------------------------------------------
;; Check & install elpa packages
(defun check-elpa-packages (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version) t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (check-elpa-packages package min-version t)))))

;; Use inner packages packed in lists
(defun load-package-layer (package-list)
  (loop for pkg in package-list
    collecting(require pkg)))

;; a no-op function to bind to if you want to set a keystroke to null
(defun void () "this is a no-op" (interactive))


;;----------------------------------------------------------------------------
;; Package Management
;;----------------------------------------------------------------------------
(defvar package-packages '(package) "Package list for import more packages")
(load-package-layer package-packages)

(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(setq package-archive-enable-alist '(("melpa" deft magit)))

(defvar my-elpa-packages '(evil
                           session
                           dired+
                           smex
                           flymake
                           flyspell-lazy
                           company
                           vlf
                           org
                           elpy
                           color-theme
                           monokai-theme
                           browse-kill-ring
                           ebib
                           ess
                           julia-mode
                           ace-jump-mode
                           auto-complete
                           multiple-cursors
                           popup
                           powerline
                           ac-slime
                           writegood-mode
                           paredit
  ) "ELPA packages")

(loop for pkg in my-elpa-packages
      collecting(check-elpa-packages pkg))


;;----------------------------------------------------------------------------
;; Site packages
;;----------------------------------------------------------------------------
;; Local packages are in site-lisp
(defvar sitelisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path sitelisp-dir)

;; Load site packages
(if (fboundp 'normal-top-level-add-to-load-path)
    (let (default-directory sitelisp-dir)
      (progn
        (setq load-path
              (append
               (loop for dir in sitelisp-dir
                     unless (string-match "^\\." dir)
                     collecting (expand-file-name dir))
               load-path)))))


;;----------------------------------------------------------------------------
;; Cursor settings
;;----------------------------------------------------------------------------
(delete-selection-mode t)
(blink-cursor-mode -1)

;; Multicursor
(global-set-key (kbd "C-}") 'mc/mark-next-like-this)
(global-set-key (kbd "C-{") 'mc/mark-previous-like-this)


;;----------------------------------------------------------------------------
;; Hint
;;----------------------------------------------------------------------------
;; Pair
(show-paren-mode t)

;; Numbering
(global-linum-mode t)
(column-number-mode 1)

;; Time display
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)


;;----------------------------------------------------------------------------
;; Highlight
;;----------------------------------------------------------------------------
(setq-default grep-highlight-matches t)
(autoload 'highlight-symbol "highlight-symbol" "" t)
(autoload 'highlight-symbol-next "highlight-symbol" "" t)
(autoload 'highlight-symbol-prev "highlight-symbol" "" t)
(autoload 'highlight-symbol-nav-mode "highlight-symbol" "" t)
(autoload 'highlight-symbol-query-replace "highlight-symbol" "" t)
(global-hl-line-mode t)


;;----------------------------------------------------------------------------
;; Indentation
;;----------------------------------------------------------------------------
(setq tab-width 4)
(setq default-tab-width 4)
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines) (toggle-indicate-empty-lines))
(setq next-line-add-newlines nil)
(global-set-key (kbd "RET") 'newline-and-indent)


;;----------------------------------------------------------------------------
;; Fold
;;----------------------------------------------------------------------------
(load-library "hideshow")
(setq-default case-fold-search t)
(global-set-key (kbd "C-+") 'hs-show-block)
(global-set-key (kbd "C--") 'hs-hide-block)
(global-set-key (kbd "C-=") 'hs-hide-all)


;;----------------------------------------------------------------------------
;; Kill ring
;;----------------------------------------------------------------------------
(setq kill-ring-max 3000)
(setq undo-limit 5000000)
(defvar killring-packages '(browse-kill-ring) "Package list for browse killring")
(load-package-layer killring-packages)
(browse-kill-ring-default-keybindings)
(setq-default mouse-yank-at-point t)
(setq-default save-interprogram-paste-before-kill t)
(setq browse-kill-ring-display-duplicates nil)
(setq browse-kill-ring-show-preview nil)
(browse-kill-ring-default-keybindings)


;;----------------------------------------------------------------------------
;; Spacing
;;----------------------------------------------------------------------------
(setq-default line-spacing 0.2)
(setq-default truncate-lines nil)
(setq-default truncate-partial-width-windows nil)
(setq whitespace-style '(tab-mark face trailing space-before-tab space-after-tab))
(global-whitespace-mode t)
(setq-default show-trailing-whitespace t)


;;----------------------------------------------------------------------------
;; GUI
;;----------------------------------------------------------------------------
;; Window frame
(setq frame-title-format '("%b" " - PULSE powered Emacs"))
(setq-default buffers-menu-max-size 30)

;; Bars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(powerline-center-theme)
(setq powerline-default-separator 'wave)
(setq-default tooltip-delay 1.5)
(setq scroll-step 1)
(setq scroll-margin 5)
(setq scroll-conservatively 100)
(setq-default compilation-scroll-output t)


;;----------------------------------------------------------------------------
;; Jump
;;----------------------------------------------------------------------------
(global-set-key (kbd "C->") 'ace-jump-mode)
(global-set-key (kbd "C-<") 'find-function)

;; Search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)


;;----------------------------------------------------------------------------
;; Miscellaneous key bindings
;;----------------------------------------------------------------------------
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
;; Vim related
;;----------------------------------------------------------------------------
(defvar vim-packages '(evil) "Package list for vim")
(load-package-layer vim-packages)

(setq evil-symbol-word-search t)
(setq evil-default-cursor t)
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
(add-to-list 'auto-mode-alist '("~/.config/nvim/init.vim" . vimrc-mode))


;;----------------------------------------------------------------------------
;; Directories op
;;----------------------------------------------------------------------------
(defvar dir-packages '(dired+) "Package list for directories")
(load-package-layer dir-packages)

(setq-default dired-details-hidden-string "")
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
;; Minibuffer
;;----------------------------------------------------------------------------
(defvar minibuffer-packages '(ido) "Package list for minibuffer enhancement")
(load-package-layer minibuffer-packages)

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

;; Smex
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-Z") 'smex-major-mode-commands)


;;----------------------------------------------------------------------------
;; Autocomplete
;;----------------------------------------------------------------------------
(defvar autocomplete-packages '(company) "Package list for autocomplete")
(load-package-layer autocomplete-packages)

(icomplete-mode 1)
(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case t)

(ac-config-default)
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

(global-set-key [C-tab] 'hippie-expand)
(global-set-key [backtab] "\C-q\t") ;; S-tab quotes a tab


;;----------------------------------------------------------------------------
;; File
;;----------------------------------------------------------------------------
(defvar file-packages '(vlf) "Package list for file op.")
(load-package-layer file-packages)
(setq vlf-batch-size 10000000)

;; Find-file-in-project (ffip).
(autoload 'find-file-in-project "find-file-in-project" "" t)
(autoload 'find-file-in-project-by-selected "find-file-in-project" "" t)
(autoload 'ffip-get-project-root-directory "find-file-in-project" "" t)

;; Autosaves
(setq auto-save-mode t)
(setq version-control t)
(setq delete-old-versions t)
(setq vc-make-backup-files t)
(setq kept-old-versions 5)
(setq kept-new-versions 5)


;;----------------------------------------------------------------------------
;; Buffer
;;----------------------------------------------------------------------------
(setq read-file-name-completion-ignore-case t)
(defalias 'list-buffers 'ibuffer)


;;----------------------------------------------------------------------------
;; Terminal
;;----------------------------------------------------------------------------
;; Set shell
(defvar my-term-shell "/bin/zsh")
(defadvice ansi-term (before force-bash) (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; utf8
(defun my-term-use-utf8 () (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)


;;----------------------------------------------------------------------------
;; Syntax
;;----------------------------------------------------------------------------
(defvar syntax-packages '(flymake) "Package list for syntax check")
(load-package-layer syntax-packages)

;; To see at most the first 3 errors for a line
(setq flymake-number-of-errors-to-display 3)

;; Let's run 2 checks at once
(setq flymake-max-parallel-syntax-checks 2)
(setq flymake-gui-warnings-enabled nil)

;; Debug
(setq gdb-many-windows t)
(setq gdb-show-main t)


;;----------------------------------------------------------------------------
;; Spell
;;----------------------------------------------------------------------------
(defvar spell-packages '(flyspell-lazy) "Package list for spell check")
(load-package-layer spell-packages)

(flyspell-mode 1)
(flyspell-prog-mode)
(flyspell-lazy-mode 1)
(setq flyspell-issue-message-flag nil)
(setq-default ispell-list-command "list")

(cond
 ((executable-find "aspell") (setq ispell-program-name "aspell"))
 ((executable-find "hunspell") (setq ispell-program-name "hunspell")
   ;; just reset dictionary to the safe one "en_US" for hunspell.
   ;; if we need use different dictionary, we specify it in command line arguments
   (setq ispell-local-dictionary "en_US")
   (setq ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
 (t (setq ispell-program-name nil) (message "You need install either aspell or hunspell for ispell")))

;; ispell-cmd-args is useless, it's the list of *extra* command line arguments
;; we will append to the ispell process when ispell-send-string()
;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use Emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)))

(defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use Emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)))


;;----------------------------------------------------------------------------
;; Theme things
;;----------------------------------------------------------------------------
(defvar theme-packages '(monokai-theme
                         color-theme
                         ) "Package list for themes")
(load-package-layer theme-packages)

;; Custom sets
(custom-set-variables
 '(safe-local-variable-values (quote ((lentic-init . lentic-orgel-org-init))))
 '(session-use-package t nil (session)))
(custom-set-faces
 '(window-numbering-face ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold))) t))


;;----------------------------------------------------------------------------
;; Julia
;;----------------------------------------------------------------------------
(defvar julia-packages '(ess-site
                         julia-mode
                         ;; julia-shell
                         ;; julia-shell-mode
                         ) "Package list for Julia")
(load-package-layer julia-packages)

;; (define-key julia-mode-map (kbd "C-c C-c") 'julia-shell-run-region-or-line)
;; (define-key julia-mode-map (kbd "C-c C-s") 'julia-shell-save-and-go)


;;----------------------------------------------------------------------------
;; Org Settings
;;----------------------------------------------------------------------------
(defvar orgmode-packages '(org
                           ob
                           org-install
                           org-habit
                           ob-tangle
                           ) "Package list for orgmode")
(load-package-layer orgmode-packages)

;; Enable logging when tasks are complete.
(setq org-log-done t)
(setq org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE")))
(setq org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))
(add-hook 'org-mode-hook (lambda () (flyspell-mode)))
(add-hook 'org-mode-hook (lambda () (writegood-mode)))

;; Config org-agenda
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-show-log t)
(setq org-agenda-todo-ignore-scheduled t)
(setq org-agenda-todo-ignore-deadlines t)

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
   )) ;; (julit . t) does not work well, tooo bad

;; No prompts when runing codes
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'org-display-inline-images)
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)

(add-hook 'org-babel-after-execute-hook
          (lambda () (condition-case nil (org-display-inline-images) (error nil)))
          'append)

(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))


;;----------------------------------------------------------------------------
;; LaTeX
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
            (setq TeX-auto-untabify t)      ; remove all tabs before saving
            (setq TeX-engine 'xetex)        ; use xelatex default
            (setq TeX-show-compilation nil) ; display compilation windows
            (TeX-global-PDF-mode t)         ; PDF mode enable, not plain
            (setq TeX-save-query nil)
            (imenu-add-menubar-index)
            ;;(define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)
            ))

;; Mac OS X fallback to the "open" program as the default viewer for all types of files.
(cond
 ;; settings for Linux
 (*linux*
  (cond
   ((executable-find "okular") (setq TeX-view-program-selection '((output-pdf "Okular") (output-dvi "Okular"))))
   ((executable-find "evince") (setq TeX-view-program-selection '((output-pdf "Evince") (output-dvi "Evince"))))
   ((executable-find "zathura") (setq TeX-view-program-selection '((output-pdf "zathura") (output-dvi "zathura"))))
   ((executable-find "mupdf") (setq TeX-view-program-selection '((output-pdf "mupdf") (output-dvi "mupdf"))))
   (t (setq TeX-view-program-selection '((output-pdf "xdg-open") (output-dvi "xdg-open")))))
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


;;----------------------------------------------------------------------------
;; External configuration
;;----------------------------------------------------------------------------
(setq custom-el (expand-file-name "custom.el" user-emacs-directory))
(setq custom-org (expand-file-name "custom.org" user-emacs-directory))

;; The extra customs will be autoloaded.
(when (and (not (file-exists-p custom-el)) (file-exists-p custom-org)) (org-babel-tangle-file custom-org))
(when (file-exists-p custom-el) (load custom-el))


;;----------------------------------------------------------------------------
;; Session
;;----------------------------------------------------------------------------
(defvar session-packages '(desktop
                           saveplace
                           ) "Package list for session managment")
(load-package-layer session-packages)

(desktop-save-mode)
(setq-default save-place t)
(add-hook 'after-init-hook 'session-initialize)

;; recentf-mode
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-saved-items 1000)
(setq recentf-exclude '("/tmp/" "/ssh:" "/sudo:" "/home/[a-z]\+/\\."))
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

;; Warmer welcomes
(setq-default initial-scratch-message
              (concat ";; Happy hacking in the PULSE powered " (or invocation-name "") ", " (or user-login-name "") "\n\n"))

(message "Emacs session initialization finished in %d seconds." (time-to-seconds (time-since emacs-load-start-time)))
(message "\nWelcome to PULSE powered %s, %s!\n" (invocation-name) (user-login-name))
