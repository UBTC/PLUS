;; !/usr/bin/env emacs
;; -*- coding:utf-8 -*-

;;----------------------------------------------------------------------------
;;  <vim-plus-emacs> Copyright (C) <2010-2015>  <Mogei Wang>
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;  If you need to contact the author, <mailto:mogeiwang@NOSPAM.gmail.com>
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(setq custom-file "custom.el")
(setq emacs-load-start-time (current-time))
(setq *macbook-pro-support-enabled* t)
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))
(setq *win32* (eq system-type 'windows-nt) )
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(setq *linux-x* (and window-system *linux*) )
(setq *xemacs* (featurep 'xemacs) )
(setq *emacs23* (and (not *xemacs*) (or (>= emacs-major-version 23))) )
(setq *emacs24* (and (not *xemacs*) (or (>= emacs-major-version 24))) )
(setq *no-memory* (cond
                   (*is-a-mac*
                    (< (string-to-number (nth 1 (split-string (shell-command-to-string "sysctl hw.physmem")))) 4000000000))
                   (*linux* nil)
                   (t nil)))

;;----------------------------------------------------------------------------
;; Less GC, more memory
;;----------------------------------------------------------------------------
(defun my-optimize-gc (NUM PER)
"By default Emacs will initiate GC every 0.76 MB allocated (gc-cons-threshold == 800000).
@see http://www.gnu.org/software/emacs/manual/html_node/elisp/Garbage-Collection.html
We increase this to 16MB by `(my-optimize-gc 16 0.5)` "
  (setq-default gc-cons-threshold (* 1024 1024 NUM)
                gc-cons-percentage PER))
(my-optimize-gc 16 0.5)

;;----------------------------------------------------------------------------
;; Load site-lisp configs
;;----------------------------------------------------------------------------
(eval-when-compile (require 'cl))

(if (fboundp 'normal-top-level-add-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/site-lisp/")
           (default-directory my-lisp-dir))
      (progn
        (setq load-path
              (append
               (loop for dir in (directory-files my-lisp-dir)
                     unless (string-match "^\\." dir)
                     collecting (expand-file-name dir))
               load-path)))))

;;----------------------------------------------------------------------------
;; Load elpa configs
;;----------------------------------------------------------------------------
(require 'package)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ;; uncomment below line if you need to use GNU ELPA
                         ;; ("gnu" . "http://elpa.gnu.org/packages/")
                         ))

(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(package-initialize)
(require-package 'evil)
(require-package 'session)
(require-package 'dired+)
(require-package 'smex)
(require-package 'idomenu)
(require-package 'yasnippet)
(require-package 'flymake)
(require-package 'flyspell-lazy)
(require-package 'company)
(require-package 'vlf)
(require-package 'org)
(require-package 'elpy)
(require-package 'color-theme)
(require-package 'monokai-theme)
(require-package 'browse-kill-ring)
(require-package 'haskell-mode)
(require-package 'window-numbering)
(require-package 'pandoc-mode)
(require-package 'gnuplot-mode)
(require-package 'go-mode)
(require-package 'ebib)
(require-package 'markdown-mode)
(require-package 'ess)
(require-package 'julia-mode)
(require 'julia-shell)
(require 'ess-site)
(require 'ob)

;;----------------------------------------------------------------------------
;; configure evil
;;----------------------------------------------------------------------------
(setq evil-symbol-word-search t)
(add-to-list 'load-path "~/.emacs.d/site-lisp/evil/lib")
(setq evil-default-cursor t)
(require 'evil)
(evil-mode 1)

;; Make movement keys work like they should
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;;----------------------------------------------------------------------------
;; configure session
;;----------------------------------------------------------------------------
;; Load the last edited files
(setq desktop-path '("~/.emacs.d"))
(load "desktop")
(desktop-save-mode)
(setq session-save-file (expand-file-name "~/.emacs.d/.session"))
(add-hook 'after-init-hook 'session-initialize)

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
;; configure dired
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
            (list (concat "\\." (regexp-opt (cdr file) t) "$")
                          (car file))))

;;----------------------------------------------------------------------------
;; configure smex
;;----------------------------------------------------------------------------
(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-Z") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;----------------------------------------------------------------------------
;; configure ido
;;----------------------------------------------------------------------------
;; Use C-f during file selection to switch to regular find-file
(require 'ido)
(ido-mode t)  ; use 'buffer rather than t to use only buffer switching
(ido-everywhere t)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length 0)
(setq ido-use-virtual-buffers t)
;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)

;;----------------------------------------------------------------------------
;; configure yasnippet
;;----------------------------------------------------------------------------
(require 'yasnippet)
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)

;;----------------------------------------------------------------------------
;; configure flymake
;;----------------------------------------------------------------------------
(require 'flymake)
;; I want to see at most the first 4 errors for a line
(setq flymake-number-of-errors-to-display 4)
;; Let's run 2 checks at once instead.
(setq flymake-max-parallel-syntax-checks 2)
(setq flymake-gui-warnings-enabled nil)

;;----------------------------------------------------------------------------
;; configure flyspell
;;----------------------------------------------------------------------------
(require 'flyspell-lazy)
(flyspell-mode 1)
(flyspell-prog-mode)
(flyspell-lazy-mode 1)
;; better performance:
(setq flyspell-issue-message-flag nil)

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

;; ispell-cmd-args is useless, it's the list of *extra* command line arguments we will append to the ispell process when ispell-send-string()
;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

;;----------------------------------------------------------------------------
;; configure company
;;----------------------------------------------------------------------------
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
;; configure vlf
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
;; configure org
;;----------------------------------------------------------------------------
(require 'org)
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
     ;; {{ org 8.2.6 has some performance issue. Here is the workaround.
     ;; @see http://punchagan.muse-amuse.in/posts/how-i-learnt-to-use-emacs-profiler.html
     org-agenda-inhibit-startup t ;; ~50x speedup
     org-agenda-use-tag-inheritance nil ;; 3-4x speedup
     ;; }}
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
   (julia . t)
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
   ))

;; No prompts when runing codes
(setq org-confirm-babel-evaluate nil)
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'org-display-inline-images)

;; babel-exec julia
(load "~/.emacs.d/site-lisp/ob-julia/ob-julia.el")

;;----------------------------------------------------------------------------
;; configure python
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
;; configure color-theme
;;----------------------------------------------------------------------------
(require 'color-theme)
(require 'monokai-theme)
;; work around color theme bug
;; @see https://plus.google.com/106672400078851000780/posts/KhTgscKE8PM
(defadvice load-theme (before disable-themes-first activate)
  ;; diable all themes
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;;----------------------------------------------------------------------------
;; configure terminal
;;----------------------------------------------------------------------------
;; @see http://stackoverflow.com/questions/2886184/copy-paste-in-emacs-ansi-term-shell/2886539#2886539
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

;; {{ @see http://emacs-journey.blogspot.com.au/2012/06/improving-ansi-term.html
;; kill the buffer when terminal is exited
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; always use bash
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; utf8
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)

;;----------------------------------------------------------------------------
;; configure shells
;;----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.bash_profile\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash_history\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.fish\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc.local\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . sh-mode))

;;----------------------------------------------------------------------------
;; configure haskell
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
;; configure lisp
;;----------------------------------------------------------------------------
(require 'lisp-mode)
;; (autoload 'enable-paredit-mode "paredit")
(setq-default initial-scratch-message
              (concat ";; Welcome to vim-plus-emacs, " (or user-login-name "") ". Happy hacking!\n\n"))
(let* ((lispy-hooks '(lisp-mode-hook
                      inferior-lisp-mode-hook
                      lisp-interaction-mode-hook))))

(turn-on-eldoc-mode)
(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))
(push '("\\.el$" flymake-elisp-init) flymake-allowed-file-name-masks)

;;----------------------------------------------------------------------------
;; configure markdown
;;----------------------------------------------------------------------------
(autoload 'markdown-mode "markdown-mode" "Mode for editing Markdown documents" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
   (cons '("\\.markdown" . markdown-mode) auto-mode-alist))

;;----------------------------------------------------------------------------
;; configure pandoc mode
;;----------------------------------------------------------------------------
(add-hook 'markdown-mode-hook 'conditionally-turn-on-pandoc)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

;;----------------------------------------------------------------------------
;; configure gnuplot...
;;----------------------------------------------------------------------------
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

(setq gnuplot-basic-offset 2)
(setq gnuplot-context-sensitive-mode t)
(setq gnuplot-inline-imge-mode t)

;;----------------------------------------------------------------------------
;; configure golang...
;;----------------------------------------------------------------------------
;; install package
(require 'go-mode)

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
(when (executable-find "goimports")
  (setq gofmt-command "goimports"))

;;----------------------------------------------------------------------------
;; configure latex...
;;----------------------------------------------------------------------------
;;; LaTeX
;; https://github.com/CestDiego/.emacs.d/blob/master/user-lisp/setup-latex.el
;; https://github.com/xyguo/emacs.d/blob/master/lisp/init-auctex.el
;; https://github.com/xiaohanyu/oh-my-emacs/blob/master/modules/ome-tex.org

;; (add-to-list 'load-path "~/.emacs.d/lisp/auctex")
;; (load "~/.emacs.d/site-lisp/auctex/auctex.el" nil t t)

(global-set-key "\C-cb" 'ebib)

(setq TeX-default-mode 'LaTeX-mode) ; Default mode for .tex files
(setq TeX-force-default-mode t) ; Force This mode Always, it is MANDATORY for my sake
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ; Activar reftex con AucTeX
;; (setq reftex-plug-into-AUCTeX t) ; COnectar a AUC TeX con RefTeX
;; ;; (setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))

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

;; (when (locate-library "auctex") (setq reftex-plug-into-AUCTeX t))

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
 ((eq system-type 'gnu/linux)
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
            (output-dvi "xdg-open")))))))

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
;; configure ESS
;;----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . R-mode))
(autoload 'R-mode "ess-site")
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
;; where the julia executable is
(setq  inferior-julia-program-name "/usr/bin/julia")

(defun my-julia-mode-hooks ()
  (require 'julia-shell-mode))
(add-hook 'julia-mode-hook 'my-julia-mode-hooks)
(define-key julia-mode-map (kbd "C-c C-c") 'julia-shell-run-region-or-line)
(define-key julia-mode-map (kbd "C-c C-s") 'julia-shell-save-and-go)

;;----------------------------------------------------------------------------
;; configure misc...
;;----------------------------------------------------------------------------
;; ----- copy from Chen: -----------------------------
(global-linum-mode t)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Use regex to search by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(setq-default buffers-menu-max-size 30
              case-fold-search t
              compilation-scroll-output t
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain
              save-interprogram-paste-before-kill t
              grep-highlight-matches t
              grep-scroll-output t
              indent-tabs-mode nil
              line-spacing 0.2
              mouse-yank-at-point t
              set-mark-command-repeat-pop t
              tooltip-delay 1.5
              ;; void problems with crontabs, etc.
              ;; require-final-newline t ; bad idea, could accidentally edit others' code
              truncate-lines nil
              truncate-partial-width-windows nil
              ;; visible-bell has some issue
              ;; @see https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/issues/9#issuecomment-97848938
              visible-bell nil)

;; @see http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)

;; find-file-in-project (ffip)
(autoload 'ivy-read "ivy")
(autoload 'find-file-in-project "find-file-in-project" "" t)
(autoload 'find-file-in-project-by-selected "find-file-in-project" "" t)
(autoload 'ffip-get-project-root-directory "find-file-in-project" "" t)

(require 'browse-kill-ring)
;; no duplicates
(setq browse-kill-ring-display-duplicates nil)
;; preview is annoying
(setq browse-kill-ring-show-preview nil)
(browse-kill-ring-default-keybindings)

(fset 'yes-or-no-p 'y-or-n-p)

;; NO automatic new line when scrolling down at buffer bottom
(setq next-line-add-newlines nil)

;; @see http://stackoverflow.com/questions/4222183/emacs-how-to-jump-to-function-definition-in-el-file
(global-set-key (kbd "C-h C-f") 'find-function)

;; from RobinH, Time management
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

;; a no-op function to bind to if you want to set a keystroke to null
(defun void () "this is a no-op" (interactive))

(defalias 'list-buffers 'ibuffer)

;; effective emacs item 7; no scrollbar, no menubar, no toolbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; vimrc
(autoload 'vimrc-mode "vimrc-mode")
(add-to-list 'auto-mode-alist '("\\.?vim\\(rc\\)?$" . vimrc-mode))

;; https://github.com/nschum/highlight-symbol.el
(autoload 'highlight-symbol "highlight-symbol" "" t)
(autoload 'highlight-symbol-next "highlight-symbol" "" t)
(autoload 'highlight-symbol-prev "highlight-symbol" "" t)
(autoload 'highlight-symbol-nav-mode "highlight-symbol" "" t)
(autoload 'highlight-symbol-query-replace "highlight-symbol" "" t)

;; recentf-mode
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-saved-items 1000
      recentf-exclude '("/tmp/"
                        "/ssh:"
                        "/sudo:"
                        "/home/[a-z]\+/\\."))

;; move focus between sub-windows
(require 'window-numbering)
(custom-set-faces '(window-numbering-face ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold)))))
(window-numbering-mode 1)

(transient-mark-mode t)
(recentf-mode 1)
(column-number-mode 1)

;;----------------------------------------------------------------------------
;; configure locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
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

;; ----- my previous miscs -----------------------------
;; User information
(setq user-full-name "Mogei Wang")
(setq user-mail-address "mogeiwang@gmail.com")

;; Fn keys.
(global-set-key [f1]  'split-window-horizontally)
(global-set-key [f2]  'split-window-vertically)
(global-set-key [f3]  'next-multiframe-window)
(global-set-key [f4]  'delete-window)
(global-set-key [f5]  'compile)
(global-set-key [f6]  'ispell-buffer)
(global-set-key [f7]  'comment-region)
(global-set-key [f8]  'speedbar)
(global-set-key [f9]  'eshell)

;; Highlight the current line
(global-hl-line-mode t)

;; Set scroll mode
(setq scroll-step 1)
(setq scroll-margin 10)
(setq scroll-conservatively 100)

;; S-tab quotes a tab
(global-set-key [backtab] "\C-q\t")

;; Auto complete
(global-set-key [C-tab] 'hippie-expand)
(setq tab-always-indent 'complete)
(icomplete-mode 1)

;; case insensitive completion
(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

(setq-default indent-tabs-mode t)
(setq default-tab-width 4)
(setq tab-width 4)

(setq whitespace-style '(tab-mark face trailing space-before-tab space-after-tab))
(global-whitespace-mode t)

; Make horizontal movement cross lines
(setq-default evil-cross-lines t)

;; Show paren
(setq show-paren-mode t)

;; No visible bells
(setq visible-bell nil)

;; Operate compressed files
(auto-compression-mode 1)

;; Auto save
(setq auto-save-mode t)
(setq version-control t)
(setq delete-old-versions t)
(setq vc-make-backup-files t)
(setq kept-old-versions 5)
(setq kept-new-versions 5)
(setq backup-directory-alist (quote (("" . "~/.emacs.d/backup"))))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;;hs-minor-mode, see also ~/.emacs.d/lisp/init-hs-minor-mode.el
(load-library "hideshow")
(global-set-key (kbd "C-=") 'hs-hide-block)
(global-set-key (kbd "C--") 'hs-show-block)
(global-set-key (kbd "C-+") 'hs-hide-all)

;; Always use gdb-many-windows:
(setq gdb-many-windows t)
(setq gdb-show-main t)

;; This option was in ~/.emacs.d/lisp/init-misc.el
(blink-cursor-mode -1)

;; Title show in the frame maintanced by the system window manager
(setq frame-title-format '("%b" " ~ " invocation-name))

;----------------------------------------------------------------------------
; Variables configured via the interactive 'customize' interface
;----------------------------------------------------------------------------
;; a custom.el can be autoloaded at startup (I use the above misc section)
(when (file-exists-p custom-file) (load custom-file))

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

;;; Local Variables:
;;; no-byte-compile: t
(put 'erase-buffer 'disabled nil)

;; All done
(when (require 'time-date nil t)
   (message "Emacs startup time: %d seconds."
    (time-to-seconds (time-since emacs-load-start-time))))
(message "\nInitialization finished. Welcome to vim-plus-%s, %s!\n" (invocation-name) (user-login-name) )
;;; End
