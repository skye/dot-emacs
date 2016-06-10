;; skyewm's .emacs

;(add-to-list 'load-path "~/.emacs.d")

(defvar elisp-dir "~/.emacs.d/vendor")
(let ((oldwd default-directory))
  ; Add all my personal scripts
  (add-to-list 'load-path elisp-dir)
  ; Add third-party libraries to load path
  (progn (cd elisp-dir)
         (normal-top-level-add-subdirs-to-load-path))
  ; Clean up
  (cd oldwd))

;(set-default-font "-Misc-Fixed-Medium-R-SemiCondensed--13-120-75-75-C-60-ISO8859-1")
;(set-default-font "Droid Sans Mono-9")
(set-face-attribute 'default nil :height 110)

;(add-to-list 'default-frame-alist '(font . "Droid Sans Mono-9"))
(add-to-list 'default-frame-alist '(geometry . "-1-1"))

(defun safe-load (library)
  (condition-case err
      (load-library library)
    (error
     (progn
       (message "Failed to load %s: %s" library err)
       (sleep-for 3)))))


;; load libraries
(mapcar
 'safe-load
 '(
;;   "color-theme"    ;; loads color theme
;;   "psvn"           ;; loads psvn
   "zenburn"        ;; zenburn color theme
   "pastels-on-dark" ;; pastels on dark color theme
   "inkpot"
   ))

(color-theme-inkpot)

;; Scheme binary
;(setq scheme-program-name "mzscheme")

(global-font-lock-mode t)

;; enable C-x C-m = M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)

;stop startup message
(setq inhibit-startup-message t)

;(setq inhibit-startup-echo-area-message "skyewm")

;; ido mode
(ido-mode t)

(show-paren-mode t)
(setq show-paren-delay 0)

(blink-cursor-mode 0)

; add column numbers
(column-number-mode t)
(line-number-mode t)

;; Highlight regions and add special behaviors to regions.
;; "C-h d transient" for more info
(setq transient-mark-mode t)

;; Window frame title
(setq frame-title-format "emacs [%b %*%+ %f]")
(setq icon-title-format "emacs [%b]")

;; no bells
(setq ring-bell-function 'ignore)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; set C-\ to kill buffer
(global-set-key [(control \\)] 'kill-this-buffer)

;; Set C-tab to cycle buffer
(global-set-key [(control tab)] 'cyclebuffer-forward)

;; Fullscreen editing, hit f11
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

;; display time in status
(display-time-mode t)

;; battery mode
;;(display-battery-mode t)

;; comment and uncomment hook
(defun toggle-comment ()
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))
(global-set-key [f2] 'toggle-comment)

;; compile
(global-set-key "\C-z" 'compile)

;; goto line as c-x g
(global-set-key "\C-xg" 'goto-line)

;; when viewing pdf/dvi automatically reload them if they change
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(tool-bar-mode 0)
(scroll-bar-mode -1)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-output-view-style
   (quote
    (("^dvi$"
      ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$")
      "%(o?)dvips -t landscape %d -o && gv %f")
     ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f")
     ("^dvi$"
      ("^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "^landscape$")
      "%(o?)xdvi %dS -paper a4r -s 0 %d")
     ("^dvi$" "^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "%(o?)xdvi %dS -paper a4 %d")
     ("^dvi$"
      ("^\\(?:a5\\(?:comb\\|paper\\)\\)$" "^landscape$")
      "%(o?)xdvi %dS -paper a5r -s 0 %d")
     ("^dvi$" "^\\(?:a5\\(?:comb\\|paper\\)\\)$" "%(o?)xdvi %dS -paper a5 %d")
     ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d")
     ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d")
     ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d")
     ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d")
     ("^dvi$" "." "%(o?)xdvi %dS %d")
     ("^pdf$" "." "evince %o")
     ("^html?$" "." "firefox %o"))))
 '(auto-revert-interval 1)
 '(c-basic-offset 2)
 '(case-fold-search t)
 '(compile-command
   "cd /home/skye/Impala && IMPALA_HOME=/home/skye/Impala make -j8 impalad")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(fci-handle-truncate-lines t)
 '(fill-column 90)
 '(goal-column nil)
 '(graphviz-dot-auto-indent-on-semi nil)
 '(password-cache-expiry nil)
 '(python-indent 2)
 '(python-indent-offset 2)
 '(require-final-newline t)
 '(show-trailing-whitespace t)
 '(split-height-threshold nil)
 '(tags-case-fold-search t)
 '(truncate-partial-width-windows nil)
 '(user-mail-address "skyewm@gmail.com")
 '(x-select-enable-primary t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-error ((t (:inherit error :foreground "red"))))
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red"))))
 '(ediff-current-diff-A ((t (:background "#444"))))
 '(ediff-current-diff-B ((t (:background "#444"))))
 '(ediff-current-diff-C ((t (:background "#444"))))
 '(ediff-fine-diff-B ((t (:background "dark green"))))
 '(font-lock-string-face ((t (:background "#1e1e27" :foreground "#ffcd8b"))))
 '(magit-item-highlight ((((class color) (background dark)) (:background "gray18"))))
 '(modeline ((t (:background "#3e3e5e" :foreground "lawn green" :weight bold))))
 '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "gold"))))
 '(outline-2 ((t (:inherit font-lock-variable-name-face :foreground "sky blue"))))
 '(outline-3 ((nil (:foreground "light pink"))))
 '(rainbow-delimiters-depth-1-face ((((background dark)) (:foreground "purple"))))
 '(rainbow-delimiters-depth-2-face ((((background dark)) (:foreground "#bbbb22"))))
 '(rainbow-delimiters-depth-3-face ((((background dark)) (:foreground "#44bbff"))))
 '(rainbow-delimiters-depth-4-face ((((background dark)) (:foreground "magenta"))))
 '(rainbow-delimiters-depth-5-face ((((background dark)) (:foreground "#88dd88"))))
 '(rainbow-delimiters-depth-6-face ((((background dark)) (:foreground "#8888dd"))))
 '(rainbow-delimiters-depth-7-face ((((background dark)) (:foreground "salmon"))))
 '(rainbow-delimiters-depth-8-face ((((background dark)) (:foreground "yellow"))))
 '(rainbow-delimiters-depth-9-face ((((background dark)) (:foreground "forestgreen"))))
 '(rainbow-delimiters-unmatched-face ((((background dark)) (:background "#ff090B" :foreground "#ffffff")))))
 ;highlight unmatched delimiters in bright red

;; (custom-set-variables
;;      '(scheme-program-name "/usr/bin/scheme"))

(require 'xscheme)

;; (add-hook 'scheme-mode-hook (lambda ()
;; 	  (global-set-key "\r" 'xscheme-send-current-line)))


(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)

(add-to-list 'auto-mode-alist '("\\.jsim\\'" . jsim-mode))
(autoload 'jsim-mode "jsim" nil t)

(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
(autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)

(setq-default indent-tabs-mode nil) ; always replace tabs with spaces
(setq-default tab-width 2) ; set tab width to 4 for all buffers
;; (setq-default tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))

;; Haskell mode
(require 'inf-haskell)

;; winmove
(when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings))
(global-set-key (kbd "<C-S-i>") 'windmove-up)
(global-set-key (kbd "<C-S-k>") 'windmove-down)
(global-set-key (kbd "<C-S-j>") 'windmove-left)
(global-set-key (kbd "<C-S-l>") 'windmove-right)

;; map C-x p to prev-window
(global-set-key "\C-xp" 'previous-multiframe-window)

;disable backup
;(setq backup-inhibited t)
;disable auto save
;(setq auto-save-default nil)
; backup directory
(setq backup-directory-alist `((".*" . "~/.emacs.d/backups")))

;; Winner mode
(when (fboundp 'winner-mode)
      (winner-mode 1))

;; CEDET
;; (load-file (concat elisp-vendor-dir "/cedet-1.0/common/cedet.el"))
;; (global-ede-mode 1)                      ; Enable the Project management system
;; (semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
;; (global-srecode-minor-mode 1)            ; Enable template insertion menu

;; protobuf mode
(require 'protobuf-mode)

(cd "~/")

(defun my-c-mode-hook ()
  (setq c-basic-offset 2))
(add-hook 'c-mode-hook 'my-c-mode-hook)

;; magit
(require 'magit)
;; change magit diff colors
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (when (not window-system)
       (set-face-background 'magit-item-highlight "black"))))
;; C-' = magit-status
;;(global-set-key (kbd "C-'") 'magit-status)

(add-hook 'python-mode-hook '(lambda ()
	  (local-set-key (kbd "C-c C-i") 'python-end-of-block)))

;; highlight-symbol
(require 'highlight-symbol)
(global-set-key [f5] 'highlight-symbol-at-point)
(global-set-key [f6] 'highlight-symbol-remove-all)

(defun lines ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'count-words-region)
    (count-lines-page)))
(global-set-key (kbd "C-x l") 'lines)

(defun python-summary ()
  (interactive)
  (occur "\\<def\\>\\|\\<class\\>"))

;; sublime minimap
(require 'minimap)

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))
(global-set-key [f10] 'increment-number-at-point)

(defun yank-pop-reverse ()
  (interactive)
  (yank-pop -1)
)
(global-set-key (kbd "M-p") 'yank-pop-reverse)

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode 1)

(global-set-key (kbd "C-S-o") 'occur)

(defun clipboard-copy ()
  (interactive)
  (clipboard-kill-region (region-beginning) (region-end))
  (clipboard-yank)
)
(global-set-key (kbd "C-S-c") 'clipboard-copy)
(global-set-key (kbd "C-S-v") 'clipboard-yank)

(put 'downcase-region 'disabled nil)

(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(fset 'html-left-bracket
   [?\M-% ?\{ backspace ?< return ?& ?# ?6 ?0 ?\; return ?!])
(fset 'html-right-bracket
   [?\M-% ?> return ?& ?# ?6 ?2 ?\; return ?!])
(put 'upcase-region 'disabled nil)

(require 'thrift-mode)

(require 'ack)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(require 'google-c-style)
;; (add-hook 'c-mode-common-hook 'google-set-c-style)

;; C tweaks for Impala
(defconst my-cc-style
  '("Google" (c-offsets-alist .
                           ((innamespace . [0])
                            (arglist-cont-nonempty . 4)
                            (arglist-intro . 4)
                            (access-label . -1)
                            (member-init-intro . 4))
                           )))
(c-add-style "my-cc-style" my-cc-style)
(defun set-my-cc-style () (interactive) (google-set-c-style) (c-set-style "my-cc-style"))
(add-hook 'c-mode-common-hook 'set-my-cc-style)

(defconst my-cc-style
  '("Google" (c-offsets-alist .
                              ((innamespace . [0])
                               (arglist-cont-nonempty . 4)
                               (arglist-intro . 4)
                               (access-label . -1)
                               (member-init-intro . 4))
                              )))
(c-add-style "my-cc-style" my-cc-style)

(defun scroll-up-one ()
  (interactive)
  (scroll-up 1)
)
(defun scroll-down-one ()
  (interactive)
  (scroll-down 1)
)
(global-set-key (kbd "C->") 'scroll-up-one)
(global-set-key (kbd "C-<") 'scroll-down-one)

(defun ack-be (query)
  (interactive "Mack-grep ")
  (ack (concat "ack-grep " query)
       "/home/skye/Impala/be/src"))

(defun ggrep (query)
  (interactive "Mgit grep ")
  (vc-git-grep query "*" "/home/skye/Impala/"))

(require 'workgroups)
(setq wg-prefix-key (kbd "C-c C-w"))

(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

(global-set-key (kbd "C-S-s") 'isearch-forward-regexp)

;; https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
 (let ((name (buffer-name))
	(filename (buffer-file-name)))
 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (if (get-buffer new-name)
	 (message "A buffer named '%s' already exists!" new-name)
	(progn 	 (rename-file name new-name 1) 	 (rename-buffer new-name) 	 (set-visited-file-name new-name) 	 (set-buffer-modified-p nil))))))

;; https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
 (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	 (if (string-match dir "\\(?:/\\|\\\\)$")
	 (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))

 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (progn 	(copy-file filename newname 1) 	(delete-file filename) 	(set-visited-file-name newname) 	(set-buffer-modified-p nil) 	t))))

(defun open-or-switch-to (file)
 (let ((existing-buffer (find-buffer-visiting file)))
   (cond (existing-buffer (pop-to-buffer existing-buffer))
         (t (find-file-existing file)))))

(defun switch-to-header-or-impl ()
 (interactive)
 (let ((other-file
        (cond
         ((not (buffer-file-name)) (error "Buffer not visiting a file"))
         ((string-match-p "\\.cc$" (buffer-file-name))
          (replace-regexp-in-string "\\.cc$" ".h"  (buffer-file-name)))
         ((string-match-p "\\.h$" (buffer-file-name))
          (replace-regexp-in-string "\\.h$" ".cc" (buffer-file-name)))
         (t (error "Not a .cc or .h file: %s" (buffer-file-name))))))
        (open-or-switch-to other-file)))
(global-set-key (kbd "C-M-;") 'switch-to-header-or-impl)

;; (require 'nhexl-mode)

;; from henry
(defun find-in-impala (regex)
   (interactive "sFilename pattern: ")
   (let ((find-name-arg "-iname"))
     (find-name-dired (getenv "IMPALA_HOME") (format "*%s*" regex))))
(global-set-key (kbd "C-c C-f") 'find-in-impala)

(defun make-lzo ()
  (interactive)
  (compile "cd ~/Impala-lzo && make"))
(global-set-key (kbd "C-x C-z") 'make-lzo)

(defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files.") )

(require 'fill-column-indicator)
;; (add-hook 'after-change-major-mode-hook 'fci-mode)
(add-hook 'c-mode-common-hook 'fci-mode)
(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'sql-mode-hook 'fci-mode)
(setq fci-rule-color "#222244")

(require 'multiple-cursors)
(global-set-key (kbd "C-\"") 'mc/mark-next-like-this)
(global-set-key (kbd "C-:") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-'") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-;") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-c '") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c ;") 'mc/mark-pop)

(set-default 'truncate-lines nil)

(global-set-key (kbd "<f12>") 'toggle-truncate-lines)

(defun find-duplicate-lines (&optional insertp interp)
    (interactive "i\np")
    (let ((max-pon (line-number-at-pos (point-max)))
	  (gather-dups))
      (while (< (line-number-at-pos) max-pon) (= (forward-line) 0)
	     (let ((this-line (buffer-substring-no-properties (line-beginning-position 1) (line-end-position 1)))
		   (next-line (buffer-substring-no-properties (line-beginning-position 2) (line-end-position 2))))
	       (when  (equal this-line next-line)  (setq gather-dups (cons this-line gather-dups)))))
      (if (or insertp interp)
	  (save-excursion (new-line) (princ gather-dups (current-buffer)))
	gather-dups)))

(defun randomize-region (beg end)
  (interactive "r")
  (if (> beg end)
      (let (mid) (setq mid end end beg beg mid)))
  (save-excursion
    ;; put beg at the start of a line and end and the end of one --
    ;; the largest possible region which fits this criteria
    (goto-char beg)
    (or (bolp) (forward-line 1))
    (setq beg (point))
    (goto-char end)
    ;; the test for bolp is for those times when end is on an empty
    ;; line; it is probably not the case that the line should be
    ;; included in the reversal; it isn't difficult to add it
    ;; afterward.
    (or (and (eolp) (not (bolp)))
        (progn (forward-line -1) (end-of-line)))
    (setq end (point-marker))
    (let ((strs (shuffle-list
                 (split-string (buffer-substring-no-properties beg end)
                               "\n"))))
      (delete-region beg end)
      (dolist (str strs)
        (insert (concat str "\n"))))))

(defun shuffle-list (list)
  "Randomly permute the elements of LIST.
All permutations equally likely."
  (let ((i 0)
        j
        temp
        (len (length list)))
    (while (< i len)
      (setq j (+ i (random (- len i))))
      (setq temp (nth i list))
      (setcar (nthcdr i list) (nth j list))
      (setcar (nthcdr j list) temp)
      (setq i (1+ i))))
  list)

(semantic-mode 1)
(global-ede-mode 1)

(global-semantic-stickyfunc-mode 1)

(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hooks 'my-find-file-check-make-large-file-read-only-hook)

(defun open-or-switch-to (file)
  (let ((existing-buffer (find-buffer-visiting file)))
    (cond (existing-buffer (set-window-buffer nil existing-buffer))
          (t (find-file-existing file)))))


(defun switch-to-header-or-impl ()
  (interactive)
  (let ((other-file
         (cond
          ((not (buffer-file-name)) (error "Buffer not visiting a file"))
          ((string-match-p "\\.cc$" (buffer-file-name))
           (replace-regexp-in-string "\\.cc$" ".h"  (buffer-file-name)))
          ((string-match-p "\\.h$" (buffer-file-name))
           (replace-regexp-in-string "\\.h$" ".cc" (buffer-file-name)))
          (t (error "Not a .cc or .h file: %s" (buffer-file-name))))))
         (open-or-switch-to other-file)))
(global-set-key (kbd "C-M-;") 'switch-to-header-or-impl)

(add-hook 'compilation-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'grep-mode-hook (lambda () (setq truncate-lines t)))

(visit-tags-table "/home/skye/Impala/be/src/TAGS")

(add-hook 'before-save-hook 'delete-trailing-whitespace)
