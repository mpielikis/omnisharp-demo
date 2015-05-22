
;; ---------
;; PACKAGING

(require 'package)
(setq package-archives
      '(("melpa"
         . "http://melpa.org/packages/")))
(package-initialize)

;; ---------------
;; COMMON SETTINGS

(defvar mswindows-p (string-match "windows" (symbol-name system-type)))
(defvar linux-p (string-match "linux" (symbol-name system-type)))

(cua-mode 1)                       ; cut / copy / paste for noobs
(setq inhibit-splash-screen t)     ; disable splash screen
(fset 'yes-or-no-p 'y-or-n-p)      ; stop asking me to type ‘yes’ as a confirmation
(show-paren-mode t)                ; see matching pairs of parentheses
(savehist-mode t)                  ; save minibuffer history
(setq ring-bell-function 'ignore)  ; don't "ring the bell"
(setq system-uses-terminfo nil)    ; use Emacs terminfo, not system terminfo

;; show tabs and trailing whitespace
(global-whitespace-mode)
(setq whitespace-style '(trailing tabs tab-mark))

;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; buffers
(ido-mode t)
(setq ido-enable-flex-matching t) ; any buffer name containing the entered characters in the given sequence will match.
(setq ido-create-new-buffer 'always) ; do not ask permission to create a new buffer
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; highlight parantheses that surrounds cursor
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; ------------------
;; COMMON KEYBINDINGS

(global-set-key (kbd "M-x") 'smex) ; set smex as default

;; window movement
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<up>") 'windmove-up)

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "s-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x b") 'ibuffer)

;; ------------
;; GUI SETTINGS

(tool-bar-mode -1)        ; disable toolbar
(menu-bar-mode -1)        ; disable menu
(set-scroll-bar-mode nil) ; disable scrollbar
(global-hl-line-mode 1)   ; highlight current line

;; Colors

(load-theme 'monokai t)

(set-face-background 'hl-line "#333")
(set-face-background 'region "#555")

;; Fonts

(when mswindows-p
  (set-face-attribute 'default nil
                      :family "Consolas" :height 100))
(when linux-p
  (set-face-attribute 'default nil
                      :family "Ubuntu Mono" :height 100))

;; ------------------
;; CODING SETTINGS

;; Indentation

(setq tab-width 4) ; or any other preferred value
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq-default indent-tabs-mode nil)
(custom-set-variables
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))))
(define-key text-mode-map (kbd "TAB") 'tab-to-tab-stop)

;; ------------------
;; FUNC: Path helpers

;; (defun find-git-repo (dir)
;;   (if (string= "/" dir) full	
;;       nil
;;     (if (file-exists-p (expand-file-name "../.git/" dir))
;;         dir
;;       (find-git-repo (expand-file-name "../" dir)))))

;; (defun find-project-root ()
;;   (interactive)
;;   (if (ignore-errors (eproject-root))
;;       (eproject-root)
;;     (or (find-git-repo (buffer-file-name)) (file-name-directory (buffer-file-name)))))

;; (defun file-path-to-namespace ()
;;   (interactive)
;;   (let (
;;         (root (find-project-root))
;;         (base (file-name-nondirectory buffer-file-name))
;;         )
;;     (substring (replace-regexp-in-string "/" "\." (substring buffer-file-name (length root) (* -1 (length base))) t t) 0 -1)
;;     )
;;   )

;; --------------------
;; FUNC: CSharp helpers

;; (defun csharp-should-method-space-replace ()
;;   "When pressing space while naming a defined method, insert an underscore"
;;   (interactive)
;;   (if (and (looking-back "void Should.*")
;;            (not (and
;;                  (looking-at ".*)$")
;;                  (looking-back "(.*"))))
;;       (insert "_")
;;     (insert " ")))

;; (eval-after-load 'csharp-mode
;;   '(progn
;;      (define-key csharp-mode-map (kbd "SPC") 'csharp-should-method-space-replace)))

;; ---------------------------
;; FUNC: Common coding helpers

;; (defun comment-or-uncomment-region-or-line ()
;;   "Comments or uncomments current line or whole lines in region."
;;   (interactive)
;;   (save-excursion
;;     (let (min max)
;;       (if (region-active-p)
;;           (setq min (region-beginning) max (region-end))
;;         (setq min (point) max (point)))
;;       (comment-or-uncomment-region
;;        (progn (goto-char min) (line-beginning-position))
;;        (progn (goto-char max) (line-end-position))))))

;; FUNC: Emacs helpers
;; -------------------

;; Recursively add site-lisp to the load path
;; Make sure custom stuff goes to the front of the list
;; (let ((default-directory "~/.emacs.d/site-lisp"))
;;   (let ((old-path (copy-sequence load-path))
;;                 (new-load-path nil))
;;         (normal-top-level-add-to-load-path '("."))
;;         (normal-top-level-add-subdirs-to-load-path)
;;         (dolist (var load-path)
;;           (unless (memql var old-path)
;;                 (add-to-list 'new-load-path var)
;;                 (setq load-path (append new-load-path old-path))))))

;; --------------
;; COMPANY CONFIG

;; (require 'company)

;; (setq company-begin-commands '(self-insert-command))
;; (setq omnisharp-company-do-template-completion t)
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(company-frontends
;;    (quote
;;     (company-pseudo-tooltip-frontend company-echo-metadata-frontend)))
;;  '(company-idle-delay 0.03)
;;  '(company-minimum-prefix-length 1)
;;  '(company-require-match nil)
;;  '(company-show-numbers t)
;;  '(cua-mode t nil (cua-base))
;;  '(helm-ag-insert-at-point (quote word))

;; -------------------------
;; COMPANY CONFIG. OMNISHARP

;;  '(omnisharp-auto-complete-want-documentation nil)
;;  '(omnisharp-company-sort-results t)
;;  ;; '(omnisharp-server-executable-path
;;  ;;   (quote /Users/jason/\.vim/bundle/Omnisharp/server/OmniSharp/bin/Debug/OmniSharp\.exe))

;; ;; rainbow mode coloring of #colors ???
;;  '(safe-local-variable-values
;;    (quote
;;     ((eval when
;;            (fboundp
;;             (quote rainbow-mode))
;;            (rainbow-mode 1)))))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(company-tooltip-annotation ((t (:inherit company-tooltip :foreground "yellow")))))


 
;; -------------------------
;; Escape to Quit minibuffer

;; ;; esc quits
;; (defun minibuffer-keyboard-quit ()
;;   "Abort recursive edit.
;; In Delete Selection mode, if the mark is active, just deactivate it;
;; then it takes a second \\[keyboard-quit] to abort the minibuffer."
;;   (interactive)
;;   (if (and delete-selection-mode transient-mark-mode mark-active)
;;       (setq deactivate-mark  t)
;;     (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
;;     (abort-recursive-edit)))

;; (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
;; (define-key company-mode-map [escape] 'company-abort)

;; (ido-mode t)
;; (setq ido-enable-flex-matching t)
;; (setq ido-create-new-buffer 'always)


;; ----------------------
;; Tab indent or complete

;; (defun check-expansion ()
;;   (save-excursion
;;     (if (looking-at "\\_>") t
;;       (backward-char 1)
;;       (if (looking-at "\\.") t
;;         (backward-char 1)
;;         (if (looking-at "->") t nil)))))

;; (defun do-yas-expand ()
;;   (let ((yas/fallback-behavior 'return-nil))
;;     (yas/expand)))

;; (defun tab-indent-or-complete ()
;;   (interactive)
;;   (if (minibufferp)
;;       (minibuffer-complete)
;;     (if (or (not yas/minor-mode)
;;             (null (do-yas-expand)))
;;         (if (check-expansion)
;;             (company-complete-common)
;;           (indent-for-tab-command)))))

;; (define-key company-active-map (kbd "<tab>") 'tab-indent-or-complete)


;; AAA
;; ----

;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/yasnippet-csharp"))

;; (defun dos2unix (buffer)
;;   "Automate M-% C-q C-m RET C-q C-j RET"
;;   (interactive "*b")
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (search-forward (string ?\C-m) nil t)
;;       (replace-match "" nil t)))
;;   nil
;;   )

;; (require 'helm-config)
;; (require 'helm-command)
;; (require 'helm-elisp)
;; (require 'helm-misc)
;; (require 'omnisharp)
;; (setq compilation-ask-about-save nil)

;; (global-set-key (kbd "s-o") 'ido-find-file) ;; C-x C-f

;; ;; find current buffer in directory
;; (global-set-key (kbd "C-M-l") 'neotree-find)
;; (global-set-key (kbd "<f7>") 'neotree-toggle)
;; (global-set-key (kbd "C-+") 'text-scale-increase)
;; (global-set-key (kbd "C-=") 'text-scale-increase)
;; (global-set-key (kbd "C--") 'text-scale-decrease)

;; (define-key helm-map (kbd "C-j") 'helm-next-line)
;; ;;(define-key helm-map (kbd "C-x hp") 'helm-previous-line)

;; ;;VS keys
;; (define-key global-map (kbd "s-<left>") 'beginning-of-line)
;; (define-key global-map (kbd "s-<right>") 'end-of-line)
;; (define-key global-map (kbd "s-<up>") 'scroll-down)
;; (define-key global-map (kbd "s-<down>") 'scroll-up)
;; (define-key global-map (kbd "s-f") 'toggle-frame-fullscreen)
;; (define-key global-map (kbd "S-M-<return>") 'toggle-frame-fullscreen)
;; (define-key global-map (kbd "C-g") 'goto-line)
;; ;;(define-key global-map (kbd "C-x hpa") 'helm-projectile-ag)
;; (global-set-key (kbd "C-x h f") 'helm-for-files)
;; (global-set-key (kbd "C-i") 'isearch-forward)
;; (define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)
;; (global-set-key [(control tab)] 'bury-buffer)
;; (global-set-key [(control shift tab)] 'unbury-buffer)
;; (global-set-key (kbd "C-M-<left>") 'er/expand-region)
;; (global-set-key (kbd "C-M-<right>") 'er/contract-region)
;; ;; enable ctrl-s to wrap around seeing as we disabled ctrl-r
;; (defadvice isearch-repeat (after isearch-no-fail activate)
;;   (unless isearch-success
;;     (ad-disable-advice 'isearch-repeat 'after 'isearch-no-fail)
;;     (ad-activate 'isearch-repeat)
;;     (isearch-repeat (if isearch-forward 'forward))
;;     (ad-enable-advice 'isearch-repeat 'after 'isearch-no-fail)
;;     (ad-activate 'isearch-repeat)))

;; -----------------------
;; coding: Key Chord

;; (require 'key-chord)
;; (key-chord-mode 1)
;; (setq key-chord-one-key-delay 0.2)
;; (setq key-chord-two-keys-delay 0.15)

;; --------------------------
;; Projectile

(projectile-global-mode)

;; indexing 
(if mswindows-p
  (setq projectile-indexing-method 'alien)
  (setq projectile-indexing-method 'native))


;; (define-key global-map (kbd "C-,") 'helm-projectile)
;; (define-key company-active-map (kbd "C-j") 'company-select-next-or-abort)
;; (define-key company-active-map (kbd "C-k") 'company-select-previous-or-abort)


;; -------------------
;; keys: ace

;; (define-key global-map (kbd "s-j") 'ace-jump-mode)


;; -------------------
;; HELPERS: Are not used

;; (defun company-complete-selection-insert-key(company-key)
;;   (company-complete-selection)
;;   (insert company-key))

;; (defun company-complete-selection-insert-key-and-complete(company-key)
;;   (company-complete-selection-insert-key company-key)
;;   (company-complete))

;; (defun csharp-indent-function-on-closing-brace()
;;   (interactive)
;;   (insert "}")
;;   (c-indent-defun))

;; ;; better than vim-vinegar
;; (require 'dired)

;; -----------------
;; ElScreen

;; (global-set-key [M-left] 'elscreen-previous)
;; (global-set-key [M-right] 'elscreen-next)


;; ;; This is your old M-x.
;; ;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; ;; (setq backup-directory-alist
;; ;;           `((".*" . ,temporary-file-directory)))
;; ;; (setq auto-save-file-name-transforms
;; ;;           `((".*" ,temporary-file-directory t)))



;; (setq ring-bell-function 'ignore)


;; ----------------------
;; AnsiTerm improving: http://echosa.github.io/blog/2012/06/06/improving-ansi-term/

(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

(defun my-term-paste (&optional string)
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string (current-kill 0))))

(defun my-term-hook ()
  (goto-address-mode)
  (define-key term-raw-map "\C-y" 'my-term-paste))

(add-hook 'term-exec-hook 'my-term-use-utf8)
(add-hook 'term-mode-hook 'my-term-hook)

;; ----------------
;; EVIL

;; (add-hook 'term-mode-hook 'evil-emacs-state)


;; ------------------
;; LOAD CONFIGS

(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))
(load-directory "~/.emacs.d/config")
