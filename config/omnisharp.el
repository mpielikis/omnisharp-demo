(require 'omnisharp)
(require 'company)


;; -------------------------
;; COMPANY CONFIG. OMNISHARP

(setq omnisharp-company-do-template-completion t)

(custom-set-variables
 '(omnisharp-auto-complete-want-documentation nil)
 '(omnisharp-company-sort-results t)
 '(omnisharp-server-executable-path
   (quote OmniSharp\.exe)))

(define-key company-active-map (kbd ".") (lambda() (interactive) (company-complete-selection-insert-key-and-complete '".")))
(define-key company-active-map (kbd "]") (lambda() (interactive) (company-complete-selection-insert-key-and-complete '"]")))
(define-key company-active-map (kbd "[") (lambda() (interactive) (company-complete-selection-insert-key '"[")))
(define-key company-active-map (kbd ")") (lambda() (interactive) (company-complete-selection-insert-key '")")))
(define-key company-active-map (kbd "<SPC>") nil)
(define-key company-active-map (kbd ";") (lambda() (interactive) (company-complete-selection-insert-key '";")))
(define-key company-active-map (kbd ">") (lambda() (interactive) (company-complete-selection-insert-key '">")))

(define-key omnisharp-mode-map (kbd "}") 'csharp-indent-function-on-closing-brace) 
(define-key omnisharp-mode-map (kbd "<RET>") 'csharp-newline-and-indent) 

(define-key omnisharp-mode-map (kbd "<f12>") 'omnisharp-go-to-definition)
(define-key omnisharp-mode-map (kbd "s-d") 'omnisharp-go-to-definition)
;; (define-key omnisharp-mode-map (kbd "C-ot ") 'omnisharp-go-to-definition)
;; (define-key omnisharp-mode-map (kbd "s-f") 'omnisharp-navigate-up)
;; (define-key omnisharp-mode-map (kbd "s-b") 'omnisharp-navigate-down)
(define-key omnisharp-mode-map (kbd "S-<f12>") 'omnisharp-helm-find-usages)

;; (define-key omnisharp-mode-map (kbd "s-u") 'omnisharp-helm-find-usages)
;; (define-key omnisharp-mode-map (kbd "s-i") 'omnisharp-helm-find-implementations)
;; (define-key omnisharp-mode-map (kbd "S-s-<f12>") 'omnisharp-helm-find-usages)
;; (define-key omnisharp-mode-map (kbd "<M-RET>") 'omnisharp-run-code-action-refactoring)
(define-key omnisharp-mode-map (kbd "C-.") 'omnisharp-run-code-action-refactoring)

;; (define-key omnisharp-mode-map (kbd "C-d") 'duplicate-current-line-or-region)

(define-key omnisharp-mode-map (kbd "<f2>") 'omnisharp-rename-interactively)
(define-key omnisharp-mode-map (kbd "<f5>") 'omnisharp-build-in-emacs)

;; ;; disable emacs ctrl-r key.... we need it for VS shortcuts
;; (global-unset-key "\C-r")
(define-key omnisharp-mode-map (kbd "s-t s-t") 'omnisharp-unit-test-single)
(define-key omnisharp-mode-map (kbd "s-t s-f") 'omnisharp-unit-test-fixture)
(define-key omnisharp-mode-map (kbd "s-t s-a") 'omnisharp-unit-test-all)
;;(define-key omnisharp-mode-map (kbd "s-r s-l") 'recompile)
(define-key omnisharp-mode-map (kbd "s-r s-r") 'omnisharp-rename)

;; (define-key omnisharp-mode-map (kbd "<M-RET>") 'omnisharp-run-code-action-refactoring)
;; (define-key omnisharp-mode-map (kbd "<C-.>") 'omnisharp-run-code-action-refactoring)

;; ;; disable emacs ctrl-k key.... we need it for VS shortcuts
;; (global-unset-key "\C-k")
(define-key omnisharp-mode-map (kbd "s-c s-f") 'omnisharp-code-format)

(global-set-key (kbd "s-c s-c") 'comment-or-uncomment-region-or-line)
;;(global-set-key (kbd "s-c s-u") 'comment-or-uncomment-region-or-line)

;; (require 'csharp-mode)

(defun my-csharp-mode ()
  (idle-highlight-mode)
  (yas-minor-mode)
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)
  (linum-mode)
  (whole-line-or-region-mode)
  (electric-pair-mode)
  (add-to-list 'company-backends 'company-omnisharp)
  (setq c-basic-offset 4) ; indents 4 chars
  (setq tab-width 4)          ; and 4 char wide for TAB
  (setq indent-tabs-mode nil) ; And force use of spaces
  (turn-on-eldoc-mode))

  (setq eldoc-idle-delay 0.1
      flycheck-display-errors-delay 0.2)

(setq omnisharp-company-strip-trailing-brackets nil) ; ?
(setq omnisharp-company-match-type 'company-match-flx)

(add-hook 'csharp-mode-hook 'my-csharp-mode)


;; NOTIFY FILE CHANGED
;; -------------------

(add-hook 'js-mode-hook 
          (lambda () 
             (add-hook 'after-save-hook 'omnisharp-notify-file-changed)))

(defun omnisharp-notify-file-changed ()
  (interactive)
  (if (string-equal (file-name-nondirectory buffer-file-name) "project.json")
                   (omnisharp-post-message-curl-as-json
                    (concat (omnisharp-get-host) "filesChanged") 
                    (list (omnisharp--get-common-params))
                    )))


(defun omnisharp-unit-test (mode)
  "Run tests after building the solution. Mode should be one of 'single', 'fixture' or 'all'" 
  (interactive)
  (let ((test-response
         (omnisharp-post-message-curl-as-json
          (concat (omnisharp-get-host) "gettestcontext") 
          (cons `("Type" . ,mode) (omnisharp--get-common-params)))))
    (let ((test-command
           (cdr (assoc 'TestCommand test-response)))

          (test-directory
           (cdr (assoc 'Directory test-response))))
      (cd test-directory)
      (compile test-command))))


;; GENERAL
;; -------

(setq-default cursor-type 'bar)
(setq gc-cons-threshold 20000000) ; faster emacs

;; -------------------
;; HELPERS: Are not used

(defun csharp-newline-and-indent ()
  "Open a newline and indent.
If point is between a pair of braces, opens newlines to put braces
on their own line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (and
             (looking-at " *}")
             (save-match-data
               (when (looking-back "{ *")
                 (goto-char (match-beginning 0))
                 (unless (looking-back "^[[:space:]]*")
                   (newline-and-indent))
                 t)))
        (unless (and (boundp electric-pair-open-newline-between-pairs)
                     electric-pair-open-newline-between-pairs
                     electric-pair-mode)
          (goto-char (match-beginning 0))
          (newline-and-indent)))))
  (newline-and-indent)) 

(defun csharp-indent-function-on-closing-brace()
  (interactive)
  (insert "}")
  (c-indent-defun))

(defun company-complete-selection-insert-key(company-key)
  (company-complete-selection)
  (insert company-key))

(defun company-complete-selection-insert-key-and-complete(company-key)
  (company-complete-selection-insert-key company-key)
  (company-complete))

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))
