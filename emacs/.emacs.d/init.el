(use-package emacs
  :bind
  ("M-<tab>" . completion-at-point)
  ("C-c d" . duplicate-line)
  ("C-c f f" . find-file)
  ("C-c f a" . format-all-buffer)
  ("C-c e r" . eval-region)
  ("C-c e b" . eval-buffer)
  ("C-c w m" . whitespace-mode)

  :init
  (setq create-lockfiles nil
      	make-backup-files nil
      	custom-theme-directory "~/.emacs.d/themes"
      	inhibit-startup-message t
      	inhibit-startup-screen t
      	initial-scratch-message ";;; Emacs is fun"
      	global-auto-revert-non-file-buffers t)
  
  (fset 'yes-or-no-p 'y-or-n-p)
  (auto-save-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (save-place-mode 1)
  (global-auto-revert-mode 1)

  ;; cursor
  ;;(setq-default cursor-type 'box)

  ;; line numbers
  ;; (setq display-line-numbers-type 'relative)
  ;; (global-display-line-numbers-mode)

  (load-theme 'doom-bluloco-light t)

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
      		  (replace-regexp-in-string
      		   "\`\[.*?]\*\|\[.*?]\*\'" ""
      		   crm-separator)
      		  (car args))
      	  (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
      	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  :config
  (add-to-list 'default-frame-alist
               '(font . "Iosevka Nerd Font-15"))
  (which-key-mode 1)

  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package ace-window
  :defer t
  :bind
  (("M-o" . ace-window))
  :init
  (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?f)))

(use-package avy
  :defer t
  :bind
  (("M-g f" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("C-'" . avy-goto-char-2)))

(use-package cape
  :defer t
  :bind ("M-p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-abbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-emoji)
  ;;(add-hook 'completion-at-point-functions #'cape-dict)
  (add-hook 'completion-at-point-functions #'cape-rfc1345)
  (add-hook 'completion-at-point-functions #'cape-sgml)
  (add-hook 'completion-at-point-functions #'cape-tex)
  (add-hook 'completion-at-point-functions #'cape-history))

(use-package consult
  :defer t
  :bind
  (("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ([remap Info-search] . consult-info)
   ;; C-x bindings in `ctl-x-map'
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ;; Other custom bindings
   ("M-y" . consult-yank-pop)                ;; orig. yank-pop
   ;; M-g bindings in `goto-map'
   ("M-g e" . consult-compile-error)
   ;;("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ;; M-s bindings in `search-map'
   ("M-s d" . consult-find)                  ;; Alternative: consult-fd
   ("M-s c" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)                 ;; orig. next-matching-history-element
   ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :config
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source))

(use-package corfu
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1))
  :config
  (global-corfu-mode)
  :custom
  (corfu-auto nil)
  (corfu-echo-documentation nil)
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil))

(use-package dashboard
  :config
  (setq dashboard-projects-backend 'project-el)

  (setq dashboard-items '((recents   . 10)
      			  (bookmarks . 10)
      			  (projects  . 10)
      			  (agenda    . 10)
      			  (registers . 10)))

  (setq dashboard-item-shortcuts '((recents   . "r")
      				   (bookmarks . "m")
      				   (projects  . "p")
      				   (agenda    . "a")
      				   (registers . "e")))

  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))

  (dashboard-setup-startup-hook))

(use-package dired
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package direnv
  :defer t
  :config
  (direnv-mode))

(defcustom container-executable 'podman
  "The executable to be used with docker mode."
  :type '(choice
      	  (const :tag "docker" docker)
      	  (const :tag "podman" podman))
  :group 'custom)

(use-package docker
  :defer t
  :bind
  ;;("C-c d" . docker)
  :config
  (pcase container-executable
    ('docker
     (setf docker-command "docker"
      	   docker-compose-command "docker-compose"
      	   docker-container-tramp-method "docker"))
    ('podman
     (setf docker-command "podman"
      	   docker-compose-command "podman-compose"
      	   docker-container-tramp-methodu "podman"))))

(use-package doom-modeline
  :defer t
  :init
  (setq doom-modeline-time t
      	doom-modeline-env-version t)

  (doom-modeline-mode 1))

(use-package doom-themes)

(use-package eglot
  :hook
  ((python-ts-mode . eglot-ensure)
   (python-mode . eglot-ensure))
  :config
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil))



(use-package electric
  :defer t
  :init
  ;;(setq electric-pair-preserve-balance nil)
  (electric-pair-mode)

  :config
  (defvar latex-mode-electric-pairs '((?$ . ?$))
    "Electric pairs for LaTeX mode.")

  (defvar org-mode-electric-pairs '(())
    "Electric pairs for org mode.")

  (defun latex-mode-add-electric-pairs ()
    "Add electric pairs for LaTeX mode."
    (setq-local electric-pair-pairs (append electric-pair-pairs latex-mode-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs)
    (message "Electric pairs added for LaTeX mode: %s" electric-pair-pairs))

  (defun org-mode-add-electric-pairs ()
    "Add electric pairs for org mode."
    (setq-local electric-pair-pairs (append electric-pair-pairs
  					    org-mode-electric-pairs
  					    latex-mode-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs)
    (message "Electric pairs added for org mode: %s" electric-pair-pairs))

  :hook
  (latex-mode . latex-mode-add-electric-pairs)
  (org-mode . org-mode-add-electric-pairs))

(use-package embark
  :bind
  ("C-." . embark-act)
  ("M-." . embark-dwim))

(use-package em-banner)

(use-package flycheck
  :defer t
  :hook
  (after-init . global-flycheck-mode))

(use-package flycheck-inline
  :defer t
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)))

(use-package format-all)

(use-package magit)

(use-package marginalia
  :bind (:map minibuffer-local-map
      	      ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package move-text
  :init
  (move-text-default-bindings))

(use-package nix-mode
  :mode "\.nix\'")

(use-package nyan-mode
  :init
  (setq nyan-animate-nyancat t
      	nyan-wavy-trail t)
  (nyan-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless flex))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package org
  :defer t
  :bind
  ("C-M-<return>" . org-insert-subheading)
  :init
  (setq org-attach-id-dir "~/org/.attach"
      	org-log-done 'time
      	org-hide-emphasis-markers t
      	org-imenu-depth 7)

  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t)
      							   (emacs-lisp . t)
      							   (python . t))))

(use-package org-roam
  :defer t
  :custom
  (org-roam-directory (concat org-directory "/roam"))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  ;;(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-modern
  :config
  (with-eval-after-load 'org (global-org-modern-mode)))

(use-package org-superstar
  :defer t
  :hook
  (org-mode . (lambda () (org-superstar-mode 1))))

(use-package org-present)

(use-package perspective
  :defer t
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode))

(use-package salt-mode
  :defer t
  :hook
  (salt-mode . (lambda () (flyspell-mode 1))))

(use-package spacious-padding
  :defer t
  :init
  (setq spacious-padding-widths
      	'( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8
           :fringe-width 8))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible and provides several examples.
  (setq spacious-padding-subtle-mode-line
      	`( :mode-line-active 'default
           :mode-line-inactive vertical-border))

  (spacious-padding-mode 1))

(use-package savehist
  :init
  (savehist-mode))

(use-package treesit
  :init
  (setq major-mode-remap-alist
      	'((bash-mode . bash-ts-mode)
      	  (python-mode . python-ts-mode))))

(use-package vertico
  :defer t
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package yasnippet
  :defer t
  :config
  (yas-global-mode 1))

(use-package zellij :defer t)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tuxikus/get-jira-ticket-number (branch)
  (when (string-match "[A-Z]\{8\}-[0-9]*" branch)
    (message (match-string 0 branch))))

(add-hook 'git-commit-setup-hook '(lambda () (insert (tuxikus/get-jira-ticket-number (magit-get-current-branch)))))

(defun tuxikus/get-bookmarks-from-file ()
  "Get bookmarks from the bookmark file"
  (with-temp-buffer
    (insert-file-contents "~/.bookmarks.org")
    (org-mode)
    (let (bookmarks)
      (org-element-map (org-element-parse-buffer) 'link
    	(lambda (l)
    	  (let* ((link (org-element-property :raw-link l))
    		 (name (org-element-interpret-data (org-element-contents l)))
    		 (tags (org-element-property :tags (org-element-property :parent l))))
    	    (push (concat name
    			  "
"
    			  link
    			  "
"
    			  (format "[%s]" (mapconcat #'identity tags ", "))) bookmarks))))
      bookmarks)))

(defun tuxikus/add-bookmark ()
  "Add a new bookmark to the bookmark file."
  (interactive)
  (let* ((title (read-from-minibuffer "Title: "))
    	 (url (read-from-minibuffer "URL: "))
    	 (tags (read-from-minibuffer "Tags: ")))
    (write-region (format "* [[%s][%s]] %s
" url title tags) nil "~/.bookmarks.org" 'append)))

(defun tuxikus/edit-bookmark ()
  "TODO implement."
  (interactive)
  (message "Not implemented."))

(defun tuxikus/delete-bookmark ()
  "TODO implement."
  (interactive)
  (message "Not implemented."))

(defun tuxikus/open-bookmark ()
  "Select a bookmark and open it."
  (interactive)
  (browse-url
   (seq-elt (split-string
    	     (completing-read "Open: " (tuxikus/get-bookmarks-from-file))
    	     "
") 1)))

(defun tuxikus/change-org-directory ()
  "Change the active org directory."
  (interactive)
  (let ((selection (completing-read "Select: " '("~/org" "~/org-edu"))))
    (setq org-directory selection
    	  org-attach-id-dir (concat org-directory "/.attach")
    	  org-roam-directory (concat org-directory "/roam")
    	  org-roam-db-location (concat org-directory "/org-roam.db"))))
