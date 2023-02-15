(setq fancy-splash-image "/home/cvm/.config/doom/emacs.svg")

;; CODE ;;
;; Fira Code NF
;; DejaVuSansMono

;; ORG ;;
;; DejaVuSans
;; Cantarell

(setq doom-font (font-spec :family "Fira Code NF" :size 16)
      doom-variable-pitch-font (font-spec :family "DejaVuSans" :size 16))

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq display-line-numbers-type 'relative)

;; Line numbers enable when needed
(dolist (mode '(text-mode-hook
               prog-mode-hook
               conf-mode-hook))
 (add-hook mode (lambda () (display-line-numbers-mode 'relative))))

;; Line numbers disable when needed
(dolist (mode '(org-mode-hook
               term-mode-hook
               shell-mode-hook
               eshell-mode-hook))
 (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Turn off scroll accel
(setq mouse-wheel-progressive-speed nil)

;; Scrolloff cursor distance
(setq scroll-margin 8)

;; Enable beacon
(beacon-mode 1)

(use-package blamer
  :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.5)
  (blamer-min-offset 0)
  (blamer-author-formatter "%s")
  (blamer-datetime-formatter ", %s ")
  (blamer-commit-formatter "● %s")
  (blamer-prettify-time-p t)
  :custom-face
  (blamer-face ((t :foreground "#505050"
                   :background nil
                   :italic t)))
  :config
  (global-blamer-mode 1))

(when (modulep! :tools lsp)
  (setq lsp-ui-doc-show-with-cursor nil)

  (setq-hook! 'lsp-mode-hook
    company-minimum-prefix-length 1
    company-idle-delay 0.1)

  (after! (lsp-ui doom-themes)
    (setq lsp-ui-imenu-colors `(,(doom-color 'dark-blue)
                                ,(doom-color 'cyan)))))

;; Fix for NVM not loading
(setq exec-path (append exec-path '("~/.nvm/versions/node/v16.19.0/bin")))

;; Enforce Google Java Code Style
;; See https://google.github.io/styleguide/javaguide.html
(when (modulep! :lang java)
  (when (modulep! :lang java +lsp)
    (setq lsp-java-format-settings-url "http://google.github.io/styleguide/eclipse-java-google-style.xml"))
  (set-formatter! 'google-java-format
    '("google-java-format" "-")
    :modes 'java-mode)
  (setq-hook! 'java-mode-hook
    tab-width 2
    fill-column 100))

(when (modulep! :lang java +lsp)
  (setq lsp-java-maven-download-sources t
        lsp-java-autobuild-enabled nil
        lsp-java-selection-enabled nil
        lsp-java-code-generation-use-blocks t
        lsp-java-code-generation-generate-comments t
        lsp-java-code-generation-to-string-code-style "STRING_BUILDER")

  ;; Lombok support
  ;; See https://github.com/redhat-developer/vscode-java/wiki/Lombok-support
  (after! lsp-java
    (push (concat "-javaagent:"
                  (expand-file-name (concat doom-user-dir
                                            "etc/lombok.jar")))
          lsp-java-vmargs)))

  ;; (add-hook 'groovy-mode-local-vars-hook #'lsp!))

;; Insert name of current branch into start of commit message
;; Ex: master:
;; Or: JIT-899:
(defun cvm/commit-insert-ticket-name ()
  (insert (shell-command-to-string
           "git rev-parse --symbolic-full-name --abbrev-ref HEAD | tr -d '\n' | sed 's/$/: /'")))


(add-hook 'git-commit-setup-hook #'cvm/commit-insert-ticket-name)

;; WSLG clipboard fix
(defun cvm/copy-selected-text(start end)
  (interactive "r")
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties start end)))
        (shell-command (concat "echo '" text "' | clip.exe")))))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(after! org
  (setq org-directory "~/.doom.d/OrgFiles/"
        org-agenda-files '("~/.doom.d/OrgFiles/Agenda.org")
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
        org-log-done 'time
        org-hide-emphasis-markers t
        org-table-convert-region-max-lines 20000
        org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
          '((sequence
             "TODO(t)"           ; A task that is ready to be tackled
             "PROJ(p)"           ; A project that contains other tasks
             "VIDEO(v)"          ; Video assignments
             "WAIT(w)"           ; Something is holding up this task
             "|"                 ; The pipe necessary to separate "active" states and "inactive" states
             "DONE(d)"           ; Task has been completed
             "CANCELLED(c)" )))) ; Task has been cancelled

(add-hook 'org-mode-hook #'org-superstar-mode)

(defun cvm/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(add-hook 'org-mode-hook #'cvm/org-mode-visual-fill)

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(defun cvm/org-colors-doom-one ()
  "Enable Doom One colors for Org headers."
  (interactive)
  (dolist
      (face
       '((org-level-1 1.7 "#51afef" extra-bold)
         (org-level-2 1.6 "#c678dd" bold)
         (org-level-3 1.5 "#98be65" semi-bold)
         (org-level-4 1.4 "#da8548" normal)
         (org-level-5 1.3 "#5699af" normal)
         (org-level-6 1.2 "#a9a1e1" normal)
         (org-level-7 1.1 "#46d9ff" normal)
         (org-level-8 1.0 "#ff6c6b" normal)))
    (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
    (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

(after! org
  (cvm/org-colors-doom-one))

(after! treemacs
  (setq treemacs-follow-mode t))

(after! doom-themes
  (setq doom-themes-treemacs-enable-variable-pitch t))

;; (defun cvm/treemacs-switch ()
;;   treemacs-display-current-project-exclusively)

;; (add-hook projectile-after-switch-project-hook #'cvm/treemacs-switch)
