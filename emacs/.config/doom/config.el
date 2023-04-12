(setq fancy-splash-image "/home/cvm/.config/doom/emacs.svg")

;; CODE ;;
;; Fira Code NF
;; DejaVuSansMono

;; ORG ;;
;; DejaVuSans
;; Cantarell

(setq doom-font (font-spec :family "Fira Code NF" :size 16)
      doom-variable-pitch-font (font-spec :family "DejaVuSans" :size 16)
      doom-big-font (font-spec :family "Fira Code NF" :size 24))

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

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

(setq display-line-numbers-type 'relative)

;; Line numbers enable when needed
(dolist (mode '(text-mode-hook
               prog-mode-hook))
 (add-hook mode (lambda () (display-line-numbers-mode 'relative))))

;; Line numbers disable when needed
(dolist (mode '(org-mode-hook
               shell-mode-hook))
 (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))

;; Turn off scroll accel
(setq mouse-wheel-progressive-speed nil)

;; Scrolloff cursor distance
(setq scroll-margin 8)

;; Enable beacon
(beacon-mode 1)

(after! treemacs
  (setq treemacs-follow-mode t))

(after! doom-themes
  (setq doom-themes-treemacs-enable-variable-pitch t))

(use-package blamer
  :defer 20
  :custom
  (blamer-idle-time 0.5)
  (blamer-min-offset 0)
  (blamer-author-formatter "%s")
  (blamer-datetime-formatter ", %s ")
  (blamer-commit-formatter "● %s")
  (blamer-prettify-time-p t)
  ;; :custom-face
  ;; (blamer-face ((t :foreground "#505050"
  ;;                  :background nil
  ;;                  :italic t)))
  :init
  (setq global-blamer-mode 1)
  (add-hook 'org-mode-hook (lambda () (blamer-mode -1)))
  (map! :leader
        :prefix "g"
        :desc "Show commit info" "i" #'blamer-show-commit-info))

;; (custom-set-variables
;;  '(git-gutter:modified-sign " ")
;;  '(git-gutter:added-sign " ")
;;  '(git-gutter:deleted-sign "-"))

;; (set-face-background 'git-gutter:modified "orange")
;; (set-face-foreground 'git-gutter:modified "orange")
;; (set-face-background 'git-gutter:added "green")
;; (set-face-foreground 'git-gutter:added "green")
;; (set-face-background 'git-gutter:deleted "red")
;; (set-face-foreground 'git-gutter:deleted "red")

;; Insert name of current branch into start of commit message
;; Ex: master:
;; Or: JIT-899:
(defun cvm/commit-insert-ticket-name ()
  (insert (shell-command-to-string
           "git rev-parse --symbolic-full-name --abbrev-ref HEAD | tr -d '\n' | sed 's/$/: /'")))


(add-hook 'git-commit-setup-hook #'cvm/commit-insert-ticket-name)

;; (require 'oauth2)

;; (defvar org-jira-microsoft-client-id "<YOUR_CLIENT_ID>")
;; (defvar org-jira-microsoft-client-secret "<YOUR_CLIENT_SECRET>")
;; (defvar org-jira-microsoft-redirect-uri "<YOUR_REDIRECT_URI>")
;; (defvar org-jira-microsoft-token-url "https://login.microsoftonline.com/common/oauth2/token")
;; (defvar org-jira-microsoft-resource "https://your-site.atlassian.net")

;; (defun org-jira-microsoft-request-token ()
;;   (oauth2-auth-and-store
;;    org-jira-microsoft-token-url
;;    "https://login.microsoftonline.com/common/oauth2/authorize"
;;    org-jira-microsoft-redirect-uri
;;    org-jira-microsoft-client-id
;;    org-jira-microsoft-client-secret
;;    org-jira-microsoft-resource))

;; (defun org-jira-microsoft-get (url)
;;   (let* ((token (org-jira-microsoft-request-token))
;;          (url-request-method "GET")
;;          (url-request-extra-headers
;;           `(("Authorization" . ,(concat "Bearer " (oauth2-token-access-token token)))
;;             ("Content-Type" . "application/json"))))
;;     (with-current-buffer (url-retrieve-synchronously url)
;;       (goto-char (point-min))
;;       (re-search-forward "^$")
;;       (delete-region (point) (point-min))
;;       (json-read))))

;; Better defaults
(after! (lsp-ui doom-themes)
  (when (modulep! :tools lsp)
    (setq lsp-ui-doc-show-with-cursor nil)
    (add-hook 'lsp-mode-hook
              (lambda ()
                (setq-local company-minimum-prefix-length 2)
                (setq-local company-idle-delay 0.0)))
    (setq lsp-ui-imenu-colors `(,(doom-color 'dark-blue)
                                ,(doom-color 'cyan)))))

(map! :map typescript-mode-map
      :leader
      :prefix "c"
      :desc "Execute code action" "a" #'lsp-execute-code-action)

;; Fix for NVM not loading
(setq exec-path (append exec-path '("~/.nvm/versions/node/v16.19.0/bin")))

;; Enforce Google Java Code Style
;; See https://google.github.io/styleguide/javaguide.html
;; (when (modulep! :lang java)
;;   (when (modulep! :lang java +lsp)
;;     (setq lsp-java-format-settings-url "http://google.github.io/styleguide/eclipse-java-google-style.xml"))
;;   (set-formatter! 'google-java-format
;;     '("google-java-format" "-")
;;     :modes 'java-mode)
;;   (setq-hook! 'java-mode-hook
;;     tab-width 4
;;     fill-column 100))

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

(when (modulep! :lang javascript)
  (add-hook 'html-mode-hook 'emmet-mode))

;; (use-package! lsp-tailwindcss)

(let ((node-path (expand-file-name "/home/cvm/.nvm/versions/node/v16.19.0/bin/node")))
  (setenv "PATH" (concat node-path ":" (getenv "PATH")))
  (setq exec-path (append `(,node-path) exec-path)))

;; Enable image functionality
(setq org-startup-with-inline-images t
      org-image-actual-width nil)

;; Org screenshots
(defun cvm/org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (buffer-file-name)
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_"))) ".png"))
  (call-process "import" nil nil nil filename)
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))

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

;; Sync clipboard
(defun cvm/copy-selected-text(start end)
  (interactive "r")
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties start end)))
        (shell-command (concat "echo '" text "' | clip.exe")))))
