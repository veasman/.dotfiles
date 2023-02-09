;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

(set-frame-parameter (selected-frame) 'alpha '(92 . 92))
(add-to-list 'default-frame-alist `(alpha . ,'(92 . 92)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

;; CODE ;;
;; Fira Code NF
;; DejaVuSansMono

;; ORG ;;
;; DejaVuSans
;; Cantarell

(setq doom-font (font-spec :family "DejaVuSansMono" :size 16)
      doom-variable-pitch-font (font-spec :family "DejaVuSans" :size 16)
      doom-big-font (font-spec :family "DejaVuSansMono" :size 24))

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq fancy-splash-image "/home/cvm/.config/doom/emacs.svg")

;; Turn off scroll accel
(setq mouse-wheel-progressive-speed nil)

;; Scrolloff cursor distance
(setq scroll-margin 8)

(beacon-mode 1)

(use-package blamer
  :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
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

(setq centaur-tabs-set-bar 'left
      centaur-tabs-gray-out-icons nil
      centaur-tabs-height 24
      centaur-tabs-set-icons t
      centaur-tabs-style "bar")

(map! :leader
      :desc "Toggle tabs globally" "t c" #'centaur-tabs-mode
      :desc "Toggle tabs local display" "t C" #'centaur-tabs-local-mode)

(evil-define-key 'normal centaur-tabs-mode-map (kbd "g <right>") 'centaur-tabs-forward        ; default Doom binding is 'g t'
                                               (kbd "g <left>")  'centaur-tabs-backward       ; default Doom binding is 'g T'
                                               (kbd "g <down>")  'centaur-tabs-forward-group
                                               (kbd "g <up>")    'centaur-tabs-backward-group)

(when (featurep! :tools lsp)
  (setq lsp-ui-doc-show-with-cursor nil)

  (setq-hook! 'lsp-mode-hook
    company-minimum-prefix-length 1
    company-idle-delay 0.1)

  (after! (lsp-ui doom-themes)
    (setq lsp-ui-imenu-colors `(,(doom-color 'dark-blue)
                                ,(doom-color 'cyan)))))

;; Enforce Google Java Code Style
;; See https://google.github.io/styleguide/javaguide.html
(when (featurep! :lang java)
  (when (featurep! :lang java +lsp)
    (setq lsp-java-format-settings-url "http://google.github.io/styleguide/eclipse-java-google-style.xml"))
  (set-formatter! 'google-java-format
    '("google-java-format" "-")
    :modes 'java-mode)
  (setq-hook! 'java-mode-hook
    tab-width 2
    fill-column 100))

(when (featurep! :lang java +lsp)
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
                  (expand-file-name (concat doom-private-dir
                                            "etc/lombok.jar")))
          lsp-java-vmargs)))

  ;; Groovy
  ;; (add-hook 'groovy-mode-local-vars-hook #'lsp!))

;; Insert name of current branch into start of commit message
;; Ex: master:
;; Or: JIT-899:
(defun cvm/commit-insert-ticket-name ()
  (insert (shell-command-to-string
           "git rev-parse --symbolic-full-name --abbrev-ref HEAD | tr -d '\n' | sed 's/$/: /'")))


(add-hook 'git-commit-setup-hook #'cvm/commit-insert-ticket-name)

(after! treemacs
  (setq treemacs-follow-mode t))

(after! doom-themes
  (setq doom-themes-treemacs-enable-variable-pitch t))

;; (defun cvm/treemacs-switch ()
;;   treemacs-display-current-project-exclusively)

;; (add-hook projectile-after-switch-project-hook #'cvm/treemacs-switch)

(map! :leader
      :desc "Org babel tangle" "m B" #'org-babel-tangle)

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
             "BLOG(b)"           ; Blog writing assignments
             "GYM(g)"            ; Things to accomplish at the gym
             "PROJ(p)"           ; A project that contains other tasks
             "VIDEO(v)"          ; Video assignments
             "WAIT(w)"           ; Something is holding up this task
             "|"                 ; The pipe necessary to separate "active" states and "inactive" states
             "DONE(d)"           ; Task has been completed
             "CANCELLED(c)" )))) ; Task has been cancelled

(add-hook 'org-mode-hook #'org-superstar-mode)

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

(defun cvm/org-colors-dracula ()
  "Enable Dracula colors for Org headers."
  (interactive)
  (dolist
      (face
       '((org-level-1 1.7 "#8be9fd" ultra-bold)
         (org-level-2 1.6 "#bd93f9" extra-bold)
         (org-level-3 1.5 "#50fa7b" bold)
         (org-level-4 1.4 "#ff79c6" semi-bold)
         (org-level-5 1.3 "#9aedfe" normal)
         (org-level-6 1.2 "#caa9fa" normal)
         (org-level-7 1.1 "#5af78e" normal)
         (org-level-8 1.0 "#ff92d0" normal)))
    (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
    (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

(defun cvm/org-colors-gruvbox-dark ()
  "Enable Gruvbox Dark colors for Org headers."
  (interactive)
  (dolist
      (face
       '((org-level-1 1.7 "#458588" ultra-bold)
         (org-level-2 1.6 "#b16286" extra-bold)
         (org-level-3 1.5 "#98971a" bold)
         (org-level-4 1.4 "#fb4934" semi-bold)
         (org-level-5 1.3 "#83a598" normal)
         (org-level-6 1.2 "#d3869b" normal)
         (org-level-7 1.1 "#d79921" normal)
         (org-level-8 1.0 "#8ec07c" normal)))
    (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
    (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

(defun cvm/org-colors-monokai-pro ()
  "Enable Monokai Pro colors for Org headers."
  (interactive)
  (dolist
      (face
       '((org-level-1 1.7 "#78dce8" ultra-bold)
         (org-level-2 1.6 "#ab9df2" extra-bold)
         (org-level-3 1.5 "#a9dc76" bold)
         (org-level-4 1.4 "#fc9867" semi-bold)
         (org-level-5 1.3 "#ff6188" normal)
         (org-level-6 1.2 "#ffd866" normal)
         (org-level-7 1.1 "#78dce8" normal)
         (org-level-8 1.0 "#ab9df2" normal)))
    (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
    (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

(defun cvm/org-colors-nord ()
  "Enable Nord colors for Org headers."
  (interactive)
  (dolist
      (face
       '((org-level-1 1.7 "#81a1c1" ultra-bold)
         (org-level-2 1.6 "#b48ead" extra-bold)
         (org-level-3 1.5 "#a3be8c" bold)
         (org-level-4 1.4 "#ebcb8b" semi-bold)
         (org-level-5 1.3 "#bf616a" normal)
         (org-level-6 1.2 "#88c0d0" normal)
         (org-level-7 1.1 "#81a1c1" normal)
         (org-level-8 1.0 "#b48ead" normal)))
    (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
    (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

(defun cvm/org-colors-oceanic-next ()
  "Enable Oceanic Next colors for Org headers."
  (interactive)
  (dolist
      (face
       '((org-level-1 1.7 "#6699cc" ultra-bold)
         (org-level-2 1.6 "#c594c5" extra-bold)
         (org-level-3 1.5 "#99c794" bold)
         (org-level-4 1.4 "#fac863" semi-bold)
         (org-level-5 1.3 "#5fb3b3" normal)
         (org-level-6 1.2 "#ec5f67" normal)
         (org-level-7 1.1 "#6699cc" normal)
         (org-level-8 1.0 "#c594c5" normal)))
    (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
    (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

(defun cvm/org-colors-palenight ()
  "Enable Palenight colors for Org headers."
  (interactive)
  (dolist
      (face
       '((org-level-1 1.7 "#82aaff" ultra-bold)
         (org-level-2 1.6 "#c792ea" extra-bold)
         (org-level-3 1.5 "#c3e88d" bold)
         (org-level-4 1.4 "#ffcb6b" semi-bold)
         (org-level-5 1.3 "#a3f7ff" normal)
         (org-level-6 1.2 "#e1acff" normal)
         (org-level-7 1.1 "#f07178" normal)
         (org-level-8 1.0 "#ddffa7" normal)))
    (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
    (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

(defun cvm/org-colors-solarized-dark ()
  "Enable Solarized Dark colors for Org headers."
  (interactive)
  (dolist
      (face
       '((org-level-1 1.7 "#268bd2" ultra-bold)
         (org-level-2 1.6 "#d33682" extra-bold)
         (org-level-3 1.5 "#859900" bold)
         (org-level-4 1.4 "#b58900" semi-bold)
         (org-level-5 1.3 "#cb4b16" normal)
         (org-level-6 1.2 "#6c71c4" normal)
         (org-level-7 1.1 "#2aa198" normal)
         (org-level-8 1.0 "#657b83" normal)))
    (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
    (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

(defun cvm/org-colors-solarized-light ()
  "Enable Solarized Light colors for Org headers."
  (interactive)
  (dolist
      (face
       '((org-level-1 1.7 "#268bd2" ultra-bold)
         (org-level-2 1.6 "#d33682" extra-bold)
         (org-level-3 1.5 "#859900" bold)
         (org-level-4 1.4 "#b58900" semi-bold)
         (org-level-5 1.3 "#cb4b16" normal)
         (org-level-6 1.2 "#6c71c4" normal)
         (org-level-7 1.1 "#2aa198" normal)
         (org-level-8 1.0 "#657b83" normal)))
    (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
    (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

(defun cvm/org-colors-tomorrow-night ()
  "Enable Tomorrow Night colors for Org headers."
  (interactive)
  (dolist
      (face
       '((org-level-1 1.7 "#81a2be" ultra-bold)
         (org-level-2 1.6 "#b294bb" extra-bold)
         (org-level-3 1.5 "#b5bd68" bold)
         (org-level-4 1.4 "#e6c547" semi-bold)
         (org-level-5 1.3 "#cc6666" normal)
         (org-level-6 1.2 "#70c0ba" normal)
         (org-level-7 1.1 "#b77ee0" normal)
         (org-level-8 1.0 "#9ec400" normal)))
    (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
    (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

;; Load our desired cvm/org-colors-* theme on startup
(after! org
  (cvm/org-colors-doom-one))

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

(defun cvm/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

;; Set the default number of workspaces
(setq exwm-workspace-number 9)

;; When window "class" updates, use it to set the buffer name
(add-hook 'exwm-update-class-hook #'cvm/exwm-update-class)

;; These keys should always pass through to Emacs
(setq exwm-input-prefix-keys
  '(?\C-x
    ?\C-u
    ?\C-d
    ?\C-h
    ?\M-`
    ?\M-&
    ?\M-:
    ?\C-\M-j ;; Buffer list
    ?\C-\ )) ;; Ctrl+SPC

;; Ctrl+Q will enable the next key to be sent directly
;; (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

;; Set up global key bindings. These always work, no matter the input state
;; Keep in mind that changing this list after EXWM initalizes has no effect
(setq exwm-input-global-keys
    `(
      ;; Reset to line-mode C-c C-k switches to char-mode via exwm-input-release-keyboard
      ([?\s-r] . exwm-reset)

      ;; Move between windows
      ([?\s-h] . windmove-left)
      ([?\s-l] . windmove-right)
      ([?\s-k] . windmove-up)
      ([?\s-j] . windmove-down)

      ;; Launch applications with shell command
      ([?\M-p] . (lambda (command)
                    (interactive (list (read-shell-command "$ ")))
                    (start-process-shell-command command nil command)))

      ;; Switch workspace
      ([?\s-w] . exwm-workspace-switch)

      ;; 'C-N': Switch to workspace at N
      ,@(mapcar (lambda (i)
                  `(,(kbd (format "C-%d" i)) .
                    (lambda ()
                      (interactive)
                      (exwm-workspace-switch-create ,i))))
                (number-sequence 0 9))))

(require 'exwm-randr)
(exwm-randr-enable)

(start-process-shell-command "xrandr" nil "xrandr --output default --mode 1080x1920 --primary --pos 1920x0")

(setq exwm-randr-workspace-monitor-plist
      '(1 "default"
        2 "default"
        3 "default"
        4 "default"
        5 "default"
        6 "default"
        7 "default"
        8 "default"
        9 "default"
        0 "default")
      exwm-workspace-warp-cursor t
      mouse-autoselect-window t
      focus-follows-mouse t)

(exwm-enable)

(use-package exwm-modeline
  :after (exwm)
  :config
  (setq exwm-modeline-dividers '("[" "] " "|")
        exwm-modeline-short t))

(add-hook 'exwm-init-hook #'exwm-modeline-mode)

;; (add-hook 'exwm-init-hook #'display-time-mode)

;; (setq display-time-24hr-format t ;; 24hr time format
      ;; display-time-day-and-date t ;; Show date and time
(setq doom-modeline-buffer-file-name-style 'auto
      doom-modeline-height 32)
      ;; doom-modeline-lsp nil ;; Disable LSP indicator
      ;; doom-modeline-time-icon nil) ;; Disable calendar icon next to time
