;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :config
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun cvm/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(package! exwm
  :recipe
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 9)

  (exwm-enable)

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
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

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
        ([?\s-p] . (lambda (command)
                      (interactive (list (read-shell-command "$ ")))
                      (start-process-shell-command command nil command)))

        ;; Switch workspace
        ([?\s-w] . exwm-workspace-switch)

        ;; 's-N': Switch to workspace at N
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))))

  (require 'exwm-randr)
  (exwm-randr-enable)
  (start-process-shell-command "xrandr" nil "xrandr --output Virtual1 --primary --mode 1920x1080 -pos 1920x0 --rotate normal --output Virtual2 --mode 1920x1080 --pos 0x0 --rotate normal")

  (setq exwm-randr-workspace-monitor-plist '(1 "Virtual2" 2 "Virtual2" 3 "Virtual2" 4 "Virtual2" 5 "Virtual2"))

  (setq exwm-workspace-warp-cursor t)

  (setq mouse-autoselect-window t
	focus-follows-mouse t)

  (exwm-enable))
