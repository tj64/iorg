;; * iorg-init.el --- init file for Emacs(server) used by iOrg
;; ** MetaData
;;   :PROPERTIES:
;;   :copyright: Thorsten_Jolitz
;;   :copyright-since: 2013
;;   :version:  0.9
;;   :licence:  GPL3+
;;   :licence-url: http://www.gnu.org/licenses/
;;   :part-of-emacs: no
;;   :git-repo: https://github.com/tj64/iorg
;;   :git-clone: git@github.com:tj64/iorg.git
;;   :authors: Thorsten_Jolitz
;;   :contact: <tjolitz@gmail.com>
;;   :inspiration:  org-mode picolisp
;;   :keywords: emacs org-mode dhtml interactive_web_applications
;;   :END:


;; * Coding system

(message "ENTERING iorg-init ...")

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; * Start Server

(setq server-name "iorg-server")
(server-start)

;; * Screen Colors

;; window type dependend screen colors
(defun iorg-set-window-type-dependend-emacsclient-colors ()
    "Sets emacsclient colors for X.
Background-color is set to black and foreground-color to wheat for
emacsclient on X"
    (if (display-graphic-p)
        (progn
          (set-background-color "black")
          (set-foreground-color "wheat"))))

(add-hook 'server-visit-hook
          'iorg-set-window-type-dependend-emacsclient-colors)

(add-hook 'emacs-startup-hook
          'iorg-set-window-type-dependend-emacsclient-colors)


;; * Requires

;; ;; remember this directory
;; (setq iorg-el-dir
;;       (file-name-directory (or load-file-name (buffer-file-name))))

;; (message "iOrg Directory: %s" iorg-el-dir)

;; require libs
(require 'iorg)
(require 'iorg-scrape)
(require 'ox-iorg)
;; (require 'iorg-agenda)
;; (require 'iorg-dired)

;; set load-path to 'emacs-w3m'
(add-to-list 'load-path (expand-file-name "~/gitclone/emacs-w3m/"))
(require 'w3m-form-patched nil 'NOERROR)

;; * General settings

;; disable menubar
(menu-bar-mode -1)
;; disable scrollbar
(scroll-bar-mode -1)
;; disable toolbar
(tool-bar-mode -1)
;; syntax highlighting
(global-font-lock-mode t)
;; entry deletes marked text
(delete-selection-mode t)
;; match parentheses
(show-paren-mode t)
;; wrap long lines in text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; enable "dired on steroids"
(ido-mode t)
;; ;; Enable versioning with default values.
;; (setq version-control t)
;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; * Run hooks and provide

(provide 'iorg-init)

;; * Custom Set Variables/Fonts

(message "FINALLY Custom vars/fonts ...")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 120 :width normal))))
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button)))) t))

