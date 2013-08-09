;; * iorg-dired.el --- dired-like mode for iOrg
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
;;   :keywords: emacs org-mode picolisp iorg dired
;;   :END:

;; ** Commentary
;; *** About iorg-dired
;; *** Installation
;; *** Bugs and Shortcomings
;; *** Emacs Version
;; ** ChangeLog
;; * Requires
;; * Mode and Exporter definitions
;; ** Mode definitions
;; *** iOrg Dired Mode

(define-derived-mode iorg-dired-mode
  fundamental-mode "iOrg Dired"
  ;; inferior-picolisp-mode "Quick Scrape"
  "Dired-like major-mode for iOrg.

The following commands are available:
\\{iorg-dired-mode-map}
\\{iorg-dired-run}
\\{iorg-dired-expect}
\\{iorg-dired-click}
\\{iorg-dired-press}
\\{iorg-dired-value}
\\{iorg-dired-enter}
\\{iorg-dired-display}
\\{iorg-dired-display-fields}
\\{iorg-dired-display-all}

Don't use the commands from parent-mode `dired' in this mode -
they won't work.")
(put 'iorg-dired-mode 'mode-class 'special)

;; * Variables
;; ** Hooks
;; ** Vars
;; ** Customs
;; *** Custom Groups
;; *** Custom Vars
;; * Functions
;; ** Non-interactive Functions
;; ** Commands

;; *** Operating on the marked files
 
(defun iorg-dired-do-search ()
  ""
  (interactive))

(defun iorg-dired-do-copy ()
  ""
  (interactive))

(defun iorg-dired-do-delete ()
  ""
  (interactive))

(defun iorg-dired-do-print ()
  ""
  (interactive))

(defun iorg-dired-do-query-replace-regexp ()
  ""
  (interactive))

(defun iorg-dired-do-rename ()
  ""
  (interactive))

(defun iorg-dired-do-touch ()
  ""
  (interactive))

(defun iorg-dired-do-shell-command ()
  ""
  (interactive))

(defun iorg-dired-do-compress ()
  ""
  (interactive))

(defun iorg-dired-do-shell-command ()
  ""
  (interactive))

(defun iorg-dired-do-async-shell-command ()
  ""
  (interactive))

;; *** Comparison commands

(defun iorg-dired-diff ()
  ""
  (interactive))

;; *** Move to marked files

(defun iorg-dired-prev-marked-file ()
  ""
  (interactive))

(defun iorg-dired-next-marked-file ()
  ""
  (interactive))

;; *** Regexp commands

(defun iorg-dired-upcase ()
  ""
  (interactive))

(defun iorg-dired-downcase ()
  ""
  (interactive))

(defun iorg-dired-flag-files-regexp ()
  ""
  (interactive))

(defun iorg-dired-mark-files-containing-regexp ()
  ""
  (interactive))

(defun iorg-dired-mark-files-regexp ()
  ""
  (interactive))

(defun iorg-dired-do-rename-regexp ()
  ""
  (interactive))

(defun iorg-dired-do-copy-regexp ()
  ""
  (interactive))

(defun iorg-dired-do-hardlink-regexp ()
  ""
  (interactive))

(defun iorg-dired-do-rename-regexp ()
  ""
  (interactive))

(defun iorg-dired-do-symlink-regexp ()
  ""
  (interactive))

(defun iorg-dired-flag-garbage-files ()
  ""
  (interactive))

;; *** Commands for marking and unmarking

(defun iorg-dired-mark-files-regexp ()
  ""
  (interactive))

(defun iorg-dired-change-marks ()
  ""
  (interactive))

(defun iorg-dired-mark ()
  ""
  (interactive))

(defun iorg-dired-unmark ()
  ""
  (interactive))

(defun iorg-dired-unmark-all-files ()
  ""
  (interactive))

(defun iorg-dired-unmark-all-marks ()
  ""
  (interactive))

(defun iorg-dired-unmark-all-marks ()
  ""
  (interactive))

(defun iorg-dired-unmark-backward ()
  ""
  (interactive))

(defun iorg-dired-next-marked-file ()
  ""
  (interactive))

(defun iorg-dired-prev-marked-file ()
  ""
  (interactive))

(defun iorg-dired-toggle-marks ()
  ""
  (interactive))


;; *** Commands not operating on all marked files

(defun iorg-dired-find-alternate-file ()
  ""
  (interactive))

(defun iorg-dired-flag-file-deletion ()
  ""
  (interactive))

(defun iorg-dired-find-file ()
  ""
  (interactive))

(defun revert-buffer ()
  ""
  (interactive))

(defun iorg-dired-goto-file ()
  ""
  (interactive))

(defun iorg-dired-do-kill-lines ()
  ""
  (interactive))

(defun iorg-dired-do-redisplay ()
  ""
  (interactive))

(defun iorg-dired-next-line ()
  ""
  (interactive))

(defun iorg-dired-find-file-other-window ()
  ""
  (interactive))

(defun iorg-dired-display-file ()
  ""
  (interactive))

(defun iorg-dired-previous-line ()
  ""
  (interactive))

(defun iorg-dired-sort-toggle-or-edit ()
  ""
  (interactive))

(defun iorg-dired-toggle-marks ()
  ""
  (interactive))

(defun iorg-dired-view-file ()
  ""
  (interactive))

(defun iorg-dired-copy-filename-as-kill ()
  ""
  (interactive))

(defun iorg-dired-do-flagged-delete ()
  ""
  (interactive))

;; *** Moving

(defun iorg-dired-next-line ()
  ""
  (interactive))

(defun iorg-dired-previous-line ()
  ""
  (interactive))

;; *** Hiding

(defun iorg-dired-hide-subdir ()
  ""
  (interactive))

(defun iorg-dired-hide-all ()
  ""
  (interactive))


;; *** iSearch

(defun iorg-dired-do-isearch ()
  ""
  (interactive))

(defun iorg-dired-do-isearch-regexp ()
  ""
  (interactive))

(defun iorg-dired-isearch-filenames ()
  ""
  (interactive))

(defun iorg-dired-isearch-filenames-regexp ()
  ""
  (interactive))

;; *** Misc

(defun iorg-dired-toggle-read-only ()
  ""
  (interactive))

(defun iorg-dired-toggle-read-only ()
  ""
  (interactive))

(defun iorg-dired-summary ()
  ""
  (interactive))

(defun iorg-dired-unmark-backward ()
  ""
  (interactive))

(defun iorg-dired-undo ()
  ""
  (interactive))

(defun iorg-dired-undo ()
  ""
  (interactive))


;; * Menus and Keys
;; ** Menus
;; ** Keys

;; Upper case keys (except !) for operating on the marked files
(define-key map "A" 'iorg-dired-do-search)
(define-key map "C" 'iorg-dired-do-copy)
(define-key map "D" 'iorg-dired-do-delete)
(define-key map "P" 'iorg-dired-do-print)
(define-key map "Q" 'iorg-dired-do-query-replace-regexp)
(define-key map "R" 'iorg-dired-do-rename)
(define-key map "T" 'iorg-dired-do-touch)
(define-key map "X" 'iorg-dired-do-shell-command)
(define-key map "Z" 'iorg-dired-do-compress)
(define-key map "!" 'iorg-dired-do-shell-command)
(define-key map "&" 'iorg-dired-do-async-shell-command)
;; Comparison commands
(define-key map "=" 'iorg-dired-diff)
;; move to marked files
(define-key map "\M-{" 'iorg-dired-prev-marked-file)
(define-key map "\M-}" 'iorg-dired-next-marked-file)
;; Make all regexp commands share a `%' prefix:
;; We used to get to the submap via a symbol iorg-dired-regexp-prefix,
;; but that seems to serve little purpose, and copy-keymap
;; does a better job without it.
(define-key map "%" nil)
(define-key map "%u" 'iorg-dired-upcase)
(define-key map "%l" 'iorg-dired-downcase)
(define-key map "%d" 'iorg-dired-flag-files-regexp)
(define-key map "%g" 'iorg-dired-mark-files-containing-regexp)
(define-key map "%m" 'iorg-dired-mark-files-regexp)
(define-key map "%r" 'iorg-dired-do-rename-regexp)
(define-key map "%C" 'iorg-dired-do-copy-regexp)
(define-key map "%H" 'iorg-dired-do-hardlink-regexp)
(define-key map "%R" 'iorg-dired-do-rename-regexp)
(define-key map "%S" 'iorg-dired-do-symlink-regexp)
(define-key map "%&" 'iorg-dired-flag-garbage-files)
;; Commands for marking and unmarking.
(define-key map "*" nil)
(define-key map "*%" 'iorg-dired-mark-files-regexp)
(define-key map "*c" 'iorg-dired-change-marks)
(define-key map "*m" 'iorg-dired-mark)
(define-key map "*u" 'iorg-dired-unmark)
(define-key map "*?" 'iorg-dired-unmark-all-files)
(define-key map "*!" 'iorg-dired-unmark-all-marks)
(define-key map "U" 'iorg-dired-unmark-all-marks)
(define-key map "*\177" 'iorg-dired-unmark-backward)
(define-key map "*\C-n" 'iorg-dired-next-marked-file)
(define-key map "*\C-p" 'iorg-dired-prev-marked-file)
(define-key map "*t" 'iorg-dired-toggle-marks)
;; Lower keys for commands not operating on all the marked files
(define-key map "a" 'iorg-dired-find-alternate-file)
(define-key map "d" 'iorg-dired-flag-file-deletion)
(define-key map "e" 'iorg-dired-find-file)
(define-key map "f" 'iorg-dired-find-file)
(define-key map "\C-m" 'iorg-dired-find-file)
(put 'iorg-dired-find-file :advertised-binding "\C-m")
(define-key map "g" 'revert-buffer)
(define-key map "j" 'iorg-dired-goto-file)
(define-key map "k" 'iorg-dired-do-kill-lines)
(define-key map "l" 'iorg-dired-do-redisplay)
(define-key map "m" 'iorg-dired-mark)
(define-key map "n" 'iorg-dired-next-line)
(define-key map "o" 'iorg-dired-find-file-other-window)
(define-key map "\C-o" 'iorg-dired-display-file)
(define-key map "p" 'iorg-dired-previous-line)
(define-key map "s" 'iorg-dired-sort-toggle-or-edit)
(define-key map "t" 'iorg-dired-toggle-marks)
(define-key map "u" 'iorg-dired-unmark)
(define-key map "v" 'iorg-dired-view-file)
(define-key map "w" 'iorg-dired-copy-filename-as-kill)
(define-key map "x" 'iorg-dired-do-flagged-delete)
;; moving
(define-key map " "  'iorg-dired-next-line)
(define-key map [remap next-line] 'iorg-dired-next-line)
(define-key map [remap previous-line] 'iorg-dired-previous-line)
;; hiding
(define-key map "$" 'iorg-dired-hide-subdir)
(define-key map "\M-$" 'iorg-dired-hide-all)
;; isearch
(define-key map (kbd "M-s a C-s")   'iorg-dired-do-isearch)
(define-key map (kbd "M-s a M-C-s") 'iorg-dired-do-isearch-regexp)
(define-key map (kbd "M-s f C-s")   'iorg-dired-isearch-filenames)
(define-key map (kbd "M-s f M-C-s") 'iorg-dired-isearch-filenames-regexp)
;; misc
(define-key map [remap read-only-mode] 'iorg-dired-toggle-read-only)
;; `toggle-read-only' is an obsolete alias for `read-only-mode'
(define-key map [remap toggle-read-only] 'iorg-dired-toggle-read-only)
(define-key map "?" 'iorg-dired-summary)
(define-key map "\177" 'iorg-dired-unmark-backward)
(define-key map [remap undo] 'iorg-dired-undo)
(define-key map [remap advertised-undo] 'iorg-dired-undo)

  ;; (let ((map iorg-dired-mode-map))
  ;;   (suppress-keymap map)
  ;;   (define-key map "d"
  ;;     'iorg-dired-display-all)
  ;;   (define-key map "f"
  ;;     'iorg-dired-display-fields)
  ;;   ;; (define-key map "a"
  ;;   ;;   'iorg-dired-display-all)
  ;;   (define-key map "x"
  ;;      'iorg-dired-expect)
  ;;   (define-key map "c"
  ;;     'iorg-dired-click)
  ;;   (define-key map "p"
  ;;     'iorg-dired-press)
  ;;   (define-key map "v"
  ;;     'iorg-dired-value)
  ;;   (define-key map "e"
  ;;     'iorg-dired-enter)
  ;;   ;; (define-key map "i"
  ;;   ;;   'iorg-dired)
  ;;   (define-key map "E"
  ;;     'iorg-edit)
  ;;   (define-key map "S"
  ;;     'iorg-dired-mode)
  ;;   ;; (define-key map [menu-bar iorg-quick-dired]
  ;;   ;;   (cons (purecopy "Quick-Dired") iorg-quick-0
  ;;   ;;         dired-menu-map))
  ;;   map)

;; (unless iorg-quick-dired-mode-map
;;   (setq iorg-dired-mode-map (make-keymap))
;;   (supress-keymap iorg-quick-dired-mode-map)


;; * Run hooks and provide

;; iorg-dired.el ends here
