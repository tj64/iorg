;; * iorg-agenda.el --- iOrg version of Org-mode's agenda
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
;;   :keywords: emacs org-mode agenda picolisp iorg 
;;   :END:

;; ** Commentary
;; *** About iorg-agenda
;; *** Installation
;; *** Bugs and Shortcomings
;; *** Emacs Version
;; ** ChangeLog
;; * Requires
;; * Mode and Exporter definitions
;; ** Mode definitions
;; *** iOrg Agenda Mode
;; * Variables
;; ** Hooks
;; ** Vars
;; ** Customs
;; *** Custom Groups
;; *** Custom Vars
;; * Functions
;; ** Non-interactive Functions
;; ** Commands
;; * Menus and Keys
;; ** Menus
;; ** Keys

  ;; (let ((map iorg-agenda-mode-map))
  ;;   (define-key map "\C-c\C-c" nil)
  ;;   (define-key map "\C-c\C-g"
  ;;     'comint-interrupt-subjob)
  ;;   (define-key map "\C-c\C-cd"
  ;;     'iorg-scrape-display-all)
  ;;   ;; (define-key map "\C-c\C-cf"
  ;;   ;;   'iorg-scrape-display-fields)
  ;;   ;; (define-key map "\C-c\C-ca"
  ;;   ;;   'iorg-scrape-display-all)
  ;;   (define-key map "\C-c\C-cx"
  ;;     'iorg-scrape-expect)
  ;;   (define-key map "\C-c\C-cc"
  ;;     'iorg-scrape-click)
  ;;   (define-key map "\C-c\C-cp"
  ;;     'iorg-scrape-press)
  ;;   (define-key map "\C-c\C-cv"
  ;;     'iorg-scrape-value)
  ;;   (define-key map "\C-c\C-ce"
  ;;     'iorg-scrape-enter)
  ;;   ;; (define-key map "\C-c\C-ci"
  ;;   ;;   'iorg-dired)
  ;;   (define-key map "\C-c\C-cE"
  ;;     'iorg-edit)
  ;;   (define-key map "\C-c\C-cQ"
  ;;     'iorg-quick-scrape-mode)
  ;;   (define-key map "\C-c\C-c\C-d"
  ;;     'iorg-scrape-display-all)
  ;;   ;; (define-key map "\C-c\C-c\C-f"
  ;;   ;;   'iorg-scrape-display-fields)
  ;;   ;; (define-key map "\C-c\C-c\C-a"
  ;;   ;;   'iorg-scrape-display-all)
  ;;   (define-key map "\C-c\C-c\C-x"
  ;;     'iorg-scrape-expect)
  ;;   (define-key map "\C-c\C-c\C-c"
  ;;     'iorg-scrape-click)
  ;;   (define-key map "\C-c\C-c\C-p"
  ;;     'iorg-scrape-press)
  ;;   (define-key map "\C-c\C-c\C-v"
  ;;     'iorg-scrape-value)
  ;;   (define-key map "\C-c\C-c\C-e"
  ;;     'iorg-scrape-enter)
  ;;   ;; (define-key map "\C-c\C-c\C-i"
  ;;   ;;   'iorg-dired)
  ;;   (define-key map "\C-c\C-c\C-E"
  ;;     'iorg-edit)
  ;;   (define-key map "\C-c\C-c\C-Q"
  ;;     'iorg-quick-scrape-mode)
  ;;   ;; (define-key map [menu-bar iorg-scrape]
  ;;   ;;   (cons (purecopy "iOrg-Scrape") iorg-scrape-menu-map))
  ;;   map)

  ;; (let ((map iorg-quick-scrape-mode-map))
  ;;   (suppress-keymap map)
  ;;   (define-key map "d"
  ;;     'iorg-scrape-display-all)
  ;;   (define-key map "f"
  ;;     'iorg-scrape-display-fields)
  ;;   ;; (define-key map "a"
  ;;   ;;   'iorg-scrape-display-all)
  ;;   (define-key map "x"
  ;;      'iorg-scrape-expect)
  ;;   (define-key map "c"
  ;;     'iorg-scrape-click)
  ;;   (define-key map "p"
  ;;     'iorg-scrape-press)
  ;;   (define-key map "v"
  ;;     'iorg-scrape-value)
  ;;   (define-key map "e"
  ;;     'iorg-scrape-enter)
  ;;   ;; (define-key map "i"
  ;;   ;;   'iorg-dired)
  ;;   (define-key map "E"
  ;;     'iorg-edit)
  ;;   (define-key map "S"
  ;;     'iorg-scrape-mode)
  ;;   ;; (define-key map [menu-bar iorg-quick-scrape]
  ;;   ;;   (cons (purecopy "Quick-Scrape") iorg-quick-0
  ;;   ;;         scrape-menu-map))
  ;;   map)

;; (unless iorg-quick-scrape-mode-map
;;   (setq iorg-scrape-mode-map (make-keymap))
;;   (supress-keymap iorg-quick-scrape-mode-map)


;; * Run hooks and provide

;; iorg-agenda.el ends here
