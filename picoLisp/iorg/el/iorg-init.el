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

;; * Requires

(message "iorg-init entered ...")

;; ;; remember this directory
;; (setq iorg-el-dir
;;       (file-name-directory (or load-file-name (buffer-file-name))))

;; (message "iOrg Directory: %s" iorg-el-dir)

;; ;; set load-path
;; (add-to-list 'load-path iorg-el-dir)

;; require libs
(require 'iorg)
(require 'iorg-scrape)
(require 'ox-iorg)
(require 'w3m-form-patched nil 'NOERROR)
;; (require 'iorg-agenda)
;; (require 'iorg-dired)


;; * Run hooks and provide

(provide 'iorg-init)

;; iorg-init.el ends here
