;; * iorg-init.el --- init file for Emacs calls from iOrg
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

;; * Set Load Path

  (message "iorg-init entered") 

  ;; (add-to-list 'load-path
  ;;            (file-name-directory
  ;;             (buffer-file-name)))

  (message "%s" load-path)

;; * Run hooks and provide

(provide 'iorg-init)

;; iorg.el ends here
