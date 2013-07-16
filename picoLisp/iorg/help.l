## * iorg/help.l --- Help-page for iOrg
## ** MetaData 
##   :PROPERTIES:
##   :copyright: Alexander_Burger Thorsten_Jolitz
##   :copyright-since: 2010
##   :version:  0.9
##   :licence:  GPL3+
##   :licence-url: http://www.gnu.org/licenses/
##   :authors: Alexander_Burger Thorsten_Jolitz
##   :contact: <abu@software-lab.de> <tjolitz@gmail.com>
##   :git-repo: https://github.com/tj64/iorg
##   :git-clone: git@github.com:tj64/iorg.git
##   :keywords: emacs org-mode picolisp wiki
##   :END:

## ** Comments

## * Code
## ** Variables
## - *TestMarkup :: holds a test markup file

## ** Permissions

(must "Help" Iorg)

## ** Functions

(let *Css *IorgCss
   (menu "Help"
      (<h3> NIL "Help")
      (form NIL
         (<h4> NIL "Here you can upload and render a test markup file")
         (gui '(+UpField) 30)
         (gui '(+Drop +Button) '(field -1) "Upload"
            '(setq *TestMarkup (: drop)) )
         (--)
         (gui '(+Able +ShowButton) '*TestMarkup NIL
            '(in *TestMarkup (<hr>) (render)) )
         (gui '(+View +TextField) '(basename *TestMarkup)) )
      (<hr>)
      (with (db 'nm '+Doc "help")
         (in (blob (:  mup) 'txt)
            (render) ) ) ) )

## iorg/help.l ends here
