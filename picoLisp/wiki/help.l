# 09aug10abu
# (c) Software Lab. Alexander Burger

# *TestMarkup

(must "Help" Wiki)

(let *Css *WikiCss
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

# vi:et:ts=3:sw=3
