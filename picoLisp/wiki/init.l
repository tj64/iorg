# 22jan13abu
# (c) Software Lab. Alexander Burger

# Create the first three documents in the root DB file
(new T '(+Doc) 'nm "home")
(new T '(+Doc) 'nm "menu")
(new T '(+Doc) 'nm "help")
(commit)

# Create two roles, one for admins and one for normal members
(obj ((+Role) nm "Administrator") perm `*Perms)
(obj ((+Role) nm "Member") perm (Wiki))
(commit)

# Create the admin user
(obj ((+User) nm "admin") pw `(passwd "admin") role `(db 'nm '+Role "Administrator"))
(commit)

# Create some initial markup texts
(put>
   (obj ((+Mup))
      u `(db 'nm '+User "admin")
      d `(date)
      t `(time)
      sum "Initial start page"
      txt "wiki/home.txt" )
   'doc
   (db 'nm '+Doc "home") )

(put>
   (obj ((+Mup))
      u `(db 'nm '+User "admin")
      d `(date)
      t `(time)
      sum "Initial menu"
      txt "wiki/menu.txt" )
   'doc
   (db 'nm '+Doc "menu") )

(put>
   (obj ((+Mup))
      u `(db 'nm '+User "admin")
      d `(date)
      t `(time)
      sum "Help text"
      txt "wiki/help.txt" )
   'doc
   (db 'nm '+Doc "help") )

(commit)

# vi:et:ts=3:sw=3
