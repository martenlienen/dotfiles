(setf org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-agenda-start-on-weekday nil
      org-capture-templates '(("g" "General work" entry (file+headline "work.org" "General") "* TODO %?")
                              ("q" "Qualify EU" entry (file+headline "work.org" "Qualify") "* TODO %?")
                              ("s" "Server Administration" entry (file+headline "work.org" "Server Administration") "* TODO %?")))
