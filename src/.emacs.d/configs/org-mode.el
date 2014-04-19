(setf org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-capture-templates '(("t" "Task" entry (file+headline "work.org" "Tasks") "* TODO %?")))

(require 'org-install)
