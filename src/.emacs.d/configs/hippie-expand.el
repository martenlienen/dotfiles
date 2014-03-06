(require 'hippie-exp)

(setf hippie-expand-try-functions-list
      '(try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-line
        try-complete-lisp-symbol))
