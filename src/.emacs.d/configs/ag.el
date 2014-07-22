(cqql/after-load 'ag
  (add-to-list 'ag-arguments "--hidden")

  (setf ag-highlight-search t))
