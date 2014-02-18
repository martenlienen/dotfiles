(require 'popwin)

(popwin-mode t)
(push '(ag-mode) popwin:special-display-config)
(push '("\*.+compilation\*" :regexp t) popwin:special-display-config)
