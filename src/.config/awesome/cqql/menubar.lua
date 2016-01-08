local menubar = require("menubar")

local settings = require("cqql.settings")

-- Set the terminal for applications that require it
menubar.utils.terminal = settings.terminal
