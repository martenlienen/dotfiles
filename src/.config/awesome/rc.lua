-- Standard awesome library
local awful = require("awful")

-- Ensure that there is always a client in focus
require("awful.autofocus")

-- Notification library
local naughty = require("naughty")

-- Notify the user of runtime errors
do
  local in_error = false
  local handler = function (err)
    -- Make sure we don't go into an endless error loop
    if in_error then return end
    in_error = true

    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, an error happened!",
                     text = err })
    in_error = false
  end

  awesome.connect_signal("debug::error", handler)
end

require("cqql.theme")
require("cqql.tags")
require("cqql.menubar")
require("cqql.wibox")
require("cqql.keybindings")
require("cqql.rules")
require("cqql.signals")
