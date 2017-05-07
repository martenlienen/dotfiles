local awful = require("awful")
local rules = require("awful.rules")
local beautiful = require("beautiful")

local keybindings = require("cqql.keybindings")

-- Rules to apply to new clients (through the "manage" signal).
rules.rules = {
  -- All clients will match this rule.
  { rule = { },
    properties = { border_width = beautiful.border_width,
                   border_color = beautiful.border_normal,
                   focus = awful.client.focus.filter,
                   raise = true,
                   keys = keybindings.clientkeys,
                   buttons = keybindings.clientbuttons } },
  { rule = { class = "MPlayer" },
    properties = { floating = true } },
  { rule = { class = "pinentry" },
    properties = { floating = true } },
  { rule = { class = "gimp" },
    properties = { floating = true } }
}
