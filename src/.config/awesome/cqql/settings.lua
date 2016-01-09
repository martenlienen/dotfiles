local awful = require("awful")

return {
  theme = os.getenv("HOME") .. "/.config/awesome/theme/theme.lua",
  terminal = "urxvt",
  modkey = "Mod4",
  layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.tile.top,
    awful.layout.suit.floating
  }
}
