local beautiful = require("beautiful")
local gears = require("gears")

local settings = require("cqql.settings")

-- Themes define colours, icons, font and wallpapers
beautiful.init(settings.theme)

if beautiful.wallpaper then
  for s = 1, screen.count() do
    gears.wallpaper.maximized(beautiful.wallpaper, s, true)
  end
end
