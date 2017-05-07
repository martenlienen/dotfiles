local math = require("math")
local string = require("string")

local awful = require("awful")
local wibox = require("wibox")
local vicious = require("vicious")
local widgets = require("vicious.widgets")

local settings = require("cqql.settings")


local textclock = awful.widget.textclock("%d/%m %H:%M", 60)

local volume = wibox.widget.textbox("")
local function formatvolume(widget, data)
  local muted = data[2] == "♩"

  if muted then
    return "V mute"
  else
    return string.format("V %3d%%", data[1])
  end
end
vicious.register(volume, vicious.widgets.volume, formatvolume, 1, "Master")

local battery = wibox.widget.textbox("")
vicious.register(battery, vicious.widgets.bat, "B $1 $2% $3", 2, "BAT1")

for s = 1, screen.count() do
  local taglist = awful.widget.taglist(s, awful.widget.taglist.filter.all)

  -- Widgets that are aligned to the left
  local left_layout = wibox.layout.fixed.horizontal()
  left_layout:add(taglist)

  -- Widgets that are aligned to the right
  local right_layout = wibox.layout.fixed.horizontal()
  if s == 1 then right_layout:add(wibox.widget.systray()) end
  right_layout:add(wibox.widget.textbox("  "))
  right_layout:add(volume)
  right_layout:add(wibox.widget.textbox("  "))
  right_layout:add(battery)
  right_layout:add(wibox.widget.textbox("  "))
  right_layout:add(textclock)

  -- Now bring it all together (with the tasklist in the middle)
  local layout = wibox.layout.align.horizontal()
  layout:set_left(left_layout)
  layout:set_right(right_layout)

  local margin = 5
  local marginbox = wibox.layout.margin()
  marginbox:set_left(margin)
  marginbox:set_right(margin)
  marginbox:set_top(margin)
  marginbox:set_bottom(margin)
  marginbox:set_widget(layout)

  -- Create the wibox
  local container = awful.wibox{ position = "top", screen = s }
  -- Reduce container height, add margins and ensure that it is even
  container.height = math.floor((0.8 * container.height + 2 * margin) / 2) * 2
  container:set_widget(marginbox)
end
