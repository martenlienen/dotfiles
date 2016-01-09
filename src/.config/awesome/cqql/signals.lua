local awful = require("awful")
local beautiful = require("beautiful")

local function initclient(c, startup)
  local function focus(c)
    magnifierLayout = awful.layout.get(c.screen) == awful.layout.suit.magnifier
    focusable = awful.client.focus.filter(c)

    if focus and not magnifierLayout then
      client.focus = c
    end
  end

  -- Focus clients on hover
  c:connect_signal("mouse::enter", focus)

  if not startup then
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- awful.client.setslave(c)

    -- Put windows in a smart way, only if they do not set an initial position
    if not c.size_hints.user_position and not c.size_hints.program_position then
      awful.placement.no_overlap(c)
      awful.placement.no_offscreen(c)
    end
  end
end

local function highlightBorder(c)
  c.border_color = beautiful.border_focus
end

local function resetBorder(c)
  c.border_color = beautiful.border_normal
end

-- Signal function to execute when a new client appears.
client.connect_signal("manage", initclient)
client.connect_signal("focus", highlightBorder)
client.connect_signal("unfocus", resetBorder)
