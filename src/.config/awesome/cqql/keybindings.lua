local awful = require("awful")

local settings = require("cqql.settings")
local mod = settings.modkey

local function rofi(mode)
  awful.util.spawn("rofi -show " .. mode)
end

root.buttons(awful.util.table.join(
               awful.button({ }, 4, awful.tag.viewnext),
               awful.button({ }, 5, awful.tag.viewprev)))

globalkeys = awful.util.table.join(
  awful.key({ mod }, "Left", awful.tag.viewprev),
  awful.key({ mod }, "Right", awful.tag.viewnext),
  awful.key({ mod }, "Escape", awful.tag.history.restore),

  awful.key({ mod }, "j",
    function ()
      awful.client.focus.byidx(1)
      if client.focus then client.focus:raise() end
  end),
  awful.key({ mod }, "k",
    function ()
      awful.client.focus.byidx(-1)
      if client.focus then client.focus:raise() end
  end),

  -- Layout manipulation
  awful.key({ mod, "Shift" }, "j", function () awful.client.swap.byidx(1) end),
  awful.key({ mod, "Shift" }, "k", function () awful.client.swap.byidx(-1) end),
  awful.key({ mod, "Control" }, "j", function ()
      awful.screen.focus_relative(1)
  end),
  awful.key({ mod, "Control" }, "k", function ()
      awful.screen.focus_relative(-1)
  end),
  awful.key({ mod }, "u", awful.client.urgent.jumpto),
  awful.key({ mod }, "Tab",
    function ()
      awful.client.focus.history.previous()
      if client.focus then
        client.focus:raise()
      end
  end),

  -- Standard program
  awful.key({ mod }, "Return", function ()
      awful.util.spawn(settings.terminal)
  end),
  awful.key({ mod, "Control" }, "r", awesome.restart),
  awful.key({ mod, "Shift" }, "q", awesome.quit),

  awful.key({ mod }, "l", function () awful.tag.incmwfact( 0.05) end),
  awful.key({ mod }, "h", function () awful.tag.incmwfact(-0.05) end),
  awful.key({ mod, "Shift" }, "h", function () awful.tag.incnmaster( 1) end),
  awful.key({ mod, "Shift" }, "l", function () awful.tag.incnmaster(-1) end),
  awful.key({ mod, "Control" }, "h", function () awful.tag.incncol( 1) end),
  awful.key({ mod, "Control" }, "l", function () awful.tag.incncol(-1) end),
  awful.key({ mod }, "space", function ()
      awful.layout.inc(settings.layouts,  1)
  end),
  awful.key({ mod, "Shift" }, "space", function ()
      awful.layout.inc(settings.layouts, -1)
  end),

  awful.key({ mod, "Control" }, "n", awful.client.restore),

  -- Prompt
  awful.key({ mod }, "r", function () mypromptbox[mouse.screen]:run() end),

  awful.key({ mod }, "x",
    function ()
      awful.prompt.run({ prompt = "Run Lua code: " },
        mypromptbox[mouse.screen].widget,
        awful.util.eval, nil,
        awful.util.getdir("cache") .. "/history_eval")
  end),

  awful.key({ mod }, "w", function() rofi("window") end),
  awful.key({ mod }, "p", function() rofi("run") end))

clientkeys = awful.util.table.join(
  awful.key({ mod }, "f", function (c) c.fullscreen = not c.fullscreen  end),
  awful.key({ mod, "Shift" }, "c", function (c) c:kill() end),
  awful.key({ mod, "Control" }, "space",  awful.client.floating.toggle),
  awful.key({ mod, "Control" }, "Return", function (c)
      c:swap(awful.client.getmaster())
  end),
  awful.key({ mod }, "o", awful.client.movetoscreen),
  awful.key({ mod }, "t", function (c) c.ontop = not c.ontop end),
  awful.key({ mod }, "n", function (c)
      -- The client currently has the input focus, so it cannot be
      -- minimized, since minimized clients can't have the focus.
      c.minimized = true
  end),
  awful.key({ mod }, "m", function (c)
      c.maximized_horizontal = not c.maximized_horizontal
      c.maximized_vertical   = not c.maximized_vertical
end))

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
  globalkeys = awful.util.table.join(
    globalkeys,
    -- View tag only.
    awful.key({ mod }, "#" .. i + 9, function ()
        local screen = mouse.screen
        local tag = awful.tag.gettags(screen)[i]
        if tag then
          awful.tag.viewonly(tag)
        end
    end),
    -- Toggle tag.
    awful.key({ mod, "Control" }, "#" .. i + 9, function ()
        local screen = mouse.screen
        local tag = awful.tag.gettags(screen)[i]
        if tag then
          awful.tag.viewtoggle(tag)
        end
    end),
    -- Move client to tag.
    awful.key({ mod, "Shift" }, "#" .. i + 9, function ()
        if client.focus then
          local tag = awful.tag.gettags(client.focus.screen)[i]
          if tag then
            awful.client.movetotag(tag)
          end
        end
    end),
    -- Toggle tag.
    awful.key({ mod, "Control", "Shift" }, "#" .. i + 9, function ()
        if client.focus then
          local tag = awful.tag.gettags(client.focus.screen)[i]
          if tag then
            awful.client.toggletag(tag)
          end
        end
  end))
end

clientbuttons = awful.util.table.join(
  awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
  awful.button({ mod }, 1, awful.mouse.client.move),
  awful.button({ mod }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
