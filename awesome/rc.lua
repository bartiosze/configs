-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local keydoc = require("keydoc")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init("/usr/share/awesome/themes/zenburn/theme.lua")
-- beautiful.init("/home/bkaliszuk/.config/awesome/themes/zenburn-custom/theme.lua")


-- This is used later as the default terminal and editor to run.
terminal = "terminal"
browser = "firefox"
editor = os.getenv("EDITOR") or "emacsclient "
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
local layouts =
{
    awful.layout.suit.tile,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.fair,
    awful.layout.suit.floating,
    awful.layout.suit.magnifier
}
-- }}}

-- {{{ Wallpaper
if beautiful.wallpaper then
    for s = 1, screen.count() do
        gears.wallpaper.maximized(beautiful.wallpaper, s, true)
    end
end
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {
   names = { "one", "two", "three", "four"},
   layout = { layouts[1], layouts[2], layouts[3], layouts[4] }
}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag(tags.names, s, tags.layout)
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({ items = { { "terminal", terminal },
                             { "bash", terminal .. " --loginShell -e bash" },
                             { "browser", browser },
                             { "awesome", myawesomemenu, beautiful.awesome_icon }}
                       })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- {{{ Wibox
-- Create a textclock widget
mytextclock = awful.widget.textclock()

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({ width=250 })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    -- Create a promptbox for each screen
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s })

    -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(mylauncher)
    left_layout:add(mytaglist[s])
    left_layout:add(mypromptbox[s])

    -- Widgets that are aligned to the right
    local right_layout = wibox.layout.fixed.horizontal()
    if s == 1 then right_layout:add(wibox.widget.systray()) end
    right_layout:add(mytextclock)
    right_layout:add(mylayoutbox[s])

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)

    mywibox[s]:set_widget(layout)
end
-- }}}

-- required by keydoc
beautiful.fg_widget_value="light green"
beautiful.fg_widget_clock="gold"
beautiful.fg_widget_value_important="red"

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewprev),
    awful.button({ }, 5, awful.tag.viewnext)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    keydoc.group("Tags"),
    awful.key({ "Mod1"          }, "Left",   awful.tag.viewprev,
              "Switch to previous tag"),
    awful.key({ "Mod1"           }, "Right",  awful.tag.viewnext,
              "Switch to next tag"),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
              "Revert tag history"),
    keydoc.group("Clients and screens"),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
              "Cycle client focus history"),
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
              "Swap with next client"),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
              "Swap with prev client"),
    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end,
              "Focus next client"),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end,
              "Focus prev client"),
    awful.key({ modkey, "Control" }, "h", function () awful.screen.focus_relative( 1) end,
              "Focus next screen"),
    awful.key({ modkey, "Control" }, "l", function () awful.screen.focus_relative(-1) end,
              "Focus previous screen"),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              "Focus urgent client"),
    awful.key({ modkey, "Control" }, "n", awful.client.restore,
              "Restore a client"),
    -- Layout manipulation
    keydoc.group("Layouts"),
    awful.key({ modkey, "Shift"   }, "=",     function () awful.tag.incmwfact( 0.05)    end,
              "Client size factor 5%"),
    awful.key({ modkey,           }, "-",     function () awful.tag.incmwfact(-0.05)    end,
              "Client size factor -5%"),
    awful.key({ modkey, "Shift"   }, "\\",    function () awful.tag.setmwfact(0.5)    end,
              "Client size factor equal"),
    awful.key({ modkey, "Control"   }, "Up",
              function () 
                 awful.tag.incnmaster( 1) 
                 naughty.notify({ preset = { timeout = 1 },
                                  title = "Master count",
                                  text = tostring(awful.tag.getnmaster())})
              end,
              "Increase master count"),
    awful.key({ modkey, "Control"   }, "Down",     
              function () 
                 awful.tag.incnmaster(-1)
                 naughty.notify({ preset = naughty.config.presets.low,
                                  title = "Master count",
                                  text = tostring(awful.tag.getnmaster())})
              end,
              "Decrease master count"),
    awful.key({ modkey, "Control" }, "k",
              function ()
                 awful.tag.incncol( 1)
                 naughty.notify({ preset = { timeout = 1 },
                                  title = "Column count",
                                  text = tostring(awful.tag.getncol())})
              end,
              "Increase column count"),
    awful.key({ modkey, "Control" }, "j",
              function ()
                 awful.tag.incncol(-1)
                 naughty.notify({ preset = { timeout = 1 },
                                  title = "Column count",
                                  text = tostring(awful.tag.getncol())})
              end,
              "Decrease column count"),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end,
              "Next layout"),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end,
              "Previous layout"),

    -- Standard program
    keydoc.group("Execution"),
    awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end,
              "Spawn terminal"),
    awful.key({ modkey,           }, "f",      function () awful.util.spawn(browser)  end,
              "Spawn browser"),
    awful.key({ modkey,           }, "b",      function () awful.util.spawn(terminal .. " --loginShell -e bash")  end,
              "Spawn browser"),
    -- Prompt
    awful.key({ modkey },            "r", function () mypromptbox[mouse.screen]:run() end,
              "Run a command"),
    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end,
              "Run lua prompt"),
    awful.key({ modkey, "Control" }, "r", awesome.restart,
              "Awesome restart"),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit,
              "Awesome quit"),



    keydoc.group("Misc"),
    awful.key({ modkey, }, "F1", keydoc.display, "Show key information"),
    awful.key({ modkey,           }, "w", 
              function () 
                 mymainmenu:show({keygrabber=true}) 
                 awful.menu.item_enter(mymainmenu, 1)
              end, "Show awesome menu"),
    awful.key({ modkey }, "p", function() menubar.show() end, "Show menubar")
)

clientkeys = awful.util.table.join(
    keydoc.group("Client"),

    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
             "Kill client"),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
             "Go to master client"),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen,
             "Move client to screen"),
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end,
             "Redraw screen"),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle,
             "Floating toggle"),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
             "Ontop toggle"),
    awful.key({ modkey,           }, "m",
              function (c)
                 c.maximized_horizontal = not c.maximized_horizontal
                 c.maximized_vertical   = not c.maximized_vertical
              end,
             "Maximized toggle"),
    awful.key({ modkey,           }, "n",
              function (c)
                 -- The client currently has the input focus, so it cannot be
                 -- minimized, since minimized clients can't have the focus.
                 c.minimized = true
              end,
              "Minimize toggle")

)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ "Mod1" }, "F" .. i,
                  function ()
                        local screen = mouse.screen
                        local tag = awful.tag.gettags(screen)[i]
                        if tag then
                           awful.tag.viewonly(tag)
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      local tag = awful.tag.gettags(screen)[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      local tag = awful.tag.gettags(client.focus.screen)[i]
                      if client.focus and tag then
                          awful.client.movetotag(tag)
                     end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      local tag = awful.tag.gettags(client.focus.screen)[i]
                      if client.focus and tag then
                          awful.client.toggletag(tag)
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "pinentry" },
      properties = { floating = true } },
    { rule = { class = "gimp" },
      properties = { floating = true } },
    -- Set Firefox to always map on tags number 2 of screen 1.
    { rule = { class = "Firefox" },
      properties = { tag = tags[3] } },
    { rule = { class = "emacs" },
      properties = { tag = tags[0] } },
    { rule = { name = "EmacsClient" },
      properties = { floating = true } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    -- Enable sloppy focus
    c:connect_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end

    local titlebars_enabled = false
    if titlebars_enabled and (c.type == "normal" or c.type == "dialog") then
        -- buttons for the titlebar
        local buttons = awful.util.table.join(
                awful.button({ }, 1, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.move(c)
                end),
                awful.button({ }, 3, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.resize(c)
                end)
                )

        -- Widgets that are aligned to the left
        local left_layout = wibox.layout.fixed.horizontal()
        left_layout:add(awful.titlebar.widget.iconwidget(c))
        left_layout:buttons(buttons)

        -- Widgets that are aligned to the right
        local right_layout = wibox.layout.fixed.horizontal()
        right_layout:add(awful.titlebar.widget.floatingbutton(c))
        right_layout:add(awful.titlebar.widget.maximizedbutton(c))
        right_layout:add(awful.titlebar.widget.stickybutton(c))
        right_layout:add(awful.titlebar.widget.ontopbutton(c))
        right_layout:add(awful.titlebar.widget.closebutton(c))

        -- The title goes in the middle
        local middle_layout = wibox.layout.flex.horizontal()
        local title = awful.titlebar.widget.titlewidget(c)
        title:set_align("center")
        middle_layout:add(title)
        middle_layout:buttons(buttons)

        -- Now bring it all together
        local layout = wibox.layout.align.horizontal()
        layout:set_left(left_layout)
        layout:set_right(right_layout)
        layout:set_middle(middle_layout)

        awful.titlebar(c):set_widget(layout)
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

-- Autostart section

awful.util.spawn_with_shell("xmodmap ~/.keys.xmodmap")


local r = require("runonce")
r.run("urxvtd -q -o -f")
r.run("ssh-add </dev/null")
r.run("vmware-user")
r.run("emacs")
r.run("xcape -e '0x1234=Return'")





