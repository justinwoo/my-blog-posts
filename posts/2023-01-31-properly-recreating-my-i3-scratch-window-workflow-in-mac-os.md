# Properly recreating my i3 scratch window workflow in Mac OS

Previously, I wrote about my attempts to recreate my i3 scratch window workflow, that I would assign a hotkey to specific applications to have them brought to the foreground in focus and hide them when I no longer use them. The approaches I'd tried so far were

- Use Alfred Powertools for 60GBP to bind specific applications ([post](./2023-01-28-expensively-recreating-my-i3-scratch-window-workflow-in-mac-os.md))

This suffered from two major problems: 1. there was a 100+ ms delay on all actions 2. only "registered" applications could be used, so emacs and other applications of mine were not being targeted.

- Use karabiner-Elements to bind hotkeys to applescripts ([post](./2023-01-29-cheaper-but-buggier-i3-scratch-window-workflow-emulation-in-mac-os.md))

This suffered from similar startup problems, a clunky configuration interface, and again, many workarounds for non-registered applications.

## Using Hammerspoon

I came across [Hammerspoon](https://www.hammerspoon.org/) and decided to try it, especially being that Lua was at least a real programming language with documentation and tooling. As the project claims it is a bridge between a Lua scripting engine and the actual Mac OS APIs, I was also hopeful this would be an actual solution.

Immediately I was amazed by their working tutorial and sample code, and the relative ease of being able to throw everything in `~/.hammerspoon/init.lua`. Then I was even more amazed that they had actual API documentation with how its parts are referenced and used.

As I was already familiar with the idea of "frontmost" and "activate", I simply followed the tutorial and documentation to quickly get what I wanted:

```lua
function toggleApp(appName)
    local app = hs.appfinder.appFromName(appName)

    if (app) then
        if (app:isFrontmost()) then
            app:hide()
        else
            app:activate()
        end
    else
        hs.alert.show("Could not find app from name: " .. appName)
    end
end

function bindApp(modifiers, key, appName)
    hs.hotkey.bind(modifiers, key, function() toggleApp(appName) end)
end

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "R", function() hs.reload() end)

hs.hotkey.bind({"alt"}, "`",
               function() hs.application.frontmostApplication():hide() end)

bindApp({"alt", "shift"}, "p", "Finder")
bindApp({"alt"}, "e", "emacs")
bindApp({"alt"}, "o", "iTerm2")
bindApp({"alt"}, "y", "Google Chrome")
bindApp({"alt"}, "i", "Slack")

hs.alert.show("Config loaded")
```

And as hoped, the result was that these bindings work instantly, so that there is no delay between my keypresses and the actual window actions being applied. This also did not suffer from the same problem with application finding as applescript or the Alfred workflow tool, and I think it's likely that the developers of Hammerspoon simply look through the actual application list rather than using whatever registered application mechanism.

So by doing this, I have solved all of the previous problems I had and found a new way to properly programmatically interact with my desktop environment in Mac OS.
