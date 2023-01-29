# Cheaper but buggier i3 scratch window workflow emulation in Mac OS

[Yesterday](./2023-01-28-expensively-recreating-my-i3-scratch-window-workflow-in-mac-os.md) I wrote about a fairly expensive solution with buying Alfred Powertools to achieve binding a keyboard shortcut to toggle the visibility of application windows in Mac OS to emulate my i3 setup. I have found a budget option for this that involves applescripts and karabiner-Elements, but I remain unconvinced that this is actually what I want.

## applescript

```applescript
on run {appName}

  if isFrontmost(appName) then
    log "Hiding " & appName
    tell application "Finder" to set visible of process appName to false
  else
    log "Activating " & appName
    tell application "System Events" to set frontmost of process appName to true
  end if

end run

on isFrontmost(appName)
  return frontmost of application appName
end isFrontmost
```

Those who have dealt with applescripts might wonder why I am not using `tell application appName to activate`. My experience with `activate` is that while it works most of the time when I am testing, there are also times when running this simply blocks my script for 3 seconds and does nothing. Some searching online showed me that 1. yes, people know about `activate` just failing in this way 2. some have resorted to just settings the `frontmost` property like so.

## karabiner-Elements

I made my config in `~/.config/karabiner/assets/complex_modifications`.

```json
{
  "title": "test rule",
  "rules": [
    {
      "description": "Toggle iTerm2",
      "manipulators": [
        {
          "from": {
            "key_code": "o",
            "modifiers": {
              "mandatory": ["left_option"]
            }
          },
          "to": [
            {
              "shell_command": "osascript ~/.dotfiles/applescripts/toggle-program.applescript iTerm2"
            }
          ],
          "type": "basic"
        }
      ]
    }
    ...
```

Like so, I bound `left_option + o` to run my applescript for toggling iTerm2. Then in the karabiner-Elements app I was able to add this by going to `Complex Modifications > Add rule > "test rule" > Enable all`.

## Result

Despite all of this work, it seems like this solution still suffers some startup delay of 100ms+ and sometimes does not trigger properly. So for those who want an easy solution that seems to handle a lot of the different scenarios already with proper interaction with Mac OS, Alfred Powertools is still the answer. But for those who want to try out this method and improve on it, please feel free to try your hand at it.
