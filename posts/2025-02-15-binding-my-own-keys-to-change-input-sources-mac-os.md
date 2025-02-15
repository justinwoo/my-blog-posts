# Binding my own keys to change input sources in Mac OS

Those of you who have used Mac OS with multiple input sources likely already know how disappointing the default "Select the previous/next input source" keyboard shortcut is. Mainly, it bothers me that no matter what I try to do with it, it will always have a giant, unpredictable amount of delay.

While I don't want to disappoint by promising too much, I think I've found that using Hammerspoon to bind the individual input sources I use works reasonably well.

## Identifying and setting input sources

Once you add the input sources you want to use in Settings, you'll have to find out the names of the sources in Mac OS to feed into Hammerspoon. This can be done easily enough using the Hammerspoon console by using the hs.keycodes API:

```lua
hs.keycodes.currentSourceID()
# com.apple.keylayout.US
```

This method is then called with the various source IDs you want to use.

```lua
hs.keycodes.currentSourceID("com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese")
hs.keycodes.currentSourceID("com.apple.keylayout.US")
hs.keycodes.currentSourceID("com.apple.keylayout.Swedish-Pro")
hs.keycodes.currentSourceID("com.apple.inputmethod.TCIM.Pinyin")
hs.keycodes.currentSourceID("com.apple.inputmethod.Korean.2SetKorean")

# ...etc
```

## Binding to keys and dealing with alerts

Binding to hotkey combinations then in your config is straightforward:

```lua
hs.hotkey.bind({"ctrl", "alt", "cmd"}, "J", function()
    hs.keycodes.currentSourceID(
        "com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese")
    alert("日本語")
end)

hs.hotkey.bind({"ctrl", "alt", "cmd"}, "K", function()
    hs.keycodes.currentSourceID("com.apple.keylayout.US")
    alert("English")
end)

hs.hotkey.bind({"ctrl", "alt", "cmd"}, "L", function()
    hs.keycodes.currentSourceID("com.apple.keylayout.Swedish-Pro")
    alert("Swedish")
end)

hs.hotkey.bind({"ctrl", "alt", "cmd"}, "H", function()
    hs.keycodes.currentSourceID("com.apple.inputmethod.TCIM.Pinyin")
    alert("漢字")
end)

hs.hotkey.bind({"ctrl", "alt", "cmd"}, ";", function()
    hs.keycodes.currentSourceID("com.apple.inputmethod.Korean.2SetKorean")
    alert("한글")
end)
```

I recommend that for using alerts, you define your own function that clears away previous messages of the same type. In my config, I defined a function that writes to a local var for the alert id to be cleared every I run the function.

```lua
local messageAlertId
function alert(text)
    hs.alert.closeSpecific(messageAlertId)
    messageAlertId = hs.alert.show(text)
end
```

## Thoughts

And that's about it. This is how I finally just bound all the input sources I want to individual keybinds so that I can have access to them without waiting for the incredibly slow input source menu in Mac OS.

I'd also like to hear of other solutions to this problem if anyone has their own methods.

## Links

* Hammerspoon https://www.hammerspoon.org/
* My config https://github.com/justinwoo/.dotfiles/blob/master/hammerspoon/init.lua
