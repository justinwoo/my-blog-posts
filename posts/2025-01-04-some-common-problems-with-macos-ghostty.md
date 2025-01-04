# Some common problems with Mac OS Ghostty

Ghostty has been a welcome addition to the options for Mac OS terminal emulators for me, as I have been mostly disappointed at all of the ones I've tried over my time using Mac OS. While I could try to adapt to using the default Terminal in Mac OS, I've always found some kind of annoyance, along with unfamiliarity with how to get it configured consistently across machines, which makes me stop using it almost as soon as I start.

## Changing the configuration doesn't seem to have any effect

The documentation tells you that `~/.config/ghostty/config` or `$XDG_CONFIG_HOME/ghostty/config` will be used for reading the configurations. But very commonly, you will end up with Ghostty configuration also living in your home path under `$HOME/Library/Application\ Support/com.mitchellh.ghostty/config`. This file seems to be created on initial loads of Ghostty without existing files in the previous paths, where pressing `Cmd + ,` to open the config file implicitly points here until those paths are made.

The problem with this behavior is that the config files seem to all be loaded, with the last file dictating the final configuration value. You can run `ghostty +show-config` to see that values you have set in your preferred configs have been overridden by this file.

I recommend everyone run `rm "$HOME/Library/Application\ Support/com.mitchellh.ghostty/config" || exit 0` or something in your initial setup that also includes symlinking the Ghostty config from your dotfiles or other backup sources.

## The default config is actually fairly bizarre

As a project boasting that most users will not need nor want to change the config, it makes some various strange choices as defaults. Here is my current config:

```
# i don't use zsh, and rather just use a newer version of
# bash that apple does not want to ship for licensing reasons
command = /opt/homebrew/bin/bash -l

# i don't use dark mode because it just strains my eyes even more
theme = Builtin Solarized Light
foreground = #000000

# ghostty's cursor handling seems to not do the right thing,
# cursor blink by default is also a strange choice to me
shell-integration-features = no-cursor
cursor-style-blink = false

# text is also a bit too smashed in by default.
# i believe this should be 1.1 line height
adjust-cell-height = 10%
font-family = Monaco

# no padding also makes everything feel clipped
window-padding-x = 8
window-padding-y = 8

# i didn't like the title or window style either
macos-titlebar-style = native
window-theme = system

# random icon styling
macos-option-as-alt = true
macos-icon = custom-style
macos-icon-frame = beige
macos-icon-screen-color = #000000
macos-icon-ghost-color = #000000
```

## Many config values require quitting the application

You will also notice when you change these configs that many of these configs will require restarting the application to apply. I don't know if this is an architectural restriction, but it does makes the configuration experience still not very nice.

## Conclusion

I still like Ghostty and will continue trying it out for a while, but it does not come off to me as a well-tested project, but rather a very early project now being subjected to real-world users who will complain, rather than whatever closed tester pool existed before.

I also don't see much reason to change from my own usage of `st` on Linux, so I don't think I will make any comment on the Linux distribution of Ghostty.

If you are also using Ghostty on Mac OS and have many of your own configuration changes and other modifications, let me know and make your own post so I can share it with people I know also.

## Links

* Ghostty https://ghostty.org/
* My config https://github.com/justinwoo/.dotfiles/blob/master/xdg/ghostty/config
