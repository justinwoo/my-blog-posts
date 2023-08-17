# Home as Ctrl+A with Karabiner on Mac OS

I've spent some time using Macs again since I bought some M2 machines, but I'm still amazed at the broken home and end key behaviors. I have a fairly disappointing solution using Karabiner to remap them to combination keys, but I can't help but wonder if there isn't a better solution already out there.

## Using Karabiner

The way I chose to do the rebinding was to use Karabiner's complex modifications, so that I first have to write a spec in JSON, then have it loaded up in the UI. This meant that I first wrote this:

```json
{
  "title": "My rules",
  "rules": [
    {
      "description": "Home to ctrl+a",
      "manipulators": [
        {
          "from": {
            "key_code": "home"
          },
          "to": [
            {
              "key_code": "a",
              "modifiers": "control"
            }
          ],
          "type": "basic"
        }
      ]
    },
    {
      "description": "Shift+home to shift+ctrl+a",
      "manipulators": [
        {
          "from": {
            "key_code": "home",
            "modifiers": {
              "mandatory": ["shift"]
            }
          },
          "to": [
            {
              "key_code": "a",
              "modifiers": [
                "shift",
                "control"
              ]
            }
          ],
          "type": "basic"
        }
      ]
    },
    {
      "description": "End to ctrl+e",
      "manipulators": [
        {
          "from": {
            "key_code": "end"
          },
          "to": [
            {
              "key_code": "e",
              "modifiers": "control"
            }
          ],
          "type": "basic"
        }
      ]
    },
    {
      "description": "Shift+end to shift+ctrl+e",
      "manipulators": [
        {
          "from": {
            "key_code": "end",
            "modifiers": {
              "mandatory": ["shift"]
            }
          },
          "to": [
            {
              "key_code": "e",
              "modifiers": [
                "shift",
                "control"
              ]
            }
          ],
          "type": "basic"
        }
      ]
    }
  ]
}
```

I then take this spec and put it into the `~/.config/karabiner` directory as a symlink:

```bash
#!/bin/bash

[[ -z $MACOS ]] && exit 0

TARGET=~/.config/karabiner/assets/complex_modifications/myrules.json

mkdir -p "$(dirname $TARGET)"

if [ -e $TARGET ]
then
    echo "skipping linking karabiner modifications"
else
    echo karabiner modifications to "$TARGET"
    ln -s "$DOT/karabiner/myrules.json" $TARGET
fi
```

Then in the Karabiner-Elements Settings, you're meant to be able to go to Complex Modifications and see your rules in "Add rule".

## Result

Now my home and end keys behave more like I wish think they should, but this still doesn't work quite as I wish it did. This bothers me even more that I didn't have to do this in older versions of Mac OS.

If anyone has a better solution to this and other modern Mac OS annoyances, I'd like to hear from you.
