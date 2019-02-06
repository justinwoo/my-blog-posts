---
title: What to check when React Native `npm start` and Jest don't work
tags: react-native
author: kimagure
slide: false
---
There seems to be a problem with watchman where it leaves its own files in an invalid state and doesn't know how to clean up after itself, making all programs relying on it to just hang.

The solution I've found to this is to remove everything inside `/usr/local/var/run/watchman/${YOUR_USER}-state`.

So the next time `jest` and `npm start` in your React Native projects just freezes, I know where I'm looking first.

