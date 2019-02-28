My libraries have a pretty "creative" naming scheme -- libraries that are absolutely usable have fairly boring names, and then all the libraries that are probably not ideal for every day are named after Korean snack foods.

Since every now and again someone actually wants to use my demonic creations, I'll try to keep this post somewhat up to date. And no, renaming these isn't an option because people will wrongly think many of these horrible libraries are meant to be used for everything. And also because Korean snack names are better than most other names these could get.

Please ping me anytime on Twitter [@jusrin00](https://twitter.com/jusrin00) if you have any questions about these or if this list needs to be updated!

## Useful libraries

* [Simple-JSON](https://github.com/justinwoo/purescript-simple-json): zero-effort JSON decoding and encoding
* [Home-Run-Ball](https://github.com/justinwoo/purescript-home-run-ball): refinement types done with row types. Incredibly useful for ensuring valid code paths AND for statically handling error conditions for displaying warnings to users.
* [Lenient-HTML-Parser](https://github.com/justinwoo/purescript-lenient-html-parser): a lenient HTML to List of Tags parser in TagSoup style.

## Pretty useful

* [Choco-Pie](https://github.com/justinwoo/purescript-choco-pie): a Cycle.js-like library for easy programming with circular events. This is a little questionable to use, but useful enough that I currently use it "in production".
* [PS-XStream](https://github.com/justinwoo/purescript-xstream): a wrapper for XStream by Andre Staltz. It's useful, but needs some maintenance help.
* [Milkis](https://github.com/justinwoo/purescript-milkis): a wrapper around node-fetch for easy fetching in node.
* [Node-Sqlite3](https://github.com/justinwoo/purescript-node-sqlite3): a wrapper for the sqlite3 library for node.
* [Node-he](https://github.com/justinwoo/purescript-node-he): a wrapper for the HE library for decoding and encoding HTML elements.

## Questionably useful
* [Kancho](https://github.com/justinwoo/purescript-kancho): a library for using constrained PureScript types to generate port-safe Elm types for embedding Elm applications in PureScript.
* [OhYes](https://github.com/justinwoo/purescript-ohyes): a library for using constrained PureScript types for transparent interop with TypeScript.
* [Node-Telegram-Bot-API](https://github.com/justinwoo/purescript-node-telegram-bot-api): a wrapper around the node-telegram-bot-api library.

## Don't use unless desperate

* [Jolly-Pong](https://github.com/justinwoo/purescript-jolly-pong): a wrapper library for Redux that correctly types the row typed interactions like combineReducers and the variant action types.
* [Chapagetti](https://github.com/justinwoo/purescript-chapagetti): a wrapper around React-Redux with correct types.
* [Gorebab](https://github.com/justinwoo/purescript-gorebab): a wrapper around Redux-Observable that tries to correctly type Epics and uses variants to correctly type filtered actions.
* [Bundaegi](https://github.com/justinwoo/purescript-bundaegi): a library for only defining Typescript types for use. See OhYes for a more useful library.