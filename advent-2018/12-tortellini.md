For whatever strange reason, many INI libraries only work by representing your INI files as hashmaps, where the most common representation is `Map String (Map String String)`. While people often live with this, I wanted to get greater type safety when working with INI configuration files.

I ended up writing two libraries: one in PureScript and one in Haskell.

## "The Tale of Two Tortellini: making record based libraries in PureScript and Haskell"

In this post, I wrote about the process involved in writing each library and how they compare when working with a model of fields to sections, sections to document.

<https://qiita.com/kimagure/items/941c22effff608dda9a7>

While the PureScript version was able to take advantage of anonymous records and the row types inside, the Haskell version was limited in that every section of the INI document needed to be its own record type. While it isn't that much of a shortcoming, it does show that having proper anonymous records would go a long way here.

And while this post is about INI files, the techniques covered here should help anyone write their own decoding code for JSON or any other format where they can work with a parsed structure.
