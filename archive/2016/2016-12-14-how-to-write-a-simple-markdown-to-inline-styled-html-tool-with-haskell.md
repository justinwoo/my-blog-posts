How to write a simple MarkDown to inline-Styled HTml tool with Haskell
A while back, I had the problem of wanting to write and publish some posts for my work's blog. I wrote my post in markdown, since it's fairly easy and familiar. I ran into the problem that our CMS, like many others, doesn't support markdown, and I really wanted to have my text formatted easily and have highlighted code blocks. I found out that it supported a limited set of HTML features that would allow me to have highlighting -- inline styling without classnames.

To accomplish this, I originally used a shell script with 5+ different tools installed on my system, but quickly realized this is just too hard to make sure works on any other computer. To that effort, I sat down and started writing a simple program with Haskell to do it.

To this effort, I knew that I could use the amazing [pandoc](https://pandoc.org/) library that I'd been using in executable form as a library to translate markdown to HTML with classnames for syntax highlighting, and that I could get a base CSS file to use with it from [here](https://github.com/jgm/highlighting-kate/blob/master/css/hk-pyg.css) (or generated from pandoc). If I could just parse my CSS file into a very simple structure and go through my HTML to remove classnames and add the inline styles, that'd be all I needed to do this.

*Ultimately, I used Haskell because it was fun and gave me working software for much less effort than would be required with any other languages I commonly use. Also because many smart people wrote these high-quality libraries which I then use for my nefarious purposes.*

## Code

To parse the CSS, we want to just get a list of my rules in a very simple structure. To this purpose, we can use [attoparsec](https://hackage.haskell.org/package/attoparsec/) since it gives us a fairly straightforward way to do parsing so that we can build up my list of rules easily.

To parse the HTML strings, we can use [TagSoup](https://github.com/ndmitchell/tagsoup) so we can have a flat list of tags to work with.

After doing that, all that would be left to do was to extract styles by matching rules to classnames to inline them into the HTML, removing classnames in the process.

### Parsing CSS

#### Modeling the problem with Types

Let's begin with some type definitions for what we want to work with. My hazy definition of CSS goes something like this:

1. A Rule consists of a Selector and Lines of attributes to be applied.
2. A Selector consists of text
3. A Line consists of a Property and its Value
4. A Property consists of text
5. A Value consists of text

Using these five, we can write out our types:

```haskell
data Rule = Rule Selector [Line]

newtype Selector = Selector Text

data Line = Line Property Value

newtype Property = Property Text

newtype Value = Value Text
```

A refresher for those unfamiliar with Haskell:

* a `newtype` is a type with a constructor of the same name as the type and one type argument, meaning that it basically creates "wrapper" types. We use these because type aliases don't provide the same level of guarantees. We can't mix up a Value with a Property, even though they both are newtypes of Text, whereas if we had simply used type aliases, many times I would accidentally pass one into the other. \*
* a `data` type is a type with a constructor of any name and any number of type arguments. The main benefit in this case is the same as with my newtypes in preventing tuples of type aliases from being used.

*\* I usually find myself using type synonyms and ending up shooting myself in the foot hundreds of times over in Typescript and Flow codebases. I find this to be a really frustrating experience and would advise others to use minimal classes with public/readonly properties instead to secure your codebases from bugs arising from such preventable cases.*

#### Writing our parser

Our parser is now written using these definitions and some building blocks.

First, we need a way to parse and remove comments.

```haskell
skipComments :: Parser ()
skipComments =
  string "/*" >> closeComment
  where
    closeComment =
      skipWhile (/= '*') >> string "*/" >> return ()
      <|> closeComment
```

By using the power of lazy evaluation, we're able to write this `skipComments` parser that reads in the begin comment sequence, throws away that result to start closing the comment using `(>>)` ("applySecond"), and in the closing of the comment, reads in the end comment sequence to actually return. If the closing of the comment fails, `(<|>)` ("alternative") allows us to simply provide `closeComment` as an alternative continue parsing.

By using the alternatives to essentially write parsers that will fail over, writing a general comment and whitespace eating parser is nice:

```haskell
skipSpace' :: Parser ()
skipSpace' =
  (skipComments >> skipSpace')
  <|> (takeWhile1 isSpace >> skipSpace')
  <|> return ()
```

Which then let us write the basic building block of our parser:

```haskell
lexeme :: Parser a -> Parser a
lexeme p = p <* skipSpace'
```

This building block lets us write parsers to start building our data structures, like so:

```haskell
parseProperty :: Parser Property
parseProperty = lexeme $
  Property . strip <$> takeWhile (\x -> x /= ':' && x /= ' ')
```

In English, "parse a lexeme where a Property is constructed with the stripping, mapped to the parser for a sequence that meets the condition that the character we are parsing is neither a colon nor an empty space".

Using these smaller parsers, we combine them (kind of like Super Sentai/Power Rangers?) into even larger parsers:

```haskell
parseRule :: Parser Rule
parseRule = do
  skipSpace'
  sel <- parseSelector
  void $ lexeme (char '{')
  ls <- many' parseLine
  void $ lexeme (char '}')
  return $ Rule sel ls
```

Refresher: For just about any type `f a` where it would "make sense", you can use `<-` in **do blocks** to extract `b` from `f b`. You can then `return` the `a` value in the block to get `f a` in the end.

### Extracting out the styles

Since we can now assume any user of our library should be able to parse CSS files using the parser we provided (or they will get a parsing error they will have to handle themselves), we can write code using our Rule type.

We'll write a function of our Rules and a list of classnames to create the inline-style string we'll be replacing the class attribute with.

1. We'll concatenate the lines from all rules that have matched to a classname.
2. We'll extract each line's property and value and just format it as `prop: val;`.
3. We'll match a rule based on if the classname (with the dot prefix) is an infix string of the selector, in which we will extract the lines and concatenate them for the rule.

This looks like the following:

```haskell
extractStyles :: [Rule] -> [String] -> String
extractStyles rules classNames =
  concat $ applyRule =<< classNames
  where
    extractLine (Line (Property prop) (Value val)) =
      unpack prop ++ ":" ++ unpack val ++ ";"
    applyOnMatch match (Rule (Selector sel) ls) =
      if pack match `isInfixOf` sel
        then return $ concat $ extractLine <$> ls
        else mempty
    applyRule cn =
      applyOnMatch ("." ++ cn) =<< rules
```

This seems dense, but the individual levels don't have too much going on, and in the end, the compiler will guarantee type safety for this function.

And so this function will convert a list of classnames into one long inline-style for rule matches.

### Parsing our HTML and applying our transforms

TagSoup will do the parsing heavy lifting for us here, parsing our HTML string into a list of `Tag`s. This data type is defined as such:

```haskell
data Tag str -- essentially allowing any feasible string type
   = TagOpen str [Attribute str]
   | TagClose str
   | TagText str
   -- and others
```

Since we need to style any open tags (since that's where inline styles go), we will only be concerned with transforming `TagOpen`s.

And so, using TagSoup, we parse our HTML string into `[Tag]`, map a transform to it, and then collect the new `[Tag]` to convert into a new HTML string. For any open tags, we will using the TagSoup helpers to get the classnames from the tag to get the inline-style to apply, filter out the class property, and then create a new `TagOpen` to boot:

```haskell
replaceClassnames :: [Rule] -> String -> String
replaceClassnames rules html =
  renderTags $
    replaceClass <$>
    parseTags html
  where
    extractClassNames = words . fromAttrib "class"
    replaceClass tag@(TagOpen name attrs) = do
      let style = extractStyles rules $ extractClassNames tag
      -- throw away class
      let attrs' = filter ((/= "class") . fst) attrs
      TagOpen name $ [("style", style) | style /= ""] ++ attrs'
    replaceClass tag = tag
```

And that's about it!

## Conclusion

I wrapped this up by creating a short command-line program that uses my library code to parse CSS and transform HTML strings for use [here](https://github.com/justinwoo/md2sht/blob/master/app/Main.hs). Most of the code is just plumbing around parsing command line arguments and calling pandoc to convert my markdown into HTML, so it isn't too interesting to read.

I hope this has shown you how you can make your own data structures in Haskell to guide your programming, how writing a parser with Haskell libraries can be straightforward, and how existing high-quality libraries can help you get a lot done without too much work on your end (so you can focus on making things **you** want).

Big thanks to my coworker [@phadej](https://twitter.com/phadej) for suggesting attoparsec to me and helping clean up and fix my code!

Please let me know ([@jusrin00](https://twitter.com/jusrin00)) what you think about this post and if you have suggestions for changes! And thanks for reading!

## Links

* md2sht: https://github.com/justinwoo/md2sht/
* attoparsec: https://hackage.haskell.org/package/attoparsec/
* tagsoup: https://github.com/ndmitchell/tagsoup
* pandoc: https://pandoc.org/
