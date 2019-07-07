# Using Tree-sitter from PureScript

Recently, I started a project to format Nix files, by making use of the [`node-tree-sitter`](https://github.com/tree-sitter/node-tree-sitter) bindings and the [`tree-sitter-nix`](https://github.com/cstrahan/tree-sitter-nix) parser. I used the node bindings for tree-sitter as it was immediately usable without any extra build setup, and since I could use PureScript to write the actual logic that needed to be performed.

## Tree-sitter in short

Tree-sitter parsers are written by your own definition of a grammar. Since Nixlang does not have misfeatures as indentation based syntax, much of the grammar can be written normally without too much trouble. For example, a "quantity" can be defined using a sequence containing literals and an anonymous "expression" rule:

```js
    quantity: $ => seq('(', $._expr, ')'),
```

These rules are collected in a `grammar.js`, which is then evaluated and analyzed to produce a bunch of C code by [`tree-sitter`](https://github.com/tree-sitter/tree-sitter).

With the above rule, we have two kinds of nodes in tree-sitter land: "named" nodes and "unnamed" nodes. "Named" nodes are those with a regular name and an actual property in our grammar rules. "Unnamed" nodes are matches on literals and properties with an underscore preceding the property name, such as this `_expr` here. For example, look at this test input:

```
====================
parens
====================

(123)

---

(expression
    (quantity (integer)))
```

In the print representation, only the named nodes are shown. We can simply print out all children using the `node-tree-sitter` binding:

```js
const Parser = require("tree-sitter");
const Nix = require("tree-sitter-nix");

const parser = new Parser();
parser.setLanguage(Nix);

const sourceCode = `(123)`;

const tree = parser.parse(sourceCode);

function nodeToObject(node) {
  return {
    type: node.type,
    rawChildCount: node.children.length,
    rawChildren: node.children.map(x => nodeToObject(x))
  };
}

var result = nodeToObject(tree.rootNode);
```

The result is the following:

```json
{
  "type": "expression",
  "rawChildCount": 1,
  "rawChildren": [
    {
      "type": "quantity",
      "rawChildCount": 3,
      "rawChildren": [
        {
          "type": "(",
          "rawChildCount": 0,
          "rawChildren": []
        },
        {
          "type": "integer",
          "rawChildCount": 0,
          "rawChildren": []
        },
        {
          "type": ")",
          "rawChildCount": 0,
          "rawChildren": []
        }
      ]
    }
  ]
}
```

If we only wanted the named children above, we would have to filter the children by the `isNamed` boolean property:

```js
function nodeToObject(node) {
  const namedChildren = node.children.filter(x => x.isNamed);
  return {
    type: node.type,
    // rawChildCount: node.children.length,
    // rawChildren: node.children.map(x => nodeToObject(x)),
    childCount: namedChildren.length,
    children: namedChildren.map(x => nodeToObject(x))
  };
}
```

This also means that "type" is not actually a useful quantity without being combined with `isNamed`. Take the expression `let a = 1; in a`. The subset of the output looks like this:

```js
// ... "rawChildren": [
    {
      "type": "let",
      "isNamed": true,
      "rawChildren": [
        {
          "type": "let",
          "isNamed": false,
          "rawChildren": []
        }, // ...
```

While we have a rule for `let` expressions, the `let` literal is still shown in the raw children. This will still be useful for us though, as while only binds are legal entities inside of the let-in expression, you can still have comments that disrupt the flow.

## Project setup

As usual, I have a Psc-Package project with the following structure:

```
psc-package.json  # psc-package config
package.json      # npm config, for getting node libraries

src/              # source files go here
  FormatNix.purs
  FormatNix.js
  Main.purs
  Main.js
test/             # test files
  Main.purs
```

In `package.json`, I'll put in the dependencies we need: the node tree-sitter bindings and the tree-sitter Nix parser:

```json
  "dependencies": {
    "tree-sitter": "^0.14.0",
    "tree-sitter-nix": "github:justinwoo/tree-sitter-nix#quantity"
  },
```

## Foreign imports

To bring in the things we need, we write some definitions into `FormatNix.js` and export them.

```js
const Parser = require("tree-sitter");
const Nix = require("tree-sitter-nix");

exports.nixLanguage = Nix;

exports.mkParser = function(language) {
  const parser = new Parser();
  parser.setLanguage(language);
  return parser;
};
```

This is about the only thing we really directly need from FFI. The rest can be done within PureScript.

Let's take care of some of our imports. First, the `nixLanguage` import should be handled with care, where we can make sure we only use this in the context of handling tree-sitter languages. We can declare a data type where values come from foreign values and set the type of the import to it.

```hs
foreign import data TreeSitterLanguage :: Type
foreign import nixLanguage :: TreeSitterLanguage
```

The kind signature `Type` is simply declaring that this is a `Type` type, so it can have values, as opposed to `Symbol`, `# Type`, etc.

For `mkParser`, we need similar, where we need to declare that there is such a thing as an opaque tree-sitter parser:

```hs
foreign import data TreeSitterParser :: Type
foreign import mkParser :: TreeSitterLanguage -> TreeSitterParser
```

From here, we can use our old friend `unsafeCoerce` to define specific types for things we need out of the tree-sitter. Take care though, to not introduce unsafeCoerce usages outside of specific small functions.

```hs
foreign import data Tree :: Type -- parse result of the tree
foreign import data Node :: Type -- individual nodes from the tree

parse :: TreeSitterParser -> String -> Tree
parse parser contents = parser'.parse contents
  where parser' = unsafeCoerce parser :: { parse :: String -> Tree }

rootNode :: Tree -> Node
rootNode tree = tree'.rootNode
  where tree' = unsafeCoerce tree :: { rootNode :: Node }
```

## Working with Nodes

To work with the Node type, we should define some utility functions to grab things out of them.

```hs
-- getting children as above
children :: Node -> Array Node
children tn = tn'.children
  where tn' = unsafeCoerce tn :: { children :: Array Node }

-- | Filter for named children
namedChildren :: Node -> Array Node
namedChildren = Array.filter isNamed <<< children

-- | Is a given Node Real or is it fake?
isNamed :: Node -> Boolean
isNamed tn = tn'.isNamed
  where tn' = unsafeCoerce tn :: { isNamed :: Boolean }

-- | Get the text contents of a node
text :: Node -> String
text tn = tn'.text
  where tn' = unsafeCoerce tn :: { text :: String }

-- | A helper TypeString type, so we can keep track of this
newtype TypeString = TypeString String
derive instance newtypeTypeString :: Newtype TypeString _
derive newtype instance eqTypeString :: Eq TypeString

-- | Read the "type" of the node
type_ :: Node -> TypeString
type_ tn = tn'."type"
  where tn' = unsafeCoerce tn :: { "type" :: TypeString }
```

And that's it. The building blocks needed to consume tree-sitter parsers from PureScript are all there.

## `module FormatNix`

I won't dive into the code because it's ever changing and mostly boring, but it mostly comes to three parts:

### `data Expr`

I have a giant data type that acts as an AST, which I transform the parse result into to later print out. It contains all of the Nix language elements handled:

```hs
data Expr

  -- top level
  = Expression (Array Expr)

  -- i just want to print comments
  | Comment String

  -- "hi"
  | StringValue String

  -- set fn args, e.g. inside of braces { pkgs ? import <nixpkgs> {} }:
  | Formals (Array Expr)

  -- set fn arg with an identifier,
  -- where it may or may not have a default value expr
  | Formal Expr (Maybe Expr)

  -- ...

  -- unknown node type, with the type string and text contents
  | Unknown String String
```

### `readNode :: Node -> Expr`

This function converts a node (and its children) to `Expr`.

```hs
readNode :: Node -> Expr
readNode n = readNode' (type_ n) n

readNode' :: TypeString -> Node -> Expr
readNode' (TypeString "comment") n = Comment (text n)
readNode' (TypeString "function") n
  | (input : output : Nil ) <- List.fromFoldable (readNode <$> namedChildren n)
    = case input of
        Formals _ -> SetFunction input output
        _ -> Function input output
  | otherwise = Unknown "function variation" (text n)
```

### `expr2Doc :: Expr -> Doc`

Finally, I have a function to convert `Expr` to `Doc`, the structure as described in "A Prettier Printer" by Wadler in 1997.

```hs
-- | "a prettier printer" by wadler
data Doc
  = DNil
  | DAppend Doc Doc
  | DNest Int Doc
  | DText String
  | DLine
  | DAlt Doc Doc

expr2Doc :: Expr -> Doc
expr2Doc (StringValue str) = DText str
expr2Doc (Unknown tag str) = DText $ "Unknown " <> tag <> " " <> str
expr2Doc (List exprs) = left <> choices <> right
  where
    inners = expr2Doc <$> exprs
    left = DText "["
    right = DText "]"
    choices = DAlt oneLine asLines
    oneLine = dwords inners <> DText " "
    asLines = (DNest 1 (dlines inners)) <> DLine
-- ...
```

And this is really about it.

## Conclusion

Hopefully this has given you a rough overview of how you can use node-tree-sitter from PureScript, and how FFI can be minimal and allow you to stretch what you can work with beyond what already exists in the language and its core libraries.

## Links

* This project <https://github.com/justinwoo/format-nix/>
* Tree-sitter <https://tree-sitter.github.io/tree-sitter/>
