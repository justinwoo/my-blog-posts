Many people really like data types in PureScript, for very good reasons. Data types are actually really amazingly good, especially sum types. The best part about data types is that we have various ways to pattern match on them, matching on where we have constructors of data types and their arguments.

What happens when we want to actually get information about our data types? If we think about how our data types are defined, we can extract some more general information about our types:

```hs
data Fruit = Apple | Banana String

-- We have a data type Fruit
-- for which we have a Sum of two arguments:
-- * Constructor "Apple", of no arguments
-- * Constructor "Banana", of one argument:
--   * String
```

We could go and define a generic set of data types that describe this shape, but we don't need to, as this is a well-known concept known as Datatype Generics, where the compiler can derive the generic form for us to use:

```hs
import Data.Generic.Rep as GR
import Data.Generic.Rep.Show (genericShow)

data Fruit = Apple | Banana String

-- note the underscore at the end for the `rep` parameter of class Generic
derive instance genericFruit :: GR.Generic Fruit _

instance showFruit :: Show Fruit where
  show = genericShow
```

So in this example, we were able to get an implementation of `show` for free, by using the datatype generics implementation of `show` with a compiler-derived instance of `Generic` for `Fruit`.

## Datatype-Generic programming for generating TypeScript code from Purescript

In this post, I go over how we can use datatype generics for codegen of TypeScript types from some PureScript type definitions.

<https://qiita.com/kimagure/items/cc0ea2982abdf1625e87>

While the newest version of the library talked about in the article now uses RowToList as TypeScript code generation largely targets Records and Variants, the ideas in it are more broadly applicable to any data types. Indeed, in PureScript 0.12, we no longer generate the generic representation for records (and that's a good thing, leave unto RowToList what is RowToList's), but we still use datatype generics for data types. 

A more readily applicable tutorial on how to use generics-rep can be found here:

<https://purescript-simple-json.readthedocs.io/en/latest/generics-rep.html>
