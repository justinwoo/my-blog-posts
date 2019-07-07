# Records/interfaces are not StringMaps/hashes/objects and vice versa

This is an excerpt from https://speakerdeck.com/justinwoo/rowlist-fun-with-purescript-2nd-edition

It's a common question to ask, "why are records any different from hashes?". This seems to stem from people wanting to apply knowledge of something familiar (JS objects) to something unfamiliar (statically typed records), but hopefully it's easy to follow along the two main differences: the difference in the values and their label/type association, and the difference in the existence of a value at a given key.

## Records are mostly heterogeneous, objects/hashes are homogeneous

Most of the time, record definitions will have multiple types for values. On the other hand, a StringMap will have only one type for the contents that can be used.

```ts
type Record = { a: string, b: int };

type StringMap = { [key: string]: string };
```

### “I handle multiple types”

There are three options for storing multiple types in a StringMap

* Dynamic/Foreign/Object/any -- a.k.a. Implicit coercion hell
You can choose to throw away the type system here and try to work by implicit knowledge of the system to coerce the type of values you look up. This depends entirely on you not making mistakes forever. Not just now, not just next week, literally forever.

* Polymorphic variants
I use this sometimes too, but… why? Never mind that if you know what polymorphic variants are, you probably don't need this article.

* Sum types
Yes, it can be useful to write a function for a given sum type and put it in here, but you still lose the actual association of the key to the value.

* Unions
In TypeScript/Flow you may be tempted to use unions of all of your possible types. You can even use type guards to discriminate the members of the union apply a function accordingly, but this is just a normal operation with homogeneous maps. There are not multiple types here, just a single type that is a union of possible members. You will still lose the association between the key and the value's type.

*edit: I remembered now that Elm confusingly calls sum types "union types" even though union types in other languages refer to untagged union, where you don't get a constructor to match against. For Elm's "union types", see the previous section on sum types.*

**If you use any of the above, you now have N * M possible values instead of N (N fields, each field can be one of M)***

### Common mistake: mixing up multiple values of a type with multiple types of a kind

People often mistake that because multiple *values* are possible, so must multiple *types*, but this is at the wrong level. Talking about "type of types" is a wholly different subject.

### Limitations in coercion

* Can’t naively coerce most records into string maps, though that can be very useful!
If you have a homogeneous record, then coercing this into a StringMap can be useful in some cases. I find usually that this is a sign that the type system you're working with isn't helpful enough, as you reach for this when you don't have generic operations on records using labels/type-level strings/Symbols. This is a common problem with using Elm or OCaml/ReasonML, but even in those cases, it is probably then worthwhile to build up a StringMap that corresponds to functions that can take the record and transform a specific field into a desired output and keep the record than to convert any static structure into a StringMap.

* But you will never convert a string map to a record without parsing or unsafe coercion
This is related to the next point, but if you have an object that may or may not have all the fields of a record defined, then it is not a valid operation to coerce it into a homogeneous record.

## Static keys -- there are explicit guarantees of keys existing in a record

Records also have their fields statically defined -- for each key in the record, there **must** be a type associated, and that **must** have a value in definition. There is a facile argument by which people say "what if it might not be defined"?, but we have two wholly different approaches that do that already: sum types (`Maybe a`) and nullable types (`a | null`).

### Lookups for StringMaps go to Maybe/Option/Nullable/Undefinable

So for a record, a lookup is always straightforward, as the field for a given key will always exist. For example, `{ a: string }` can always be looked up by applying `_.a` to get a `string` out.

For a StringMap, such a simple operation does not exist. As the field may not exist, every lookup results in `Maybe a` (or `undefined | a` and whatnot), such that:

```ts
const stringMap: { [key: string]: string } = {a: "hello"};

stringMap.a; // string | undefined, where the value is string
stringMap.b; // string | undefined, where the value is undefined
```

### “I know it exists”

There are ways to encode that you know that a value exists.

* Unsafe coercion from Maybe a to a
You can use implicit knowledge to coerce lookups to unwrap them, or eliminate the `undefined` member from your unions. You have probably already been screwed by this in prod multiple times, and there's no reasonable explanation why you keep doing this.

* Coercion from phantom type parameter evidence
This does not work with TypeScript, and I do not know of how it would be done with Flow. Either way, you can try to use phantom type parameters to store information about what fields will always be defined. This has its uses and can be fairly useful, but isn't what you want a vast majority of the time.

```hs
newtype MyMap (alwaysDefined :: # Cond) = MyMap (StrMap a)
getApple :: forall a cond trashSubRow alwaysDefined
   . RowCons “apple” cond trashSubRow alwaysDefined
   => MyMap alwaysDefined a -> a
```
* Refinement types
Well, if you know what refinement types are and use them, you probably have no need for this article. Otherwise, I hope you enjoy Liquid Haskell or something?

## Conclusion

Hopefully this has shown that records and StringMaps are completely separate things. If not, I hope you'll try to keep some of these ideas in mind when reading more into this.