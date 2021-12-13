# Problems with updating PureScript libraries

For a couple of years, I used to deal with having to update my PureScript libraries for new versions of the PureScript language. With the latest 0.14.x series of releases, I took a break to not actually attempt to update all of my libraries, instead relying on PRs to actually update the few of the libraries that other people needed. This actually ended up being more than a handful, to my surprise, but in the end, I think a lot of them also could be simply copy-pasted into people's projects without much problems.

In my experience, there are many reasons why many libraries do not get updated to newer versions of PureScript, and why people who are no longer interested in engaging with the "PureScript community" often choose to stop using PureScript rather than attempt to update their libraries and projects.

## Libraries change in backwards-incompatible ways for no useful reason

In PureScript, there is a tendency to change libraries in backwards-incompatible ways for no useful reason. These changes are often meant to "encourage correctness" or somehow match the author's aesthetic tastes for what code looks like, but serves no actual functional change that would not have been met by introducing a new definition. Such, by changing just the type signature of the code, upgrades are stopped by this.

By forcing any consumers of libraries to deal with largely meaningless changes to adhere to aesthetics, the authors of libraries quickly lose the faith of consumers of stability, and consumers who suffer from being hit by these dozens of times soon find the effort to keep using PureScript more burdensome than the benefits provided. Authors of PureScript libraries (and indeed, any language with fewer than ~10k active users) often live in some kind of delusional world in which changes to their library are so important that everyone must pay attention to them. To compound: what better time to introduce these changes than to when people need to update to new versions of the PureScript compiler?

### Example: `fromLeft`

One example would be `fromLeft`, which was changed from a function of `Partial` constraint (meaning that you would need to explicitly mark where you used this potentially dangerous function with implicit proofs) to one that would take a "default parameter". A careful reader would ask why you would need such a change if you could 1) introduce a new function which didn't destroy existing usages and prevent friction-less updates or 2) anyone who actually wished to have this new behavior wouldn't already use pattern matching or some other method instead. The simple answer is that the authors who make these changes wish to explicitly inform you that you are guilty of using this function in the past, and that you should repent by manually changing your implementation accordingly (except: you probably used this because of some low-level detail instead of just trying to find a shortcut).

Consider what one gets as an error message when trying to upgrade:

```purs
  Could not match type

    Either (Either l1 r5) t4 -> Either l1 r5

  with type

    l1


while trying to match type t2 (Either t3 t4 -> t3)
  with type t0 l1
while checking that expression (unsafePartial (map fromLeft)) ((filter isLeft) xs)
  has type t0 l1
in value declaration compactableEvent

where l1 is a rigid type variable
        bound at (line 0, column 0 - line 0, column 0)
      r5 is a rigid type variable
        bound at (line 0, column 0 - line 0, column 0)
      t0 is an unknown type
      t2 is an unknown type
      t3 is an unknown type
      t4 is an unknown type
```

Many users know at this point that whatever dependency they used before has changed its API, but not many are so willing to look for the cause. In many cases, even this kind of type error will make users give up on making updates to their library.

## There is no provided tooling to automatically update libraries

While one can thankfully use tools like npm-check-update for dependencies from Bower, the actual process of updating usages of libraries and language core features does not have the same luxury. In addition to the above, if you consider new changes such as the polykinds changes, you routinely end up with errors like these:

```purs
  Could not match kind

    Type -> Type

  with kind

    Type


while checking that type RowList
  has kind Type
```

These end up being a variety of nuisances for some and absolute road blocks for less experienced users.

### There is no good, simple tooling for making code changes

The PureScript syntax is a mess, there's no decent grammar, and there is no tooling to make AST changes easy, all leading to that there are no easy ways to make automatic version migration tools for PureScript. But really, there is no real need, as there are not that many actual lines of code to bother with. But as there are not that many people actually going around to make these updates, there still is some need for automatic tooling, but it will not happen with the current or future state of PureScript.

## What's the point of many of these changes? i.e. why even update?

Many compiler changes either cater to some crowd that want esoteric changes (polykinds) or are small enough changes that do not actually help you when working with PureScript (automatic instance name generation). Bugs with how the PureScript compiler constructs build plans to try to attempt to make compiled artifacts match the sources, various bugs and deficiencies with compiler errors, development feedback, and debugging, lack of fixes and newer features that would improve interoperability with JavaScript, and more all contribute to little being offered to users to update to newer versions of PureScript. Many of the most useful changes to PureScript ended with the introduction of functional dependencies, datatype generics, and arguably row type solving.

## "Don't just complain about it"

While some may challenge me to try to introduce my own changes to the PureScript compiler and ecosystem, I would say that I already have attempted to talk to many people about this and perform actions of my own. In some ways, I have changed PureScript, but only for the worse: the usage of Dhall in Package Sets is largely meaningless and yet another source of annoyance in using PureScript. After even just a couple of years, I already gave up, not because of the technical problems, but because of the social ones.

## Conclusion

This is an incomplete article that doesn't truly explore enough of what I think has gone wrong with PureScript and what it tries to offer its existing and potential users in its updates. I write this mostly so that when I discuss this for the some-hundredth time with my discussion groups that I have something that can act as a poster-board.
