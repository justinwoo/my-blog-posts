Many times when we deal with errors from impure routines in PureScript applications, we have to think about errors at multiple stages. Is it an exception from the runtime that is caught as an Error? Or is it an error that we expect to handle in some way?

And while many will end up using `ExceptT` to model the expected errors, I don't find this particularly pleasing to use with impure code, where I still end up with two different layers of errors to handle.

What if I could just put information in an Error? I would really like to use the normal errors too, but the problem is that I can only store strings inside of the normal JS Error, so I have to find another way to transport error information. But is it really far off from using Error?

## "Pseudo-dynamically typed errors in PureScript"

In this post, I talk about putting my error information in a Variant value, and putting it into a subclass of Error:

<https://qiita.com/kimagure/items/71e938ee93e31bd2e79b>

By using a subclass of error, I can stuff it into wherever Error goes, and then I can retrieve this error information by using some reading operations, where I can make sure that 1) I have an instance of my VariantError class and 2) the key "type" in the runtime representation of the Variant contained is within the set of keys expected.

And by using this, I no longer need to handle errors on multiple levels and can get away with not having to deal with `ExceptT MyError Aff`. While many might disagree with this approach, this really does make working with my code much easier, especially for people who are not familiar with transformers.
