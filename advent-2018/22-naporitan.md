To group a bunch of proxies together either for convenience or for RowToList-directed type-level verification, you might choose to put them in a record. However, this usually leaves us in a position where we then need to go through and write out the value of our proxy literally, leading to a load of repetition and pain, where we need to make sure we have actually declared all of the fields in our record of proxy types below.

If PureScript has taught us anything, it's that if we have static knowledge about something, we should be able to derive its value.

## "Reflecting a record of proxies and keys of row types"

In this post, I wrote about how we can once again use RowToList with the combination of Record.Builder to reflect a record of proxies into its value:

<https://qiita.com/kimagure/items/b08175d22f9950ba3dfb>

Just like that, we don't need to write any error-prone boilerplate, since we can use the compiler to derive the value:

```hs
proxies ::
  { apple :: Proxy Int
  , banana :: Proxy String
  }
proxies = N.reflectRecordProxy
```

I hope even if this example seems a bit silly, this really gives you some ideas on how powerful languages really can derive values for you from the types, and how this is really incredibly useful when you make a conscious choice of what you'd like to get out of your system. Otherwise, there would be a much smaller difference between PureScript and other languages.
