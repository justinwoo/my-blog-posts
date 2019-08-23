# English

With most libraries that work purely with instances, working with an exception is quite annoying. Take for an example the case where you have a record of a couple different fields. If all of the fields have an instance for your decoding type class, everything works just fine. What if just one of those fields is actually a date, but in JSON is represented as a state? Then your entire record no longer has an instance for decoding.

But surely, we should be able to hot swap this, right? There's no catch-all instance for printing and parsing date strings, since everyone likes to use different formats. Sure, we might newtype the field itself, but then we pay the penalty of having to carry around a newtype for whatever date type we use elsewhere. We could also newtype the type that carries this type, but then we need to manually write out how the other fields of the record are read too. Surely there must be some kind of solution where we can tell the compiler to do the default decoding of all of the other fields save one, which we'll handle ourselves.

Well, considering that PureScript has anonymous record types, couldn't we get the compiler to infer some other type here that we can work with? Yeah, we can!

## "Modified JSON parsing for free with PureScript-Simple-JSON"

In this post, I talked about how we can get the compiler to infer for us a type that is decoded from some JSON that we can then further manually decode and replace properties.

<https://qiita.com/kimagure/items/801e1c55d4f8f218f11e>

Since I wrote this post, I also wrote a page in the Simple-JSON guide:

<https://purescript-simple-json.readthedocs.io/en/latest/inferred-record-types.html>

In short, inferred record types let us to do this:

```hs
type RecordMisnamedField =
  { cherry :: Int
  }
  
readRecordMisnamedField :: String -> Either Foreign.MultipleErrors RecordMisnamedField
readRecordMisnamedField s = do
  inter <- JSON.readJSON s
  pure $ Record.rename grapeP cherryP inter
  where
    grapeP = SProxy :: SProxy "grape"
    cherryP = SProxy :: SProxy "cherry"
```

And this will read JSON with `grape :: Int` from the inferred context.

---

# Japanese

純粋な環境で動作するライブラリでは、例外の処理を行うことは非常に面倒です。異なるフィールドを持つレコードを扱う場合の例を考えてみましょう。もしもすべてのフィールドに対して型クラスからのデコードが実装されているのであれば、問題なく動作するでしょう。これらのフィールドのうちの1つだけが実際には日付ですが、JSONでは状態(null 許容)として表される場合どうでしょうか。これは、レコード全体がデコードの実装を持たなくなることを指しています｡

こういったとき好きに変換することはできないのでしょうか。誰もが異なったフォーマットを使うのですから、日付文字列の出力と解析を行う万能の実装はないでしょう。フィールドを新しい型に組み込む場合、他の場所で使っているdata型については、新しい型を持ち歩かなければならないというペナルティを受け入れる必要があります。この型を持つ型を新しい型にすることもできますが、レコードの他のフィールドにも、どのように読み出しを行うかを手動で書き出す必要があります。他のすべてのフィールドを既定のデコードで動作するようコンパイラに指示することができる解決策がなければなりません。

PureScriptに匿名レコード型があることを考慮すると、他の型をコンパイラに推論させることはどうでしょうか。そう、これは可能なのです。

## "PureScript-Simple-JSONを使用して自由にJSONを修正する"

以下の記事では、JSONからデコードされた値を手動でデコードしてプロパティを変更する型をコンパイラに文脈から推論させるために、どのように記述するかを話しました｡

<https://qiita.com/kimagure/items/801e1c55d4f8f218f11e>

この記事にも書いたように、Simple-JSON用のドキュメントをこのようにまとめました。

<https://purescript-simple-json.readthedocs.io/en/latest/inferred-record-types.html>

簡単に言えば、推測されたレコード型は、以下のように使用できます。

```hs
type RecordMisnamedField =
  { cherry :: Int
  }
  
readRecordMisnamedField :: String -> Either Foreign.MultipleErrors RecordMisnamedField
readRecordMisnamedField s = do
  inter <- JSON.readJSON s
  pure $ Record.rename grapeP cherryP inter
  where
    grapeP = SProxy :: SProxy "grape"
    cherryP = SProxy :: SProxy "cherry"
```

これは推論された前後関係から `grape :: Int`を使ってJSONを読み込みます。

---

Thanks to @noolbar for the Japanese translation!
