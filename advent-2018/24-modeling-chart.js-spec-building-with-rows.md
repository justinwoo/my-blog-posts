# English

While we've previously talked about ways to model JS charting library specs by using Row.Union, another interesting viewpoint to look from instead is how to allow for building an arbitrary spec just based on restricting what properties can be set based on what kind of chart we are building.

## "Using Rows and RowToList to model Chart.js spec building"

In this article, I talked about how we might model spec-building for Chart.js, by using a row type to specify which functions for builder components are valid for which types of charts:

<https://qiita.com/kimagure/items/fd05ad13ee8def0fb4ed>

The problem that this article mainly focused on was that we needed to get the intersection of two row types when composing chart builders:

```hs
newtype ChartBuilder
  (appliesTo :: # Type)
  (input :: # Type)
  (output :: # Type)
  = ChartBuilder (Builder (Record input) (Record output))
```

So to compose two of these chart builders together, we need to make sure that the new chart builder that we output has a `appliesTo` parameter that is valid. So given the composition of `( a :: _ , b :: _ )` and `( a :: _ )`, we need to produce the output `( a :: _ )`.

While it would be convenient if we could have multiple constraints solved simultaneously to allow us to express a system of Union constraints, we do need to actually implement this in terms of RowToList as only one constraint can be fully determined at a time. I wrote about this problem more in the follow-up article, "Making Diffs of differently-typed Records in PureScript", so I hope you'll read through it sometime:

<https://qiita.com/kimagure/items/ca229cb4ba76db0c24a8>

---

# Japanese

以前に、Row.Unionを使用してJSのグラフライブラリ仕様をモデル化する方法について説明しましたが、目的とするグラフでどのような種類のプロパティを設定できるかを制限し、これに基づいて任意の仕様を構築する方法があります。

## "row型とRowToListを使ってChart.js仕様をモデル化する"

以下の記事では、row型を使用してビルダーコンポーネントの関数がどの型のグラフに対して有効かを指定することによって、Chart.jsの仕様作成をモデル化する方法について説明しました。

<https://qiita.com/kimagure/items/fd05ad13ee8def0fb4ed>

この記事で注目した問題は、`ChartBuilder`を作成するときに2つのrow型の合成を取得する必要がある点です。

```hs
newtype ChartBuilder
  (appliesTo :: # Type)
  (input :: # Type)
  (output :: # Type)
  = ChartBuilder (Builder (Record input) (Record output))
```

これら`ChartBuilder`の2つを一緒にするには、出力する新しい`ChartBuilder`に有効な `appliesTo`パラメータが有効であるかを確認する必要があります。そのため、 `( a :: _ , b :: _ )`と `( a :: _ )`の構成を考えると、出力`( a :: _ )`を生成する必要があります。

Union制約のシステムを表現するために複数の制約を同時に解決できるのであれば便利ですが、一度に1つの制約しか完全に決定できないため、RowToListに関して実装する必要があります。この問題については、"PureScriptで異なる型のレコードの差分を取る"という記事で詳しく書かれていますので、時間があれば読んでください。

<https://qiita.com/kimagure/items/ca229cb4ba76db0c24a8>

---

Thanks to @noolbar for the Japanese translation!
