---
title: Using Rows and RowToList to model Chart.js spec building
tags: purescript
author: kimagure
slide: false
---
Recently, I've been wondering about how I could model building specs for libraries like Chart.js as glorified wrappers around [Record.Builder.insert](https://pursuit.purescript.org/packages/purescript-record/docs/Data.Record.Builder#v:insert), so that I could correctly compose a bunch of insertion operations with the correct constraints. And while this is often solved using typeclasses to provide instances for each chart type that may use a certain property, I saw this as a prime opportunity to use row types instead to use the labels to describe which kinds of charts a function can be applied to.

## The Chart.js spec

Chart.js usage for creating a bar chart looks roughly like this:

```js
new Chart.Bar(context, {
  data: {
    labels: ["red", "green", "blue"],
    datasets: [{
      type: "bar",
      label: "# votes",
      data: [1,2,3],
      // ...
    }]
  },
  options: {
    // ...
  }
});
```

And as most of the configuration goes in `data.datasets`, I set out to make constructing the records easy.

## Newtyped Record.Builder

While I would use Record.Builder to put this together, I needed to newtype this so that I could restrict what chart type specific dataset properties could be used, so that I could make sure I used the correct properties with the correct types. And so:

```hs
newtype ChartBuilder
  (appliesTo :: # Type)
  (input :: # Type)
  (output :: # Type)
  = ChartBuilder (Builder (Record input) (Record output))
```

This newtype wraps Builder and provides the input and output directly, with the phantom type parameter `appliesTo`, which is a row type that I use with fields of `[label] :: Unit`, e.g.:

```hs
label :: forall input
   . RowLacks "label" input
  => String
  -> ChartBuilder
       ( lineDataDataset :: Unit
       , barDataDataset :: Unit
       )
       input
       (label :: String | input)
label x = ChartBuilder (Builder.insert (SProxy :: SProxy "label") x)
```

In this case, `label` is a dataset property that applies to both line and bar charts.

## Composing ChartBuilder

While I can compose the inner Builders directly, I needed to handle the `appliesTo` parameter by making sure to only save the fields that were defined in the builders I wanted to compose together.

This is when RowToList is especially helpful -- not only does it convert row types to `RowList`s that I can iterate, it also sorts the fields by the labels. By using this property, I can write a typeclass for getting the intersection:

```hs
class RowListIntersection
  (acc :: RowList)
  (xs :: RowList)
  (ys :: RowList)
  (res :: RowList)
  | acc xs ys -> res
```

Then it's time to do some dynamic programming on the type level. For the cases when I've reached the end of either list, I know there are no more matches and I can return my accumulate early.

```hs
instance rliNilXS :: RowListIntersection acc Nil trash acc
instance rliNilYS :: RowListIntersection acc trash Nil acc
```

In the Cons-Cons case, while I can always match the types being the same between my two rows, the actual symbols may differ. To correctly iterate the row lists and accumulate my result, I rely on the Boolean and Ordering modules defined in Typelevel-Prelude to help me with the conditions:

* If the labels of X and Y are the same, then add the label to my accumulate for the nested RowListIntersection constraint. If not, reuse the existing accumulate.
* If the label of X is less than Y, then this means that my left side row list is "behind" the right. In this case, I use the tail to shift this left side along. For the case when the symbols are equal, I also need to shift this left side along, as there is no more use for this specific label. In the case when the X label is greater, I actually need to keep the list as is.
* As the complement of X, the opposite conditions apply, where if X is less than Y, then I will keep the right rowlist the same in the constraint. Otherwise, the row list may be iterated.

Put into code, my constraints end up looking like this:

```hs
instance rliConsCons ::
  ( CompareSymbol xname yname ord
  , Equals ord EQ isEq
  , Equals ord LT isLt
  , Or isEq isLT isEqOrLt
  , If isEq
      (RLProxy (Cons xname ty acc))
      (RLProxy acc)
      (RLProxy acc')
  , If isEqOrLt
      (RLProxy xs)
      (RLProxy (Cons xname ty xs))
      (RLProxy xs')
  , If isLt
      (RLProxy (Cons xname ty ys))
      (RLProxy ys)
      (RLProxy ys')
  , RowListIntersection acc' xs' ys' res
  ) => RowListIntersection acc (Cons xname ty xs) (Cons yname ty ys) res
```

Finally, I can use this typeclass to define composition of my ChartBuilder, where the intersection of my appliesTo rows will be kept.

```hs
composeChartBuilder
  :: forall
       app1 app1L
       app2 app2L
       app3 app3L
       a b c
   . RowToList app1 app1L
  => RowToList app2 app2L
  => RowListIntersection Nil app1L app2L app3L
  => ListToRow app3L app3
  => ChartBuilder app1 a b
  -> ChartBuilder app2 b c
  -> ChartBuilder app3 a c
composeChartBuilder (ChartBuilder builder1) (ChartBuilder builder2) =
  ChartBuilder $ builder2 <<< builder1
```

And for convenience, I define an operator for composing these ChartBuilders:

```hs
infixr 9 composeChartBuilder as <<<<
```

## Defining Chart functions

This is the glorious boring part, where I define the builders quite verbosely as to what the rows should be.

```hs
data_ :: forall input
   . RowLacks "data" input
  => Array Number
  -> ChartBuilder
       ( lineDataDataset :: Unit
       , barDataDataset :: Unit
       )
       input
       ("data" :: Array Number | input)
data_ x = ChartBuilder (Builder.insert (SProxy :: SProxy "data") x)

backgroundColor :: forall input
   . RowLacks "backgroundColor" input
  => Array String
  -> ChartBuilder
       ( barDataDataset :: Unit
       , pieChartDataset :: Unit
       )
       input
       ("backgroundColor" :: Array String | input)
backgroundColor xs = ChartBuilder
  (Builder.insert (SProxy :: SProxy "backgroundColor") xs)
```

So as above, the first row parameter to ChartBuilder defines which chart types this may apply to, and then the input is left as completely open to any usage, and the output being the input extended with the field that the specific builder adds.

Then I define my bar chart making function, where I run the builder to build the dataset I need and pass it off to FFI to do the rest.

```hs
foreign import makeBarChart_ :: forall barChartSpec e
   . Context2D -> barChartSpec -> Eff e ChartInstance

makeBarChart :: forall appliesTo output e
   . Context2D
  -> { labels :: Array String
     , datasetBuilder ::
         ChartBuilder
          (barDataDataset :: Unit | appliesTo)
          ("type" :: String)
          output
     }
  -> Eff e ChartInstance
makeBarChart ctx {labels, datasetBuilder: ChartBuilder builder} =
  makeBarChart_ ctx
    { "data": {
        labels,
        datasets:
          [ Builder.build builder {"type": "bar"}
          ]
      }
    }
```

So finally the appliesTo constraint is used here, where the RowCons constraint requires that the `(barDataDataset :: Unit)` be a field in the appliesTo parameter.

## Usage

The actual usage looks like what you'd expect, where I effectfully get the 2D context from a canvas element and feed it into makeBarChart, with labels defined as a normal array of strings and the builder defined by composing a bunch of ChartBuilders together:

```hs
main = do
  context <- traverse getContext2D =<< getCanvasElementById "myChart"
  case context of
    Nothing -> log "couldn't find chart context..."
    Just ctx ->  do
      chart <- makeBarChart ctx { labels, datasetBuilder }
      log "made bar chart"
  where
    labels =
      [ "Red"
      , "Blue"
      , "Yellow"
      , "Green"
      , "Purple"
      , "Orange"
      ]
    datasetBuilder
         = label "# of Votes"
      <<<< data_ [12.0, 19.0, 3.0, 5.0, 2.0, 3.0]
      <<<< backgroundColor ["rgba(255, 99, 132, 0.2)", "rgba(54, 162, 235, 0.2)", "rgba(255, 206, 86, 0.2)", "rgba(75, 192, 192, 0.2)", "rgba(153, 102, 255, 0.2)", "rgba(255, 159, 64, 0.2)"]
      <<<< borderWidth 1
```

And voila, we have a chart:

![](https://i.imgur.com/mFBejTd.png)

## Conclusion

Hopefully this has shown that there are all kinds of fun things we can do with row types and type level programming. Problems where we have traditionally applied typeclasses can instead be solved with sufficiently advanced row type fun (emphasis on *fun*).

Also, why define some weird DSL when you can do it with functions, right?

## Links

* Repo: https://github.com/justinwoo/purescript-dotorimuk (WIP, please contribute if you are interested!)
* Typelevel Prelude: https://pursuit.purescript.org/packages/purescript-typelevel-prelude

