---
title: Unions for Partial Properties in PureScript
tags: purescript
author: kimagure
slide: false
---
For a while I've been wondering about how things like charting libraries should be used from PureScript, where often they are very dynamic beasts of many partial properties. Before PureScript 0.11, the most sensible way to handle this was to create a row-typed DSLs that would allow you to write properties at the correct levels, and these steps would be interpreted to build up your chart config.

Previously, I wrote [here](https://qiita.com/kimagure/items/fd05ad13ee8def0fb4ed) about how you could use PureScript-Record in 0.11.x to instead correctly type a record Builder, where the commands then carried in their row information what chart they would be used for. But this also is not so simple to use and does not let us define what properties a chart wants, but only what charts a property wants. Not so glorious still.

## Union-based Basic approaches

Recently, the folks at Lumi (with Phil et al) have released [React-Basic](https://github.com/lumihq/purescript-react-basic), which takes a nice approach to this problem: why not use a normal row type [Union](https://pursuit.purescript.org/builtins/docs/Prim#t:Union) to express a union of a user-provided concrete row type and its inferred complement? That is, considering the definition of Union:

```hs
class Union (l :: # Type) (r :: # Type) (u :: # Type)
  | l r -> u
  , r u -> l
  , u l -> r
```

From these functional dependencies we can see that for any given two concrete types, we can solve for the third. For example, given

```hs
  l ~ (apple :: Int)
  u ~ (apple :: Int, banana :: String)
  
  Union l r u
```

Then we know there is an instance that matches using the concrete definitions of `l` and `u`, and `r` will be solved for.

That's actually just about the whole thing. What remains are only the implementations.

## Gomtang-Basic

I used this same idea to implement a wrapper for Baidu's [ECharts](http://echarts.baidu.com/) in [Gomtang-Basic](https://github.com/justinwoo/purescript-gomtang-basic). The name comes from that I like [Gomtang](https://en.wikipedia.org/wiki/Gomguk) and the Union-based approach I dubbed "-Basic".

And so, there is some typical boilerplate setup like usual:

```hs
foreign import data Instance :: Type

foreign import makeChart_ :: forall e. Element -> Eff e Instance
foreign import setOption_ :: forall option e
   . option -> Instance -> Eff e Unit 

makeChart :: forall e. Element -> Eff e Instance
makeChart = makeChart_
```

And then the interesting bit:

```hs
setOption :: forall e option option'
   . Union option option' Option
  => Record option -> Instance -> Eff e Unit
setOption = setOption_

type Option =
  ( title :: TitleOption
  , tooltip :: TooltipOption
  , xAxis :: XAxisOption
  , yAxis :: YAxisOption
  , visualMap :: VisualMapOption
  , calendar :: CalendarOption
  , series :: Array SeriesOption
  )

data TitleOption
data TooltipOption
data XAxisOption
data YAxisOption
data VisualMapOption
data CalendarOption
data SeriesOption
```

And so with the constraint `Union option option' Option`, I define that a user-provided `option` row type (from the record input) should contain a subset of the fields defined in the `Option` row type. The limitation here though, is that since Option needs to be concretely typed, I can't have everything be parameterized, as any missing property will not be able to be solved with `Union`. But no worries, since we can define those as needed:

```hs
type VisualMap =
  ( min :: Number
  , max :: Number
  , calculable :: Boolean
  , orient :: String
  , left :: String
  , bottom :: String
  )

type BarSeries =
  ( name :: String
  , data :: Array Number
  )
```

Then we can make helper functions that check the `Union` of properties passed to these, and then those can be converted into the opaque types.

```hs
makeVisualMap
  :: forall fields fields'
   . Union fields fields' VisualMap
  => Record fields
  -> VisualMapOption
makeVisualMap = unsafeCoerce

makeBarSeries
  :: forall fields fields' trash
   . Union fields fields' BarSeries
  => RowLacks "type" fields
  => RowCons "type" String fields trash
  => Record fields
  -> SeriesOption
makeBarSeries r = unsafeCoerce $ insert (SProxy :: SProxy "type") "bar" r
```

And you might say, "wait, `unsafeCoerce`???" But remember the definition of the data type:

```hs
data VisualMapOption
data SeriesOption
```

There is no way to inspect or match on these once they have been created by coercion, and there is no way to create values of these types otherwise. So the coercion is safe, in that you can no longer touch this from the PureScript side. Of course, others could coerce "wrong" values into this type, but that's also its advantage -- nobody needs to wait around for me to customize their chart as needed, and they can add plenty of their own constraints as needed.

*I feel this last paragraph needs to be stressed more, since there are often detractors who try to claim weird things like "PureScript is unsafe" or "PureScript is not typesafe". Instead of making facile remarks or believing them, it's important to dig into why something might be done. In this case, unsafeCoerce is used for coercing these values, but we have applied additional constraints to the inputs and do this to make our lives easier and to use the type system to do things it does well. You can still complain if you want, but please, let's not tell lies. Not that you're very accountable though, it's only a point of honesty and following the [Golden Rule](https://en.wikipedia.org/wiki/Golden_Rule).*

## Usage

The usage looks fairly similar to how we work with this in JS:

```hs
main = do
  renderChart mainOption (ElementId "main")
  renderChart heatMapOption (ElementId "heatmap")
  where
    mainOption =
      { title: makeTitle { text: "Bar Example" }
      , xAxis: makeXAxis
          { data:
              [ "shirt"
              , "cardigan"
              , "chiffon"
              , "pants"
              , "heels"
              , "socks"
              ]
          }
      , yAxis: makeYAxis {}
      , series: pure $ makeBarSeries
        { name: "sales"
        , data: [5.0, 20.0, 36.0, 10.0, 10.0, 20.0]
        }
      }
    heatMapOption =
      { tooltip: makeTooltip { position: "top" }
      , visualMap: makeVisualMap
          { min: 0.0
          , max: 10.0
          , calculable: true
          }
      , calendar: makeCalendar
          { range: ["2017-10-20", "2018-2-17"]
          , cellSize: ["auto", "auto"]
          }
      , series: pure $ makeHeatMapSeries $
          { coordinateSystem: "calendar"
          , calendarIndex: 0
          , data:
              [ ["2017-10-28", "3"]
              , ["2017-11-18", "8"]
              , ["2017-11-23", "4"]
              , ["2017-12-18", "6"]
              , ["2017-01-01", "1"]
              , ["2018-01-15", "9"]
              , ["2018-02-12", "2"]
              ]
        }
      }
```

And the outputs of this simple bar chart and heatmap look like this:

![](https://i.imgur.com/1Iess2J.png)

And not too much effort involved at all!

## Conclusion

So hopefully this has showed you that using the `Union` constraint can make things a lot easier for you to work with. In purely PureScript code, you might find some interesting uses of combining this with `RowToList` to handle only properties that are defined.

## Links

* Gomtang-Basic: https://github.com/justinwoo/purescript-gomtang-basic
* React-Basic: https://github.com/lumihq/purescript-react-basic

