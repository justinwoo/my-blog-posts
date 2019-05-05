#!/usr/bin/env runghc

module Main where

import Data.List

newtype Date = Date String
newtype Title = Title String
newtype Path = Path String -- relative to ./posts/

data Entry = Entry Date Title Path

prepareEntry :: String -> Entry
prepareEntry
  path@
  ( y1 : y2 : y3 : y4 : '-'
  : m1 : m2 : '-'
  : d1 : d2 : '-'
  : rest
  ) = Entry date title (Path path)
  where
    date = Date $ y1:y2:y3:y4:" " ++ month ++ ' ':d1:d2:""
    month = monthLabel m1 m2
    title = Title $ (\x -> if x == '-' then ' ' else x) <$> rest
prepareEntry xs = error $ "could not process " ++ xs

monthLabel :: Char -> Char -> String
monthLabel '0' '1' = "Jan"
monthLabel '0' '2' = "Feb"
monthLabel '0' '3' = "Mar"
monthLabel '0' '4' = "Apr"
monthLabel '0' '5' = "May"
monthLabel '0' '6' = "Jun"
monthLabel '0' '7' = "Jul"
monthLabel '0' '8' = "Aug"
monthLabel '0' '9' = "Sep"
monthLabel '1' '0' = "Oct"
monthLabel '1' '1' = "Nov"
monthLabel '1' '2' = "Dec"
monthLabel x y = fail $ "not a date: " ++ x:y:""

showEntry :: Entry -> String
showEntry (Entry (Date date) (Title title) (Path path)) = intercalate "\n"
  [ "#### " <> date
  , ""
  , "##### " <> title
  , ""
  , "[file](./posts/" <> path <> ")"
  ]

main :: IO ()
main = do
  let blocks = showEntry <$> prepareEntry <$> inputs
  putStrLn $ intercalate "\n\n" blocks

inputs :: [String]
inputs =
  [

    "2019-04-29-why-easy-purescript-nix.md"
  , "2019-04-29-short-simplified-tortellini-with-instance-chains.md"
  , "2019-04-06-using-tree-sitter-from-purescript.md"
  , "2019-03-16-short-overriding-version-bounds-in-nix-haskell.md"
  , "2019-03-16-building-purescript-projects-with-nix.md"
  , "2019-03-14-rust-with-runtime-deps-made-easy-with-nix.md"
  , "2019-03-02-generic-sums-to-variant-and-back-again.md"
  , "2019-02-11-making-a-simple-haskell-cli-without-anything-fancy.md"
  , "2019-02-02-purescript-package-management-explained.md"
  , "2019-01-13-consuming-electron-apps-in-nix-by-patching-binaries.md"
  , "2019-01-12-putting-your-own-derivations-in-nix-profile.md"
  , "2018-11-10-upgrade-from-bower-to-nix-with-psc-package2nix.md"
  , "2018-11-03-nix-ify-your-psc-package-dependencies.md"
  , "2018-10-29-make-your-own-psc-package-with-perl.md"
  , "2018-10-27-pseudo-dynamically-typed-errors-in-purescript.md"
  , "2018-10-24-using-purescript-easily-with-nix.md"
  , "2018-10-18-easy-markdown-to-beamer-with-pandoc-and-nix.md"
  , "2018-09-15-reflecting-a-record-of-proxies-and-keys-of-row-types.md"
  , "2018-09-11-we-dont-need-peano-numbers-in-purescript.md"
  , "2018-08-26-what-ive-learned-since-quitting-elm.md"
  , "2018-08-13-expecting-inferred-types-feat-custom-type-errors.md"
  , "2018-08-06-converting-generics-rep-to-row-types-in-purescript.md"
  , "2018-07-29-user-empowerment-of-ffi-in-purescript.md"
  , "2018-07-23-formatting-type-level-strings-with-row-type-labels.md"
  , "2018-07-02-implement-your-own-compiler-type-class-in-purescript.md"
  , "2018-06-22-managing-psc-package-sets-with-dhall.md"
  , "2018-06-17-a-year-of-twitter-memes.md"
  , "2018-06-04-well-typed-parameterized-sqlite-parameters-with-purescript.md"
  , "2018-05-31-99+-posts-on-qiita-as-an-english-only-poster:-reflections.md"
  , "2018-05-27-fun-type-level-literal-number-arithmetic-with-instance-chains.md"
  , "2018-05-16-simple-routing-based-on-parsing-type-level-strings.md"
  , "2018-05-11-parsing-type-level-strings-to-extract-types.md"
  , "2018-05-03-well-typed-path-params-in-purescript-0.12.md"
  , "2018-05-02-instance-chains-to-get-nested-record-label-paths.md"
  , "2018-04-27-type-classes-and-instances-are-pattern-matching-for-types.md"
  , "2018-04-21-generics-rep-sums-and-products-to-list-for-fun.md"
  , "2018-04-17-matching-on-js-union-members-with-row-types-handling-js-unions-cont.md"
  , "2018-04-15-easy-hot-reloading-purescript-with-parcel.md"
  , "2018-04-02-handling-js-unions-with-row-types.md"
  , "2018-04-01-making-a-new-library-and-using-it-in-your-own-psc-package-set.md"
  , "2018-03-25-row-typed-fun-for-building-sqlite-queries.md"
  , "2018-03-20-setting-up-purescript-in-march-2018.md"
  , "2018-03-11-one-line-js-ffi-with-purescript-ffi-props.md"
  , "2018-03-03-make-your-own-form-library-in-purescript.md"
  , "2018-02-18-unions-for-partial-properties-in-purescript.md"
  , "2018-02-04-short-decoding-product-types-using-generics-rep.md"
  , "2018-02-03-generic-decoding-of-sum-types-feat-fields-to-row-and-vice-versa.md"
  , "2018-02-02-making-diffs-of-differently-typed-records-in-purescript.md"
  , "2018-01-30-using-rows-and-rowtolist-to-model-chart.js-spec-building.md"
  , "2018-01-27-short-composed-modified-json-parsing-for-free-with-simple-json.md"
  , "2018-01-14-short-type-level-path-params-to-write-from-records-with-purescript.md"
  , "2018-01-13-type-level-path-params-parsed-to-records-with-purescript.md"
  , "2018-01-08-modified-json-parsing-for-free-with-purescript-simple-json.md"
  , "2018-01-06-opting-in-to-better-types-and-guarantees-in-purescript.md"
  , "2017-12-28-the-tale-of-two-tortellini:-making-record-based-libraries-in-purescript-and-haskell.md"
  , "2017-12-23-polymorphic-proxy-fun-in-purescript.md"
  , "2017-12-17-records-interfaces-are-not-stringmaps-hashes-objects-and-vice-versa.md"
  , "2017-12-16-generating-elm-types-for-port-safe-communication-from-purescript.md"
  , "2017-12-13-easy-purescript-bundling-with-parcel.md"
  , "2017-12-11-multi-target-projects-in-purescript.md"
  , "2017-12-09-controlled-flow-with-purescript-bismuth.md"
  , "2017-12-05-more-rowlist-fun-with-records-in-haskell-feat-scotty-route-handler-pairings-and-homerunwannabe.md"
  , "2017-12-04-nice-validation-with-purescript.md"
  , "2017-11-28-fun-with-records-in-haskell-by-making-rowlists.md"
  , "2017-11-21-record-based-api-route-handler-pairing-with-row-types.md"
  , "2017-11-17-ohyes-you-can-interop-with-typescript-using-purescript.md"
  , "2017-10-28-upgrade-from-bower-to-psc-package.md"
  , "2017-10-22-mapping-a-function-to-a-homogeneous-record-in-purescript.md"
  , "2017-10-20-and-some-you-shouldnt-purescript-libraries-ive-written-that-you-might-consider-using.md"
  , "2017-09-08-fun-row-typed-validation-with-purescript-home-run-ball.md"
  , "2017-08-26-converting-types-you-dont-want-to-ones-you-do-in-purescript.md"
  , "2017-08-17-using-types-with-parameters-to-eliminate-invalid-code-paths-in-typescript.md"
  , "2017-07-28-using-ixmonad-to-enforce-good-hamburger-building-in-purescript.md"
  , "2017-07-21-type-safe-record-operations-with-purescript-record.md"
  , "2017-07-15-writing-a-json-decoder-using-purescripts-rowtolist.md"
  , "2017-07-11-embedding-elm-into-a-purescript-halogen-app.md"
  , "2017-07-08-automatically-de-encoding-json-in-purescript-using-generics-rep.md"
  , "2017-06-29-datatype-generic-programming-for-generating-typescript-code-from-purescript.md"
  , "2017-06-25-differences-of-purescript-from-elm.md"
  , "2017-04-29-using-purescripts-row-types-for-tracking-validations.md"
  , "2017-04-27-writing-a-youtube-audio-scraping-tool-with-purescript.md"
  , "2017-03-27-writing-a-full-stack-app-with-purescript-with-phantom-types.md"
  , "2017-03-05-building-a-mobile-web-audio-player-with-purescript-halogen.md"
  , "2016-12-14-how-to-write-a-simple-markdown-to-inline-styled-html-tool-with-haskell.md"
  , "2016-10-20-what-to-check-when-react-native-npm-start-and-jest-dont-work.md"
  , "2016-10-13-getting-audio-from-youtube-channels-in-the-easiest-way-possible-using-haskell.md"
  , "2016-10-04-how-to-convert-markdown-into-inline-styled-html.md"
  , "2016-09-25-writing-a-simple-telegram-chat-bot-in-purescript.md"
  , "2016-09-21-async-in-purescript-is-fun-and-easy.md"
  , "2016-09-18-purescript-on-travis-ci-as-of-sept-2016.md"
  , "2016-07-31-making-a-simple-node-program-with-purescript.md"
  , "2016-07-10-making-a-haskell-scotty-web-app-and-putting-it-on-heroku.md"
  , "2016-03-15-making-a-scroll-table-with-purescript-halogen.md"
  , "2016-02-18-writing-a-simple-etch-sketch-with-purescript.md"

  ]
