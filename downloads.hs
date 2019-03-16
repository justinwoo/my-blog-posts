#!/usr/bin/env runghc

module Main where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as DTIO
import qualified System.Process as Proc

type Entry = (String, String)

download :: Entry -> IO ()
download (name, url) = do
  let cmd = "test -e " <> name <> " || wget " <> url <> " -O " <> name
  process <- Proc.spawnCommand cmd
  exitCode <- Proc.waitForProcess process
  putStrLn $ name <> ": " <> show exitCode

-- clean up entries by stripping header
cleanup :: Entry -> IO ()
cleanup (name, url) = do
  contents <- DTIO.readFile name
  let result = Text.strip . Text.unlines . stripHeader $ Text.lines contents
  DTIO.writeFile name result
    where
      isInfixOf s t = Text.pack s `Text.isInfixOf` t
      stripHeader xs = case xs of
        d1 : title : tags : author : slide : d2 : rest
          | d1 == Text.pack "---"
          , d2 == Text.pack "---"
          , "title:" `isInfixOf` title
          , "tags:" `isInfixOf` tags
          , "author:" `isInfixOf` author
          , "slide:" `isInfixOf` slide -> rest
        _ -> xs

main :: IO ()
main = do
  _ <- traverse download posts
  _ <- traverse cleanup posts
  putStrLn $ "Finished " <> show (length posts)

posts :: [Entry]
posts =
  ( ( "posts/2016-02-18-writing-a-simple-etch-sketch-with-purescript.md", "https://qiita.com/kimagure/items/15361b4bc5f4dfac20e8.md" )
  , ( "posts/2016-03-15-making-a-scroll-table-with-purescript-halogen.md", "https://qiita.com/kimagure/items/92f0a278971c7deb8eb5.md" )
  , ( "posts/2016-07-10-making-a-haskell-scotty-web-app-and-putting-it-on-heroku.md", "https://qiita.com/kimagure/items/5947e2db40b9ec2226bf.md" )
  , ( "posts/2016-07-31-making-a-simple-node-program-with-purescript.md", "https://qiita.com/kimagure/items/5674e3ae9c87262af762.md" )
  , ( "posts/2016-09-18-purescript-on-travis-ci-as-of-sept-2016.md", "https://qiita.com/kimagure/items/b1a892870de4073908af.md" )
  , ( "posts/2016-09-21-async-in-purescript-is-fun-and-easy.md", "https://qiita.com/kimagure/items/2ebce1399bac00c79656.md" )
  , ( "posts/2016-09-25-writing-a-simple-telegram-chat-bot-in-purescript.md", "https://qiita.com/kimagure/items/2da0fe86b218b3f832d0.md" )
  , ( "posts/2016-10-04-how-to-convert-markdown-into-inline-styled-html.md", "https://qiita.com/kimagure/items/6820e2df2a7604047862.md" )
  , ( "posts/2016-10-13-getting-audio-from-youtube-channels-in-the-easiest-way-possible-using-haskell.md", "https://qiita.com/kimagure/items/0a2f3d60789c646e4426.md" )
  , ( "posts/2016-10-20-what-to-check-when-react-native-npm-start-and-jest-dont-work.md", "https://qiita.com/kimagure/items/a340fde5000dfd0102de.md" )
  , ( "posts/2016-12-14-how-to-write-a-simple-markdown-to-inline-styled-html-tool-with-haskell.md", "https://qiita.com/kimagure/items/97e8d7b9cb318ba7ebef.md" )
  , ( "posts/2017-03-05-building-a-mobile-web-audio-player-with-purescript-halogen.md", "https://qiita.com/kimagure/items/653c52e77d7cd3567498.md" )
  , ( "posts/2017-03-27-writing-a-full-stack-app-with-purescript-with-phantom-types.md", "https://qiita.com/kimagure/items/b576b5bfe370180599f8.md" )
  , ( "posts/2017-04-27-writing-a-youtube-audio-scraping-tool-with-purescript.md", "https://qiita.com/kimagure/items/115c04bc64d09a3a07a1.md" )
  , ( "posts/2017-04-29-using-purescripts-row-types-for-tracking-validations.md", "https://qiita.com/kimagure/items/5c248844ab28c8c91b16.md" )
  , ( "posts/2017-06-25-differences-of-purescript-from-elm.md", "https://qiita.com/kimagure/items/f1827c9129f3ee6ede35.md" )
  , ( "posts/2017-06-29-datatype-generic-programming-for-generating-typescript-code-from-purescript.md", "https://qiita.com/kimagure/items/cc0ea2982abdf1625e87.md" )
  , ( "posts/2017-07-08-automatically-de-encoding-json-in-purescript-using-generics-rep.md", "https://qiita.com/kimagure/items/00f97c7fc6cef178fa3c.md" )
  , ( "posts/2017-07-11-embedding-elm-into-a-purescript-halogen-app.md", "https://qiita.com/kimagure/items/d12525d42516f95dd541.md" )
  , ( "posts/2017-07-15-writing-a-json-decoder-using-purescripts-rowtolist.md", "https://qiita.com/kimagure/items/d8a0681ae05b605c5abe.md" )
  , ( "posts/2017-07-21-type-safe-record-operations-with-purescript-record.md", "https://qiita.com/kimagure/items/7d777826acf371293a93.md" )
  , ( "posts/2017-07-28-using-ixmonad-to-enforce-good-hamburger-building-in-purescript.md", "https://qiita.com/kimagure/items/a0ee7313e8c7690bf3f5.md" )
  , ( "posts/2017-08-17-using-types-with-parameters-to-eliminate-invalid-code-paths-in-typescript.md", "https://qiita.com/kimagure/items/ed612f25c3d3bcfaecd7.md" )
  , ( "posts/2017-08-26-converting-types-you-dont-want-to-ones-you-do-in-purescript.md", "https://qiita.com/kimagure/items/f750d85377520a14066f.md" )
  , ( "posts/2017-09-08-fun-row-typed-validation-with-purescript-home-run-ball.md", "https://qiita.com/kimagure/items/eeb40541fc56b8dba2cc.md" )
  , ( "posts/2017-10-20-and-some-you-shouldnt-purescript-libraries-ive-written-that-you-might-consider-using.md", "https://qiita.com/kimagure/items/daa388ffe14747d13f57.md" )
  , ( "posts/2017-10-22-mapping-a-function-to-a-homogeneous-record-in-purescript.md", "https://qiita.com/kimagure/items/06d7eed9521b6217b771.md" )
  , ( "posts/2017-10-28-upgrade-from-bower-to-psc-package.md", "https://qiita.com/kimagure/items/0d9354900d7a7dbd3864.md" )
  , ( "posts/2017-11-17-ohyes-you-can-interop-with-typescript-using-purescript.md", "https://qiita.com/kimagure/items/4847685d02d4b15a556c.md" )
  , ( "posts/2017-11-21-record-based-api-route-handler-pairing-with-row-types.md", "https://qiita.com/kimagure/items/bb9bd3e4ffe1bba4c214.md" )
  , ( "posts/2017-11-28-fun-with-records-in-haskell-by-making-rowlists.md", "https://qiita.com/kimagure/items/6a9764966edd6cef497d.md" )
  , ( "posts/2017-12-04-nice-validation-with-purescript.md", "https://qiita.com/kimagure/items/f75befebdd37f6e8879f.md" )
  , ( "posts/2017-12-05-more-rowlist-fun-with-records-in-haskell-feat-scotty-route-handler-pairings-and-homerunwannabe.md", "https://qiita.com/kimagure/items/7c3521cfbf00ad173801.md" )
  , ( "posts/2017-12-09-controlled-flow-with-purescript-bismuth.md", "https://qiita.com/kimagure/items/0c2712d5a417c1671e6d.md" )
  , ( "posts/2017-12-11-multi-target-projects-in-purescript.md", "https://qiita.com/kimagure/items/8ca4f386dbcb9f404b87.md" )
  , ( "posts/2017-12-13-easy-purescript-bundling-with-parcel.md", "https://qiita.com/kimagure/items/24e6d3a0f47814c9630b.md" )
  , ( "posts/2017-12-16-generating-elm-types-for-port-safe-communication-from-purescript.md", "https://qiita.com/kimagure/items/09b24ed22cfc596248b4.md" )
  , ( "posts/2017-12-17-records-interfaces-are-not-stringmaps-hashes-objects-and-vice-versa.md", "https://qiita.com/kimagure/items/a011335bbb539e179f4e.md" )
  , ( "posts/2017-12-23-polymorphic-proxy-fun-in-purescript.md", "https://qiita.com/kimagure/items/6e383ea0c6e29bf210e5.md" )
  , ( "posts/2017-12-28-the-tale-of-two-tortellini:-making-record-based-libraries-in-purescript-and-haskell.md", "https://qiita.com/kimagure/items/941c22effff608dda9a7.md" )
  , ( "posts/2018-01-06-opting-in-to-better-types-and-guarantees-in-purescript.md", "https://qiita.com/kimagure/items/7c3a01e2e5dfebb3313f.md" )
  , ( "posts/2018-01-08-modified-json-parsing-for-free-with-purescript-simple-json.md", "https://qiita.com/kimagure/items/801e1c55d4f8f218f11e.md" )
  , ( "posts/2018-01-13-type-level-path-params-parsed-to-records-with-purescript.md", "https://qiita.com/kimagure/items/4f5c6054870f631ff768.md" )
  , ( "posts/2018-01-14-short-type-level-path-params-to-write-from-records-with-purescript.md", "https://qiita.com/kimagure/items/777133d6bbff67e3819d.md" )
  , ( "posts/2018-01-27-short-composed-modified-json-parsing-for-free-with-simple-json.md", "https://qiita.com/kimagure/items/43fd7b02db2950f04a1a.md" )
  , ( "posts/2018-01-30-using-rows-and-rowtolist-to-model-chart.js-spec-building.md", "https://qiita.com/kimagure/items/fd05ad13ee8def0fb4ed.md" )
  , ( "posts/2018-02-02-making-diffs-of-differently-typed-records-in-purescript.md", "https://qiita.com/kimagure/items/ca229cb4ba76db0c24a8.md" )
  , ( "posts/2018-02-03-generic-decoding-of-sum-types-feat-fields-to-row-and-vice-versa.md", "https://qiita.com/kimagure/items/b27245a5a11462145bd5.md" )
  , ( "posts/2018-02-04-short-decoding-product-types-using-generics-rep.md", "https://qiita.com/kimagure/items/18046a721881ac9270ac.md" )
  , ( "posts/2018-02-18-unions-for-partial-properties-in-purescript.md", "https://qiita.com/kimagure/items/581c63707673db61e061.md" )
  , ( "posts/2018-03-03-make-your-own-form-library-in-purescript.md", "https://qiita.com/kimagure/items/b35ad4a68939337275aa.md" )
  , ( "posts/2018-03-11-one-line-js-ffi-with-purescript-ffi-props.md", "https://qiita.com/kimagure/items/b0b7da07d8183cb51d58.md" )
  , ( "posts/2018-03-20-setting-up-purescript-in-march-2018.md", "https://qiita.com/kimagure/items/570e6f2bbce5b4724564.md" )
  , ( "posts/2018-03-25-row-typed-fun-for-building-sqlite-queries.md", "https://qiita.com/kimagure/items/7b86c1a16adb2045b584.md" )
  , ( "posts/2018-04-01-making-a-new-library-and-using-it-in-your-own-psc-package-set.md", "https://qiita.com/kimagure/items/c37b228e80318d4158f0.md" )
  , ( "posts/2018-04-02-handling-js-unions-with-row-types.md", "https://qiita.com/kimagure/items/141423771ad1f5a84425.md" )
  , ( "posts/2018-04-15-easy-hot-reloading-purescript-with-parcel.md", "https://qiita.com/kimagure/items/a870d250f75a6822759b.md" )
  , ( "posts/2018-04-17-matching-on-js-union-members-with-row-types-handling-js-unions-cont.md" , "https://qiita.com/kimagure/items/7a0d1675522c09b4bcb6.md" )
  , ( "posts/2018-04-21-generics-rep-sums-and-products-to-list-for-fun.md", "https://qiita.com/kimagure/items/a5e340242f038b0dc748.md" )
  , ( "posts/2018-04-27-type-classes-and-instances-are-pattern-matching-for-types.md", "https://qiita.com/kimagure/items/08c59fa21adcd6968ae1.md" )
  , ( "posts/2018-05-02-instance-chains-to-get-nested-record-label-paths.md", "https://qiita.com/kimagure/items/7e313ee68280186d76dc.md" )
  , ( "posts/2018-05-03-well-typed-path-params-in-purescript-0.12.md", "https://qiita.com/kimagure/items/3273d20c4c5ad74dbe26.md" )
  , ( "posts/2018-05-11-parsing-type-level-strings-to-extract-types.md", "https://qiita.com/kimagure/items/6729a5d55ab99bcee8ec.md" )
  , ( "posts/2018-05-16-simple-routing-based-on-parsing-type-level-strings.md", "https://qiita.com/kimagure/items/5c3f3fcb898e480c56be.md" )
  , ( "posts/2018-05-27-fun-type-level-literal-number-arithmetic-with-instance-chains.md", "https://qiita.com/kimagure/items/b19cdbc1807109fb11cb.md" )
  , ( "posts/2018-05-31-99+-posts-on-qiita-as-an-english-only-poster:-reflections.md", "https://qiita.com/kimagure/items/1a569987fee84ae26d4f.md" )
  , ( "posts/2018-06-04-well-typed-parameterized-sqlite-parameters-with-purescript.md", "https://qiita.com/kimagure/items/4b08e9f0479d5866ec04.md" )
  , ( "posts/2018-06-17-a-year-of-twitter-memes.md", "https://qiita.com/kimagure/items/4f0bb365965d31e6cd58.md" )
  , ( "posts/2018-06-22-managing-psc-package-sets-with-dhall.md", "https://qiita.com/kimagure/items/c419ba740ac134a837a2.md" )
  , ( "posts/2018-07-02-implement-your-own-compiler-type-class-in-purescript.md", "https://qiita.com/kimagure/items/8736fe6a2f25da526368.md" )
  , ( "posts/2018-07-23-formatting-type-level-strings-with-row-type-labels.md", "https://qiita.com/kimagure/items/c4bc704df3791437c9bb.md" )
  , ( "posts/2018-07-29-user-empowerment-of-ffi-in-purescript.md", "https://qiita.com/kimagure/items/0ce4d9d2792dd110ee45.md" )
  , ( "posts/2018-08-06-converting-generics-rep-to-row-types-in-purescript.md", "https://qiita.com/kimagure/items/1ea18bd6b782d45a48d5.md" )
  , ( "posts/2018-08-13-expecting-inferred-types-feat-custom-type-errors.md", "https://qiita.com/kimagure/items/00c1ca57d6999904b595.md" )
  , ( "posts/2018-08-26-what-ive-learned-since-quitting-elm.md", "https://qiita.com/kimagure/items/93a42d67a8833f99fe2e.md" )
  , ( "posts/2018-09-11-we-dont-need-peano-numbers-in-purescript.md", "https://qiita.com/kimagure/items/522fa4dd4abdcc313c8e.md" )
  , ( "posts/2018-09-15-reflecting-a-record-of-proxies-and-keys-of-row-types.md", "https://qiita.com/kimagure/items/b08175d22f9950ba3dfb.md" )
  , ( "posts/2018-10-18-easy-markdown-to-beamer-with-pandoc-and-nix.md", "https://qiita.com/kimagure/items/9d27015e12d4f22b53db.md" )
  , ( "posts/2018-10-24-using-purescript-easily-with-nix.md", "https://qiita.com/kimagure/items/de2a4ff45dd8fe8be4b1.md" )
  , ( "posts/2018-10-27-pseudo-dynamically-typed-errors-in-purescript.md", "https://qiita.com/kimagure/items/71e938ee93e31bd2e79b.md" )
  , ( "posts/2018-10-29-make-your-own-psc-package-with-perl.md", "https://qiita.com/kimagure/items/625070775da70b37b67e.md" )
  , ( "posts/2018-11-03-nix-ify-your-psc-package-dependencies.md", "https://qiita.com/kimagure/items/85a64437f9af78398638.md" )
  , ( "posts/2018-11-10-upgrade-from-bower-to-nix-with-psc-package2nix.md", "https://qiita.com/kimagure/items/aec640d0047d08d2ce90.md" )
  , ( "posts/2019-01-12-putting-your-own-derivations-in-nix-profile.md", "https://qiita.com/kimagure/items/8b4df59236717e54a2bc.md" )
  , ( "posts/2019-01-13-consuming-electron-apps-in-nix-by-patching-binaries.md", "https://qiita.com/kimagure/items/6e0fd0aad389c1f274c6.md" )
  , ( "posts/2019-02-02-purescript-package-management-explained.md", "https://qiita.com/kimagure/items/6a5881b07bc4a3e6ea86.md" )
  , ( "posts/2019-02-11-making-a-simple-haskell-cli-without-anything-fancy.md", "https://qiita.com/kimagure/items/9b33a5fa48baf105abe4" )
  , ( "posts/2019-03-02-generic-sums-to-variant-and-back-again.md", "https://qiita.com/kimagure/items/c4791d309b5cda607ce2.md" )
  , ( "posts/2019-03-14-rust-with-runtime-deps-made-easy-with-nix.md", "https://qiita.com/kimagure/items/4e4e65dde9471a17b304.md" )
  , ( "posts/2019-03-16-building-purescript-projects-with-nix.md", "https://qiita.com/kimagure/items/d2687a5a68c84cc651d0.md" )
  ]
