# Spanish

Una de las cosas más convenientes al utilizar PureScript es su Interfaz de Funciones Foráneas (FFI), ya que resulta bastante permisiva sobre la forma de trabajar con ella. Sin embargo, muchas veces las personas no aprenden suficiente acerca del funcionamiento de la FFI, usualmente porque piensan que va a resultar muy complicado o demandar demasiado esfuerzo, o porque lo consideran "inferior" a lo que ellos hacen. Bueno, a este ultimo grupo no se como podemos ayudarlo, pero al primero definitivamente podemos dotarlo de poder con algunas referencias y herramientas.

## "Dándole poder al usuario de FFI en PureScript"

En este post hablé sobre las distintas técnicas comunes de FFI en PureScript, tales como funciones con efectos sin currificar, tipos de datos opacos, conversión de Promesas a Aff, y más:

<https://qiita.com/kimagure/items/0ce4d9d2792dd110ee45>

Es importante notar que si querés validar alguno de los valores retornados desde JS, podés directamente usar Simple-JSON sobre los valores de tipo `Foreign`.

Si leer un post entero resulta demasiado, pienso que un muy buen uso de tu tiempo es al menos leer la documentación de Effect.Uncurried:

<https://pursuit.purescript.org/packages/purescript-effect/2.0.0/docs/Effect.Uncurried>

---

# English

One of the most convenient things about using PureScript is the FFI, which is quite lenient in letting you choose how you want to work with it. However, many times people don't learn enough about how the FFI works, usually because they think that it will be too hard/too much of a hassle, or see it as "below" them. Well, I don't know how we can help people in the latter camp, but the former camp can definitely be empowered with some references and examples.

## "User empowerment of FFI in PureScript"

In this post, I talked about the various common FFI techniques in PureScript, such as uncurried effect functions, opaque data types, Promises to Aff, and more:

<https://qiita.com/kimagure/items/0ce4d9d2792dd110ee45>

It's important to note that if you do want to validate some returned value from JS, you can readily put Simple-JSON to work on `Foreign`-typed values.

If reading a whole post seems like too much, I think your time is most well spent at least reading the Effect.Uncurried docs:

<https://pursuit.purescript.org/packages/purescript-effect/2.0.0/docs/Effect.Uncurried>

---

# 日本語

FFIはPureScriptにおいて最も強力な機能の一つであり、様々なアプローチで活用することができます。しかしながら、多くの人はFFIがどう機能しているか知りません。なぜなら、一般的にFFIは「学習コストが高そう」と思われていたり、なぜだか過小評価されていたりするためです。後者のケースについてはどうにもできませんが、前者については間違いなく、この記事で紹介するリンクと実例が参考になるでしょう。

## "User empowerment of FFI in PureScript"

次の記事では、PureScriptにおけるFFIの一般的なテクニックについて書きました。 **uncurried effect functions** , **opaque data types** , **Promises to Aff** などです。

https://qiita.com/kimagure/items/0ce4d9d2792dd110ee45

また、JSから返された値をバリデートしたい場合、[Simple-JSON](https://github.com/justinwoo/purescript-simple-json)を使って、その値を`Foreign`-typed な値として扱うことができます。

もし紹介した記事を最後まで読むのが大変なら、最低限 `Effect.Uncurried` の公式ドキュメントを読めば良いでしょう。

https://pursuit.purescript.org/packages/purescript-effect/2.0.0/docs/Effect.Uncurried

---

Thanks to [mvaldesdeleon](https://twitter.com/mvaldesdeleon) for the Spanish and @e_ntyo for the Japanese translations!
