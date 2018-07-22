Using types with parameters to eliminate invalid code paths in Typescript
Recently, I've been trying to get some Typescript users to take advantage of the fact that we have "generics" to constrain types and write code to only the valid paths that we have.

## The problem

In a codebase that needs to call some endpoints, the code for working with them typically falls into two patterns:

1) Write the "correct implementation" with almost no real typing of anything
2) Write the remote request code using a giant union type of all of the possible values, and then discriminate by a unique identifier or coerce the type

Let's go into why this doesn't work

### "Correct implementations"

This is the easiest to see flawed, as it completely throws away all type safety:

```ts
// request can take any request param and return any response
function request<Params, Response>(params: Params): Promise<Response> {
  return somePromise(params) as Promise<Response>; // the correct implementation would take a schema
}

function getVideos(Params: VideoParams): Promise<VideoResponse> {
  return request(Params);
}
```

Even though `request` is generic and it works, the problem is that we can easily make mistakes by providing the wrong type for the response type. Consider I accidentally wrote `BookResponse`. This would not be what is expected from sending a request using our `VideoParams`.

The other problem is that even though there might be parameters provided, there's no constraints here: since this function works for all `Params` and all `Responses`, there is no relationship defined between the two and this allows for mistakes like expecting books to come back from a videos request.

### Giant Useless Union

One way many users try to solve the request typing problem is to make huge records for the types that they want to work with:

```ts
type Params =
  | VideoParams
  | BookParams
  | ChickenParams
  | // ...

type Response =
  | VideoResponse
  | BookResponse
  | ChickenResponse
  | // ...

// "ah ha! this time they're constrained to members of the union!!!"
function request(params: Params): Promise<Response> {
  return somePromise(params) as Promise<Response>;
}

function getVideos(Params: VideoParams): Promise<VideoResponse> {
  return request(Params);
}
```

Again, this has the same problems as last time, and one even worse: the union types will keep growing, but may not even have a corresponding member from one union in the other. There is still no constraint on these types, so there is no way to even check that the two types are the same. **There is a possible M * N number of possible type combinations, of which there are only M number of actual valid pairs. That means I have this many invalid code paths that I must guard against even though they should never happen (and should be checked with a schema in a real implementation).**

## The solution

Let's start by defining an actual type:

```ts
type Resource<Request, Response> = {
  request: Request,
  response: Response
}
```

*Note that because of Typescript's structural typing, we need to make sure that we actually have all type parameters inside of the body. You might want to learn about "Phantom Types" if you're interested in approaches that don't necessitate this.*

With this definition, we can define a `Resource` where we have the pairing of `Request` and `Response`. For example:

```ts
type VideoResource = Resource<VideoParams, VideoResponse>
```

Now we can define `request` from earlier by actually reusing the types:

```ts
function request<R extends Resource<Request, Response>, Request, Response>(
  request: Request
): Response {
  return somePromise(params) as Response;
}
```

With this definition, we now actually have a request function that pairs the `Request` and `Response`. If you call this using a `VideoResource`, the params will be checked for being a `VideoParams`, and the response will always be `VideoResponse`.

As always though, Typescript's limitations come back to haunt us, as the usage site looks like this:

```ts
function getVideos(params: VideoParams): Promise<VideoResponse> {
  return request<VideoResource, VideoParams, VideoResponse>(params);
}
```

The problem is that if we provide [default type parameters](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html#generic-parameter-defaults), those defaults often completely overwrite the type parameters and make it impossible for us to get type safety, and type parameters can't be solved correctly as they can be in languages with better type systems.

To get around this, we can use a strange feature in Typescript to extract types from record fields after providing `any` into the parameters:

```ts
function request<R extends Resource<any, any>>(
  request: R['request'] // extracts the 'request' field
): Promise<R['response']> { // same for 'response'
  return somePromise(params) as Promise<R['response']>;
}
```

Now that request is defined in terms of the types inside of the `Resource`, we can call it with just one parameter:

```ts
function getVideos(params: VideoParams): Promise<VideoResponse> {
  return request<VideoResource>(params);
}
```

Now we have it in a more usable form without redundant arguments.

## Conclusion

Hopefully this has shown you how you can leverage type definitions to constrain types so that you only have to handle code paths that are correct.

Big thanks to Giulio Canti for showing me that such a thing as extracting types from fields was possible. It's pretty unholy.

## 後書き

I wrote this because someone asked me to put some writing down so that it could be referenced by others rather than the word-of-mouth tradition of ranting on Slack or the meaningless practice of shitposting on Twitter. I may do more of these kinds of posts in the future if asked, but I would much rather have everyone not write dialects of Javascript.

If you're interested in these kinds of approaches in general, you might quite enjoy using a host of different languages that allow you to express this as succinctly as

```hs
data Resource request response = Resource

request :: Resource request response -> request -> Async response
```

...with better inference, tooling, community and technical support, and much more, which can be done with Elm, Purescript, Haskell, Scala, or a host of other languages. If your language can do this with phantom types, I'd appreciate if you would write a blog post about it too.
