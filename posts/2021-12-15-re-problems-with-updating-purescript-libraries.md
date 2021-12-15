# Re: Problems with updating PureScript libraries

My [last post](https://github.com/justinwoo/my-blog-posts/blob/master/posts/2021-12-14-problems-with-updating-purescript-libraries.md) resonated both positively and negatively with readers, with both private and public responses. Even though I think I made it clear what the kind of biased perspective and tone the post would have, some have responded in confrontational ways.

In this post, I will respond to some of those kinds of public comments, but without direct attribution, because this is not about specific people, but more about the actual intentions of such messages.

## "If there are bugs with the compiler's build system, did you report them?"

I'm very confused by this message from this particular person, because I had discussed this problem with them multiple times both in various community spaces and in direct messages. Once they even told me directly that they were looking into this issue, and I appreciated that, but regardless, the way the build system works now does not work under a scenario I find not too unlikely: if modules that are transitive dependencies of other modules that have been modified and their immediate dependencies not edited (e.g. large git branch checkouts), the PureScript build plan will erroneously not rebuild these modules, resulting in incorrect builds that manifest typically in type errors, where the type was changed and the consumers changed accordingly, but the source module was not rebuilt, so stale incorrect types do not unify.

This seems to be caused by the compiler's build plan incorrectly making assumptions of validity of output artifacts based on timestamps. Typically, I have seen others and myself delete the output/ directory to force a full rebuild, but I have found that a more correct solution is to simply delete outputs whose timestamps are older than the according sources. You may ask why they would not be always rebuilt in this case, but I would actually rather than the file contents only be considered for rebuild criteria anyway (e.g. SHA).

After many years of discussing this problem, I am no longer looking to get this fixed. At some point, one contributor was interested in implementing builds for PureScript using Shake, but I do not know what has happened since.

## "Just because you are not interested in these changes, it doesn't mean nobody is."

Yes, some changes can be good, and some are not so nice or necessary. In some of the threads I found in trying to look for why some changes had happened, I found that those who disagreed heavily with my last post also disagreed with the introduction of some of these breaking changes. I also found that some library authors understood what would happen for many users and came up with migration plans, where functions were marked with `Warn` constraints to cause compiler warnings, so that users may see these warnings and change their usages before another future update would enact them.

## "I'm bored of [...] Your characterization of all of these issues is very self-centered."

Yes, my post was written from the perspective of people who deal with these problems and those I know who have stopped using PureScript over many years with each release.

Dismissive responses like these mostly serve two purposes: those who already disagree with the post see that someone else also disagrees strongly to feel vindicated in some way, while those who agree came to understand more clearly why they should not talk about these issues in public spaces.

In the end, these responses don't really help the situation in any way. What I have described still persists regardless of my writing about it, and actions may or may not be taken based on that. One previous post, about the situation of the PureScript compiler development, gave some justification for a new version of PureScript being released, containing many quality of life changes: https://github.com/justinwoo/my-blog-posts/blob/master/posts/2020-05-22-some-backported-purescript-changes-for-my-own-uses.md, https://github.com/purescript/purescript/issues/3886

## While we're still here...

 I have found that nobody has really bothered with package sets in the way that I had for many years. Many packages that were previously in the package sets are left unupdated, and the maintainers of the package sets do not try to chase down all of these packages to update them like I had. This seems like a minor complaint, but those dismissing this point should remember that many people's experience with PureScript inevitably ends up with looking at stale libraries that only needed a minor update, and users who are not so accustomed to updating packages with knowledge of changes of new PureScript versions only have so much time and willpower in their budget.

But I don't think there is a good solution for this, as you would need to find someone who wanted to make sure every package was updated as I did who will not end up withdrawing from community participation.

## Conclusion

It is not my intention to talk about anyone specifically when I criticize what has happened with PureScript in my articles. I am writing about what has happened as a result of interactions between multiple people, which may be inevitable given some of the circumstances, and these are just one interpretation of the events as told by me to a small audience of readers.

My articles do not require responses or answers. They are just insignificant pieces of writing that I present to people who want to ask me these questions.

While there are some people I actually dislike in PureScript, I only dislike them for personal reasons. I do not actually dislike the person who sent me responses that I have used in my article here, I only used these as talking points.
