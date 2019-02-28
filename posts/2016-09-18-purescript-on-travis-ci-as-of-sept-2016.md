# tl;dr

Use the Trusty beta build environment.

```yaml
language: node_js
sudo: required
dist: trusty
node_js: 6
install:
  - npm install -g bower purescript pulp
script:
  - npm install
  - bower install
  - npm test
```

Probably at some point you will run Purescript builds on Travis, and there doesn't seem to be much written on the internet about it. I found that if I didn't set up Travis correctly, I would end up with [10+ min build times](https://travis-ci.org/justinwoo/purescript-xstream/builds/160786861).

But by taking advantage of the [Trusty beta Build Environments](https://docs.travis-ci.com/user/trusty-ci-environment/), you can get fairly fast builds taking only [a little over a minute](https://travis-ci.org/justinwoo/purescript-xstream/builds/160812640). Just take the config above (swapping out `npm test` if you need) and add it as `.travis.yml` and you should be good to go.

Thanks to [Gary Burgess](https://twitter.com/gb_r) and [Christoph Hegemann](https://twitter.com/kritzcreek) for helping me look into this.