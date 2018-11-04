#! /usr/bin/env perl

use warnings;
use strict;
use feature 'say';

my $input = <<"END";
2018-11-03|https://qiita.com/kimagure/items/85a64437f9af78398638
2018-10-29|https://qiita.com/kimagure/items/625070775da70b37b67e
2018-10-27|https://qiita.com/kimagure/items/71e938ee93e31bd2e79b
2018-10-24|https://qiita.com/kimagure/items/de2a4ff45dd8fe8be4b1
2018-10-18|https://qiita.com/kimagure/items/9d27015e12d4f22b53db
2018-09-15|https://qiita.com/kimagure/items/b08175d22f9950ba3dfb
2018-09-11|https://qiita.com/kimagure/items/522fa4dd4abdcc313c8e
2018-08-26|https://qiita.com/kimagure/items/93a42d67a8833f99fe2e
2018-08-13|https://qiita.com/kimagure/items/00c1ca57d6999904b595
2018-08-06|https://qiita.com/kimagure/items/1ea18bd6b782d45a48d5
2018-07-29|https://qiita.com/kimagure/items/0ce4d9d2792dd110ee45
2018-07-23|https://qiita.com/kimagure/items/c4bc704df3791437c9bb
2018-07-02|https://qiita.com/kimagure/items/8736fe6a2f25da526368
2018-06-22|https://qiita.com/kimagure/items/c419ba740ac134a837a2
2018-06-17|https://qiita.com/kimagure/items/4f0bb365965d31e6cd58
2018-06-04|https://qiita.com/kimagure/items/4b08e9f0479d5866ec04
2018-05-31|https://qiita.com/kimagure/items/1a569987fee84ae26d4f
2018-05-27|https://qiita.com/kimagure/items/b19cdbc1807109fb11cb
2018-05-16|https://qiita.com/kimagure/items/5c3f3fcb898e480c56be
2018-05-11|https://qiita.com/kimagure/items/6729a5d55ab99bcee8ec
2018-05-03|https://qiita.com/kimagure/items/3273d20c4c5ad74dbe26
2018-05-02|https://qiita.com/kimagure/items/7e313ee68280186d76dc
2018-04-27|https://qiita.com/kimagure/items/08c59fa21adcd6968ae1
2018-04-21|https://qiita.com/kimagure/items/a5e340242f038b0dc748
2018-04-17|https://qiita.com/kimagure/items/7a0d1675522c09b4bcb6
2018-04-15|https://qiita.com/kimagure/items/a870d250f75a6822759b
2018-04-02|https://qiita.com/kimagure/items/141423771ad1f5a84425
2018-04-01|https://qiita.com/kimagure/items/c37b228e80318d4158f0
2018-03-25|https://qiita.com/kimagure/items/7b86c1a16adb2045b584
2018-03-20|https://qiita.com/kimagure/items/570e6f2bbce5b4724564
2018-03-11|https://qiita.com/kimagure/items/b0b7da07d8183cb51d58
2018-03-03|https://qiita.com/kimagure/items/b35ad4a68939337275aa
2018-02-18|https://qiita.com/kimagure/items/581c63707673db61e061
2018-02-04|https://qiita.com/kimagure/items/18046a721881ac9270ac
2018-02-03|https://qiita.com/kimagure/items/b27245a5a11462145bd5
2018-02-02|https://qiita.com/kimagure/items/ca229cb4ba76db0c24a8
2018-01-30|https://qiita.com/kimagure/items/fd05ad13ee8def0fb4ed
2018-01-27|https://qiita.com/kimagure/items/43fd7b02db2950f04a1a
2018-01-14|https://qiita.com/kimagure/items/777133d6bbff67e3819d
2018-01-13|https://qiita.com/kimagure/items/4f5c6054870f631ff768
2018-01-08|https://qiita.com/kimagure/items/801e1c55d4f8f218f11e
2018-01-06|https://qiita.com/kimagure/items/7c3a01e2e5dfebb3313f
2017-12-28|https://qiita.com/kimagure/items/941c22effff608dda9a7
2017-12-23|https://qiita.com/kimagure/items/6e383ea0c6e29bf210e5
2017-12-17|https://qiita.com/kimagure/items/a011335bbb539e179f4e
2017-12-16|https://qiita.com/kimagure/items/09b24ed22cfc596248b4
2017-12-13|https://qiita.com/kimagure/items/24e6d3a0f47814c9630b
2017-12-11|https://qiita.com/kimagure/items/8ca4f386dbcb9f404b87
2017-12-09|https://qiita.com/kimagure/items/0c2712d5a417c1671e6d
2017-12-05|https://qiita.com/kimagure/items/7c3521cfbf00ad173801
2017-12-04|https://qiita.com/kimagure/items/f75befebdd37f6e8879f
2017-11-28|https://qiita.com/kimagure/items/6a9764966edd6cef497d
2017-11-21|https://qiita.com/kimagure/items/bb9bd3e4ffe1bba4c214
2017-11-17|https://qiita.com/kimagure/items/4847685d02d4b15a556c
2017-10-28|https://qiita.com/kimagure/items/0d9354900d7a7dbd3864
2017-10-22|https://qiita.com/kimagure/items/06d7eed9521b6217b771
2017-10-20|https://qiita.com/kimagure/items/daa388ffe14747d13f57
2017-09-08|https://qiita.com/kimagure/items/eeb40541fc56b8dba2cc
2017-08-26|https://qiita.com/kimagure/items/f750d85377520a14066f
2017-08-17|https://qiita.com/kimagure/items/ed612f25c3d3bcfaecd7
2017-07-28|https://qiita.com/kimagure/items/a0ee7313e8c7690bf3f5
2017-07-21|https://qiita.com/kimagure/items/7d777826acf371293a93
2017-07-15|https://qiita.com/kimagure/items/d8a0681ae05b605c5abe
2017-07-11|https://qiita.com/kimagure/items/d12525d42516f95dd541
2017-07-08|https://qiita.com/kimagure/items/00f97c7fc6cef178fa3c
2017-06-29|https://qiita.com/kimagure/items/cc0ea2982abdf1625e87
2017-06-25|https://qiita.com/kimagure/items/f1827c9129f3ee6ede35
2017-04-29|https://qiita.com/kimagure/items/5c248844ab28c8c91b16
2017-04-27|https://qiita.com/kimagure/items/115c04bc64d09a3a07a1
2017-03-27|https://qiita.com/kimagure/items/b576b5bfe370180599f8
2017-03-05|https://qiita.com/kimagure/items/653c52e77d7cd3567498
2016-12-14|https://qiita.com/kimagure/items/97e8d7b9cb318ba7ebef
2016-10-20|https://qiita.com/kimagure/items/a340fde5000dfd0102de
2016-10-13|https://qiita.com/kimagure/items/0a2f3d60789c646e4426
2016-10-04|https://qiita.com/kimagure/items/6820e2df2a7604047862
2016-09-25|https://qiita.com/kimagure/items/2da0fe86b218b3f832d0
2016-09-21|https://qiita.com/kimagure/items/2ebce1399bac00c79656
2016-09-18|https://qiita.com/kimagure/items/b1a892870de4073908af
2016-07-31|https://qiita.com/kimagure/items/5674e3ae9c87262af762
2016-07-10|https://qiita.com/kimagure/items/5947e2db40b9ec2226bf
2016-03-15|https://qiita.com/kimagure/items/92f0a278971c7deb8eb5
2016-02-18|https://qiita.com/kimagure/items/15361b4bc5f4dfac20e8
END

my @posts = split "\n", $input;

my $test = <<"TEST";
---
title: Using PureScript easily with Nix
tags: purescript NixOS
author: kimagure
slide: false
---
For a while, I've been wanting to come up with some kind of way to download and prepare programs I need to work with my various PureScript/PS-related Dhall projects, but could not really come up with a clear solution. While one popular method is to try to use Docker containers for everything, it incurs a lot of costs both on initialization and repeat initializations, and while many use npm to manage all of their dependencies, many times I am both not running on environments with npm installed nor do I want to use npm to manage installations. So, I ended up with Nix.
TEST

my $file_template = <<"END";
# TITLE
BODY
END

sub handle_post_body {
    my ($text_body, $date_string, $url) = @_;

    (my $title) = ($text_body =~ /title: (.*)/);

    if ($title) {
        (my $a, my $b, my $header_stripped) = split "---", $text_body;

        my $contents = $file_template;
        $contents =~ s/TITLE/$title/;
        $contents =~ s/BODY/$header_stripped/;
        chomp($contents);

        my $target = "${date_string}-${title}.md";
        $target =~ s/ /-/g;
        $target =~ s/\//-/g;
        $target = lc $target;

        open(my $fh, '>', $target) or die "Could not get file handle for $target";
        print $fh $contents;
        close $fh;

        say "wrote $target";
    } else {
        say "could not get title from text body for $url";
    }
}

print `mkdir -p .attempted`;

sub handle_post {
    my ($post) = @_;

    my $attempted = ".attempted/$post";
    $attempted =~ s/\//-/g;
    $attempted =~ s/\|/-/g;
    $attempted =~ s/:/-/g;

    unless (-e $attempted) {
        (my $date_string, my $url) = split /\|/, $post;

        if ($date_string) {
            my $text_body = `curl ${url}.md`;
            handle_post_body($text_body, $date_string, $url);
        } else {
            say "could not split to date.url: $post"
        }
    }

    print `touch $attempted`;
}

foreach my $post (@posts) {
    handle_post($post);
}
