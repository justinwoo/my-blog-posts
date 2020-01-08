# A simple Telegram bot in Rust with Actix

Recently, I found myself wanting to make a Telegram bot that did the same things as an existing bot I'd written in PureScript over three years ago (detailed here: <https://github.com/justinwoo/my-blog-posts/blob/master/posts/2016-09-25-writing-a-simple-telegram-chat-bot-in-purescript.md>). However, I soon ran into some problems with selecting a library to use: much of the ecosystem around async in Rust is still quite in motion to an outsider like me, and this was compounded in the telegram-bot crate I considered using, where a large update was underway that I didn't quite understand. Instead, I looked to find more flexible and easier solutions to use.

When we look at my (updated) previous implementation, it has three main characteristics:

* It uses FRP/reactive events
* These "drivers" are each responsible for one thing, such as interacting with the Telegram API
* It uses these events in a cycle to communicate between different "drivers"

In many ways, this was a use case for using some actor system anyway, so I ended up using Actix.

## Goals

My bot really only needs to do a handful of things:

* Run a timer, so that every 30 minutes, a job is run.
* On user messages, run the job immediately.
* When a job has run, return the result to the user as a message.

So this implies that there are three actors to our system:

1) a timer actor, which sends job requests
2) a job actor, which runs the jobs on requests and sends job results
3) a bot actor, which sends job requests and makes Telegram messages on job results

## `main.rs`

My entry point is quite short, but also implies a lot of other behaviors about my program:

```rust
use actix::prelude::*;

use tsbot::timer::Timer;

fn main() -> Result<(), std::io::Error> {
    let system = System::new("test");

    Timer { interval: 60 * 30 }.start();

    system.run()
}
```

This entry point starts a Actix System in which our actors will run, and curiously enough, only the timer actor is needed to initialize itself. This is because our job actor is declared as a Supervised System Service, so it will be run if it is requested from the System Registry. More on this in the next section.

Once our initialization is done, we can tell the system to run and our actors will get moving.

## `lib.rs`

I've found using `lib.rs` to be the most convenient way of working with Rust projects so far. This module contains declarations of the modules in our application.

```rust
#![feature(try_blocks)]

pub mod bot;
pub mod scraper;
pub mod telegram;
pub mod timer;
pub mod types;
```

Not all of these need to be public, but I kept it this way for both ease of use and so that anyone could use my application as a library if desired.

## `types.rs`

This is where I declared the types and Messages that will be used in my application.

```rust
use actix::prelude::*;

// we don't need a better error model, yet
pub type MyError = String;

// alias for Result so I don't have to repeat myself too much later on
pub type MyResult<A> = Result<A, MyError>;

// here are the job requests in our system
#[derive(Debug)]
pub enum Msg {
    Timer,
    User,
}

// here are the job results in our system
#[derive(Debug)]
pub struct RunResult {
    pub from_user: bool,
    pub text: String,
}

// a Msg can be transported in Actix via the Message trait
// however, we are not interested in the result of sending a message in this way
impl Message for Msg {
    type Result = ();
}

// same with RunResult, we are not interested in the result
impl Message for RunResult {
    type Result = ();
}
```

## `timer.rs`

This is the timer actor in our application.

```rust
use actix::prelude::*;

use crate::scraper::Scraper;
use crate::types::*;

pub struct Timer {
    pub interval: u64,
}

impl Actor for Timer {
    type Context = Context<Self>;

    fn started(&mut self, ctx: &mut Self::Context) {
        let scraper = Scraper::from_registry();

        scraper.do_send(Msg::Timer);

        ctx.run_interval(
            std::time::Duration::from_secs(self.interval),
            move |act, _ctx| {
                println!("Sending from {}", act.interval);
                scraper.do_send(Msg::Timer);
            },
        );
    }
}
```

Here, we define that our actor will have some property `interval`. Then we implement `Actor` and override `fn started`. In this function, we are able to get an instance (specifically, the *only* instance) of our scraper and send it messages. Using the execution context, we can run an interval to send messages by the duration.

## `scraper.rs`

The scraper service can be retrieved from the system registry because of the `Supervised` and `SystemService` implementations for our scraper actor.

```rust
use actix::prelude::*;

use crate::bot::TelegramBot;
use crate::types::*;

#[derive(Default)]
pub struct Scraper;

impl Actor for Scraper {
    type Context = Context<Self>;
}

impl Supervised for Scraper {}

impl SystemService for Scraper {}
```

Then, we define a `Handler` impl for our scraper so that it can perform jobs as response to the messages.

```rust
impl Handler<Msg> for Scraper {
    type Result = ();

    fn handle(&mut self, msg: Msg, _ctx: &mut Context<Self>) {
        let bot = TelegramBot::from_registry();

        let send_text = |text| {
            bot.do_send(RunResult {
                from_user: match msg {
                    Msg::User => true,
                    _ => false,
                },
                text,
            })
        };

        let scraper = std::process::Command::new("./scraper").output();

        match scraper {
            Err(e) => send_text(format!("Error running scraper: {}", e)),
            Ok(output) => {
            // ...
                    send_text(stdout.to_string());
            // ...
        }
    }
}
```

Like before, we are able to get our bot actor from the registry, since it is also registered as a supervised service.

## `bot.rs`

Our bot definition starts in a similar fashion as our jobs actor:

```rust
use actix::prelude::*;
use url::percent_encoding::{utf8_percent_encode, SIMPLE_ENCODE_SET};

use crate::scraper::Scraper;
use crate::telegram;
use crate::types::*;

#[derive(Default)]
pub struct TelegramBot {
    pub token: String,
    pub master: String,
    pub last_update_id: Option<u64>,
}

impl Supervised for TelegramBot {}

impl SystemService for TelegramBot {
    fn service_started(&mut self, _ctx: &mut Context<Self>) {
        println!("Telegram bot service started");
    }
}
```

One problem we do have here is that `Default` is used for our bot actor's initial state, but we do need these to be defined correctly if we are to use the Telegram API. We can put that in the `started` function of the `Actor` impl.

```rust
impl Actor for TelegramBot {
    type Context = Context<Self>;

    fn started(&mut self, ctx: &mut Self::Context) {
        // these will crash our program if they are not set, as a valid error condition
        self.token = std::env::var("TELEGRAM_BOT_TOKEN").unwrap();
        self.master = std::env::var("TELEGRAM_BOT_MASTER").unwrap();

        let scraper = Scraper::from_registry();

        // check for updates every 5 seconds
        ctx.run_interval(std::time::Duration::from_secs(5), move |act, _ctx| {
            // prepare the Telegram API request to get updates
            let method_params = format!(
                "getUpdates?timeout=1{}",
                match act.last_update_id {
                    // offsets are used to mark updates as read in the Telegram API
                    // prepare offset, which is last update id + 1
                    Some(id) => format!("&offset={}", id + 1),
                    None => "".to_owned(),
                }
            );

            // attempt to make proper url and make the request
            let res: MyResult<telegram::Response> = try {
                let url = telegram::get_url(&act.token, &method_params)?;
                let mut res = reqwest::get(url)
                    .or_else(|e| Err(format!("Failed to fetch getUpdates with error: {}", e)))?;
                res.json()
                    .or_else(|e| Err(format!("Failed to parse telegram res body: {}", e)))?
            };

            match res {
                Ok(res) if res.ok => {
                    // we only need to send one job request,
                    // so send it after checking all of the updates
                    let mut should_update = false;

                    for update in res.result {
                        // make sure to save this so that it can be used to mark updates as read above
                        act.last_update_id = Some(update.update_id);

                        // apply together a whole bunch of optional things
                        let _: Option<()> = try {
                            let message = update.message?;
                            let from = message.from?;
                            let text = message.text?;

                            // make sure only the correct user is asking for jobs to be run
                            if from.id.to_string() == act.master && text == "get" {
                                should_update = true;
                            }
                        };
                    }

                    if should_update {
                        println!("Should update based on user message");
                        scraper.do_send(Msg::User);
                    }
                }
                // if my bot breaks, i'm willing to ssh to the right machine and look at the logs
                Ok(res) => {
                    eprintln!(
                        "Telegram API problem, res.ok was not true: {:?}",
                        res.description
                    );
                }
                Err(err) => {
                    eprintln!("{}", err);
                }
            }
        });
    }
}
```

The `get_url` function referenced here is defined in `telegram.rs`, along with the types of things that are meant to be read out:

```rust
pub fn get_url(token: &String, method_params: &str) -> MyResult<reqwest::Url> {
    let raw_url = format!("https://api.telegram.org/bot{}/{}", token, method_params);
    let parsed_url = reqwest::Url::parse(&raw_url);

    parsed_url.or_else(|e| Err(format!("Failed to parse telegram url: {}", e)))
}
```

As with getting updates, handling the job results involves just a small call to the Telegram API.

```rust
impl Handler<RunResult> for TelegramBot {
    type Result = ();

    fn handle(&mut self, res: RunResult, _ctx: &mut Context<Self>) {
        println!("Handling run result: {:?}", res);

        let method_params = format!(
            "sendMessage?chat_id={}&text={}",
            self.master,
            // the message has to be url encoded
            utf8_percent_encode(&res.text, SIMPLE_ENCODE_SET)
        );

        let res: MyResult<()> = try {
            let url = telegram::get_url(&self.token, &method_params)?;
            let _ = reqwest::get(url).or_else(|e| Err(format!("Failed to send message: {}", e)))?;
        };

        if let Err(e) = res {
            println!("Error sending telegram message: {}", e);
        }
    }
}
```

## Conclusion

And that's about it! We can see that the application works as originally described, as a series of dependencies:

```
timer sends requests to scraper
bot sends requests to scraper
scraper sends results to bot
```

All three actors are observed to be running as a result of the timer being started within the system.

-----

I hope this has shown that using Actix can be a fun way to write applications like chat bots. If you've used reactive events libraries or cyclic events libraries like Cycle.js, using Actix should be somewhat familiar to how you think about how to write applications.

## Links

* Actix: <https://github.com/actix/actix>
* This repo: <https://github.com/justinwoo/tsbot>
