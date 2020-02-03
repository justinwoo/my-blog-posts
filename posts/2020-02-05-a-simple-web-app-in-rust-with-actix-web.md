# A simple web app in Rust with Actix-Web

Recently, I decided to try writing a backend for my vidtracker app, which is a simple frontend for browsing what videos I have downloaded and have watched. I chose to use the actix-web library since it seemed nice enough to get started without too much trouble, while making using tokio async functions easy. I'm primarily writing this post just to show that there doesn't have to be too many pieces involved in using this library, since many of the existing examples seem quite involved and lack much written explanations.

## Structure

My program has four parts:

* `types.rs`, where I store some common types
* `db.rs`, where I do some talking to SQLite
* `files.rs`, where I do some talking to files-related things
* `main.rs`, where I set up actix-web

## `types.rs`

First, I define some type aliases as conveniences of what I'm going to work with in my application.

```rust
// actix related imports
use actix_web::{error, Error, HttpResponse};

// i use String because i don't care to handle the errors explicitly, for now
pub type MyError = String;

// convenience function to prepare MyError inside of MyResult
pub fn error<A, S: Into<String>>(s: S) -> MyResult<A> {
    let string = s.into();
    Err(string)
}

// convenience alias for Result that I will use throughout my application
pub type MyResult<A> = Result<A, MyError>;

// an alias for what the Result type in actix-web
pub type ActixResult = Result<HttpResponse, Error>;

// convenience function to take my application result and prepare an actix-web result
pub fn to_actix_result<A: Serialize>(result: MyResult<A>) -> ActixResult {
    match result {
        Ok(x) => Ok(HttpResponse::Ok().json(x)),
        Err(e) => Err(error::ErrorBadRequest(e)),
    }
}
```

Then comes my actual application types, which will mostly be encoded to and decoded from JSON.

```rust
pub type Path = String;

#[derive(Deserialize, Serialize, Debug)]
pub struct Watched {
    pub path: Path,
    pub created: String,
}

#[derive(Deserialize, Serialize, Debug)]
pub struct Update {
    pub path: Path,
    pub watched: bool,
}

#[derive(Deserialize, Serialize, Debug)]
pub struct Open {
    pub path: Path,
}
```

Again, I might be better off newtyping (creating single-argument structs) some of these, but I'll keep them like this for now.

## `db.rs`

For talking to SQLite, I ended up with the rusqlite crate. Most of my usage ends up like so:

```rust
pub fn get_watched(conn: &Connection) -> MyResult<Vec<Watched>> {
    let mut stmt = conn
        .prepare("select path, created from watched order by created desc")
        .or_else(|e| {
            return error(format!(
                "Could not prepare statement for get watched query: {}",
                e
            ));
        })?;

    let rows = stmt
        .query_map(params![], |row| {
            Ok(Watched {
                path: row.get(0).unwrap(),
                created: row.get(1).unwrap(),
            })
        })
        .or(error("Could not query map for get watched query"))?;

    let collected: rusqlite::Result<Vec<Watched>> = rows.collect();

    collected.or_else(|e| error(format!("Could not collect results of get watched: {}", e)))
}
```

So not too much stands in the way of me just writing SQL. Then even when I deserialize my results, I'm perfectly willing to just go at doing some manual operations, since I also end up including tests for this method in the same file.

With this, I've got my `Watched` entries created with `MyResult`, so my handler can make use of this.

## `files.rs`

Since my application is for listing some files based on some criteria and opening them, the methods in this file are about what you can expect. Even opening a file is handled in the simplest way, where I get a relative path and executable name to use for opening a file using a spawned process.

```rust
pub fn open_file(dir: &String, path: &String, exe: &String) -> MyResult<()> {
    let path = std::path::Path::new(&dir).join(path);
    std::process::Command::new(exe)
        .arg(&path)
        .spawn()
        .or(error(format!("Error opening exe: {}", exe)))?;
    Ok(())
}
```

## `main.rs`

Quite a bit happens in my main program, but not in too many complicated ways. First, the data that is shared in my application:

### Shared data

```rust
use std::sync::Mutex;

struct MyData {
    dir: String,
    exe: String,
    conn: Mutex<rusqlite::Connection>,
}
```

Since I'm okay with blocking all of my SQLite-related requests, I ended up using a mutex to lock access to this connection. Maybe a normal application would try to set up a connection pool.

To prepare this for actix-web to use, I need to put it in a `actix_web::web::Data`:

```rust
use actix_web::{web};

type MyState = web::Data<MyData>;
```

The entry point of my program begins by reading in some required environment variables and getting my db connection, after which I crate `MyData` and put it inside `actix_web::web::Data`.

```rust
#[actix_rt::main]
async fn main() -> io::Result<()> {
    let dir = std::env::var("DIR").expect("DIR must be set to the videos home.");
    let exe = std::env::var("EXE").expect("EXE must be set to an executable in PATH.");

    let conn = vt::db::get_conn(&dir).expect("Could not get a connection to the database.");
    vt::db::ensure_table(&conn).unwrap();

    let my_data = web::Data::new(MyData {
        dir,
        exe,
        conn: Mutex::new(conn),
    });

    // ...
```

### Starting the server

The actix-web API ends up having you use a fluent-style API to then register what all you have in your application. In my case, this means that I clone my `actix_web::web::Data` to pass into provide app data. Then I register all of my services that serve some routes.

```rust
#[actix_rt::main]
async fn main() -> io::Result<()> {
    // ...
    HttpServer::new(move || {
        App::new()
            .app_data(my_data.clone())
            .service(web::resource("/watched").to(watched))
            .service(web::resource("/files").to(files))
            .service(web::resource("/update").to(update))
            .service(web::resource("/get-icons").to(get_icons))
            .service(web::resource("/open").to(open))
            .service(actix_files::Files::new("/", "dist").index_file("index.html"))
    })
    .bind("127.0.0.1:4567")?
    .run()
    .await
}
```

My handlers then all look like normal async functions, with one caveat that I make a lock on my database connection on each handler.

```rust
macro_rules! get_conn {
    ($state: ident) => {
        &$state
            .conn
            .lock()
            .expect("Could not get lock on database connection.");
    };
}

async fn update(state: MyState, update: web::Json<Update>, req: HttpRequest) -> ActixResult {
    print_req(&req);
    let conn = get_conn!(state);
    let result = vt::db::upsert_watched_path(conn, &update.path, update.watched);
    to_actix_result(result.map(|_| "OK"))
}
```

Then the lock is released when `conn` above goes out of scope.

## Conclusion

I hope this has shown that writing a web service in Rust with actix-web doesn't have to require too much work, and that it's really about putting together some smaller pieces for the kinds of properties you need to work with.

Amusingly, the most likely fault in this application is where I use a mutex for my database connection. If I accidentally lock the database connection mutex while performing a long running operation, then my web server will be locked until that completes. In such cases, it's probably best to either adopt some sort of design that doesn't involve manual locks or locks within a fixed scope.

With this, I've now had an application running with a Rust backend and PureScript frontend for a while. While I kind of miss the convenience of being able to derive terms from types in PureScript, the overall ease of use of Rust has made me not doubt giving up PureScript on Node.

## Links

* This repo <https://github.com/justinwoo/vt>

* actix-web <https://github.com/actix/actix-web>
