# Slantbot

Slantbot scans a list of approved subreddits searching for comments starting with its invocation command. Queries are harvested from the first line of such comments, and forwarded to Slant.co's search interface. Results are parsed, formatted and returned as a reply to the triggering comment. Additionally, it forwards any direct messages it receives to a maintainer's account. The subreddit list is read in from a remote SQL database; other configuration (usernames, API codes, etc.) are provided through environment variables.

The code is modular and extensible, and many parts could be useful to other projects.

- The actions performed within a subreddit are separate from the scanning mechanism, so the "skeleton" of Slantbot can be used for basically any reddit bot. In any case, it uses the Haskell [reddit library](https://hackage.haskell.org/package/reddit) and should serve as good example code for that.

- While the Algolia query composition is merely "good enough", the associated JSON results parser is generic, and complete as far as I've been able to discover, needing only a schema for the user data (which can be partial).

- `Main.hs` is generic even beyond the bot: it reads in configuration, then spawns a list of threads, which currently happens to be a singleton list of just the reddit bot, but could be anything (commented lines there hint at a future plan). Anyone curious about threads in Haskell should take a look, because it's almost embarrassingly simple. :)
