# Mirakelbot

This is a tiny IRC bot written in Haskell to learn this wonderful language ;)

Inspired by [https://github.com/jaspervdj/number-six](number-six)

# How to use

## Installation

```sh
$ cabal sandbox init
$ cabal install
```
## GHCI

```sh
$ cabal repl
```

```haskel
let cfg = BotConfig "irc.freenode.net" (PortNumber 6667) (T.pack "#mychannel") (T.pack "mynickname") (T.pack "myRealName") (T.pack "!")
runBot cfg
```

## Stand alone
There will be soon a standalone version of the bot

# License

Copyright (c) 2014 Anatolij Zelenin


    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

