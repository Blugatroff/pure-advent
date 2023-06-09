# Advent of Code in Purescript
My solutions for **AOC 2021** and **AOC 2022** !

I'm using this to learn Purescript and purely functional programming in general.

When i hit a wall on Day 19 I started converting my [**Haskell solutions**](https://github.com/Blugatroff/adventofcode) to **Purescript** out of curiosity.

Even though Haskell and Purescript are obviously quite similar, there are some important differences because of which i actually enjoy writing Purescript more than Haskell:

- The Tooling just works™ (at least compared to haskell :D). Compilation is quite fast. [Purescript-language-server](https://github.com/nwolverson/purescript-language-server) never complained about incompatible compiler versions or some such, the package manager [spago](https://github.com/purescript/spago) feels simpler and more user friendly. And perhaps most importantly, no YAML configuration files.
- It's very easy to simply look at the generated javascript for improving performance or gaining insight into how some purescript library actually works.
- You have to think about stack safety! This is also true about Haskell, but in Purescript you will quickly get a stack overflow while a stack unsafe program Haskell program will just eat up all your RAM.
- Purescript doesn't have [300 non-standard compiler extensions](https://wiki.haskell.org/Language_extensions).
- [Actual proper arrays](https://pursuit.purescript.org/packages/purescript-arrays/) (both immutable and ST variants). I still don't understand why it's such a pain to get a generic, mutable, dynamic array in the ST or IO Monads in Haskell.
- Proper Records and Type Rows.

Of course, my Purescript programs were never as fast as the Haskell equivalent, but that's fine. I didn't pick either language for their speed, if i want ultimate speed I'm just going to use Rust.

## Progress
- 2021: **`1..=18`**
- 2022: **`1..=25`**

## Dependencies
- spago
- purescript-psa
- just
- esbuild
- nodejs

If you use Nix can also get a complete development environment with **`nix develop`**.

## How to Run

```sh
spago run -a 2022 -a 17 -a --part -a two
```
or using [`just`](https://just.systems/man/en/)
```
just run 2022 17 --part two
```

## Architecture
The solution to every day is in its own module and exports two functions, one for either part of the problem.

Both functions always have the same signature.
```haskell
partOne :: String -> Either String String
partTwo :: String -> Either String String
```
See [*2022/Day1.purs*](https://github.com/Blugatroff/pure-advent/blob/main/src/Year2022/Day1.purs) for an example of this.

The [Main](https://github.com/Blugatroff/pure-advent/blob/main/src/Main.purs) module then just has to pick the right function from the list of days, depending on the CLI arguments, and apply it to the input.

The input is automatically downloaded from [`adventofcode.com`](https://adventofcode.com) and cached in the `inputs` directory
