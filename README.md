# Advent of Code in Purescript
My solutions for **AOC 2021** and **AOC 2022** !

I'm using this to learn Purescript and purely functional programming in general.

When i hit a wall on Day 19 I started converting my [**Haskell solutions**](https://github.com/Blugatroff/adventofcode) to **Purescript** out of curiosity.

Even though Haskell and Purescript are obviously quite similar, there are some important differences because of which i actually enjoy writing Purescript more than Haskell:

- The Tooling just works™ (at least compared to haskell :D). Compilation is quite fast. [Purescript-language-server](https://github.com/nwolverson/purescript-language-server) never complained about incompatible compiler versions or some such, the package manager [spago](https://github.com/purescript/spago) feels simpler and more user friendly. ~~And perhaps most importantly, no YAML configuration files.~~ The newest version of spago migrated to using YAML :(
- It's very easy to simply look at the generated javascript for improving performance or gaining insight into how some purescript library actually works.
- You have to think about stack safety! I think this is also true about Haskell, but in Purescript you will quickly get a stack overflow while a stack unsafe program Haskell program might just eat up all your RAM.
- Purescript doesn't have [300 non-standard compiler extensions](https://wiki.haskell.org/Language_extensions).
- Proper Records and Type Rows.

Of course, my Purescript programs were never as fast as the Haskell equivalent, but that's fine. I didn't pick either language for their speed, if i want ultimate speed I'm just going to use Rust.

## Progress
- 2021: **`1..=22`**
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
just run run 2022 17 --part two
```
to run all days in a specific year use `all`
```
just run all 2021
```

## Architecture
The solution to every day is in its own module and only has one export named `day`.

A day is created using the `makeDay` function which expects the `parse` function and function to solve day one and two respectively.

This is the signature of the `makeDay` function:
```haskell
makeDay 
  :: forall input
   . (String -> Either String input) 
  -> (input -> Either String String) 
  -> (input -> Either String String) 
  -> Day
```
See [*2022/Day1.purs*](https://github.com/Blugatroff/pure-advent/blob/main/src/Year2022/Day1.purs) for an example of this.

The [Main](https://github.com/Blugatroff/pure-advent/blob/main/src/Main.purs) module then just has to pick the right function from the list of days, depending on the CLI arguments, and apply it to the input.

When running all solutions of a particular year, every day is run in *parallel* by spawning another node process.

The input is automatically downloaded from [`adventofcode.com`](https://adventofcode.com) and cached in the `inputs` directory
