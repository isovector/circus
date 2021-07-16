# circus

## Dedication

> Like it? Well I don't see why I oughtn't to like it. Does a boy get a chance
> to whitewash a fence every day?"
>
> --Tom Sawyer, 'The Adventures Of Tom Sawyer'


## Overview

This package contains types and a little DSL for producing JSON worthy of [netlistsvg](https://github.com/nturley/netlistsvg) --- a fantastic program for drawing circuit diagrams.


## Usage

While the `Circus.Types` module contains the bare-metal types that serialize in
the expected format, it's not a joyful experience to use for yourself. Instead,
`mkCell` is a smart constructor that will do what you want.

The most interesting types are `Cell`s, which correspond to electrical
components, and `Module`s, which are circuits themselves. The oddly named `Bit`
represents an electrical node (a wire), and two components are connected by
making them share a `Bit`.

Of course, `Module`s form a monoid, so they're easy to put together.

But what makes them easier to put together is the `Circus.DSL` module, which
provides some monadic actions for building `Module`s. It gives you access to a
fresh supply of unique `Bit`s, automatically accumulates `Cell`s, and allows for
`Bit` unification (feedback loops.)

Optimistically added components that never ended up getting used? Automatically
prune them with the `simplify` from `Circus.Simplify`!

When you're all done, call `renderModuleBS` or `renderModuleString` to get a
JSON representation ready to ship off to `netlistsvg`. Easy as that, my dude.

