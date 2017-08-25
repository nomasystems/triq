# Welcome to Triq -- Trifork QuickCheck for Erlang

[![Build Status](https://travis-ci.org/triqng/triq.svg?branch=master)](https://travis-ci.org/triqng/triq) 

This is a fork of Triq that is being run under the ZeroMQ Collaberation rules, http://rfc.zeromq.org/spec:22 with the one exception being that it is under the apache licence 



Triq (pronounced "Trick Check") is a free alternative to [QuviQ
eqc](http://www.quviq.com/). Triq's API is modelled closely after
`eqc`, so I recommend their tutorials and slides for an introduction
to QuickCheck.  Notice that QuviQ `eqc` has many features not found in
`triq`, but it is open source licensed under the Apache license.  For
instance, `eqc` has features for reporting, management, probably a
much better shrinking mechanism, cool C integration, and
professional support.

## Using Triq

### Installation via package manager

To use `triq`, you can add it as a project dependency and let your
package manager of choice handle it:

rebar.config: `{triq, "1.*"}`

erlang.mk: `DEPS = triq`

mix.exs: `{:triq, "~> 1.*"}`

### Installation from source into `$ERL_LIBS`

If you want to make `triq` available globally, you can install it from source
into your Erlang installation by adding it in one of your `$ERL_LIBS` paths.
So, it's either somewhere like `/usr/local/lib/lib/erlang/lib` or `$HOME/.erl`.

You can either download a tagged release from
`https://github.com/triqng/triq/releases` and extract that or clone the git
repo `https://github.com/triqng/triq` in the target directory. Once that's
done, cd into the directory and run `./rebar compile` or just `make`.

Now, if you start `erl`, you should be able to call functions from the
`triq` module.

```
$ erl
1> code:which(triq).
"/usr/local/lib/erlang/lib/triq/ebin/triq.beam"
2>
```

### Writing properties with Triq

To write properties with `triq`, include the header file:

```
-include_lib("triq/include/triq.hrl").
```

And you're ready to write property tests.  An example property could be:

```
prop_append() ->
    ?FORALL({Xs,Ys},{list(int()),list(int())},
            lists:reverse(Xs++Ys)
            ==
            lists:reverse(Ys) ++ lists:reverse(Xs)).
```

To test this property, run `triq:check/1`, thus:

```
1> triq:check(prop_append()).
......................................................................
..............................
Ran 100 tests
true
2>
```

If the test fails, it will try to shrink the result; here is an example:

```
prop_delete() ->
    ?FORALL(L,list(int()),
        ?IMPLIES(L /= [],
            ?FORALL(I,elements(L),
                ?WHENFAIL(io:format("L=~p, I=~p~n", [L,I]),
                          not lists:member(I,lists:delete(I,L)))))).
```

Which runs like this:
```
1> triq:check(triq_tests:prop_delete()).
x....Failed!
L=[4,5,5], I=5

Failed after 5 tests with false
Simplified:
        L = [0,0]
        I = 0
false
2>
```

You can get the values used for the failing test with `counterexample`,
and reuse the same test values with `check/2`:
```
3> A = triq:counterexample(triq_tests:xprop_delete()).
x.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxFailed!
L=[3,2,1,1,1], I=1

Failed after 101 tests with false
Simplified:
	L = [0,0]
	I = 0
[[0,0],0]
4> A.
[[0,0],0]
5> triq:check(triq_tests:xprop_delete(), A).
Failed!
L=[0,0], I=0

Failed after 1 tests with false
Simplified:
	L = [0,0]
	I = 0
false
6>
```

Modules compiled with the `triq.hrl` header, auto-export all functions named
`prop_*`, and have a function added called `check/0` which runs `triq:check/1`
on all the properties in the module.

```
1> mymodule:check().
```

You can also instruct `triq` to generate EUnit integration functions which allow
the module to be treated like an ordinary EUnit tests module. This avoids the need
for `triq` or generic `qc` support in your build/test tool of choice.
To achieve that, just make sure to include the attribute `-triq(eunit).` in the module.
So, the initial `triq.hrl` include would turn into this:

```
-include_lib("triq/include/triq.hrl").
-triq(eunit).
```

Good luck!
