Perforator
=====

Perforator is a (E)unit-testing style performance testing tool.

Although it can be used as a standalone tool, to unleash it's full power
use it with the Perforator CI server. Check it out here:

https://github.com/Spawnfest2012/perforator2

Usage
-----

For ehanced comfort Perforator ships with a rebar plguin,
add this baby to your `rebar.config`:

``` erlang
{plugins, [perforator_rebar_plugin]}.
{deps, [
    {git, ".*", "git@github.com:Spawnfest2012/perforator.git", "master"}
]}.
```

The testing goes like this:

# Write some `*_perf.erl` modules and put them in your `tests/` directory.
# Run `./rebar perf`
# Exlore the wonderful results written in `.perf/`

_perf.erl modules
-----

`_perf.erl` modules are supposed to be very similar (in terms of syntax) to
EUnit's test modules.

Test objects can be:
* simple:
```test_case_perf() -> timer:sleep(100).``` functions.
* EUnit style fixtures:
```test_generator_perf_() -> {setup, Setup, Cleanup, TestObj}.```
* for more examples check out `test/sample_lists_perf.erl` module.

Note the `_perf_()`, `_perf()` suffixes, they are kind of the same as EUnit's
`_test()` and `_test_()` ones. The main difference between EUnit is that you
don't need to return a fun everywhere.


Notes on test runs and statistics
----

Each test case is being run 5 times with 500ms sleeps (it's hardcoded for
now, sorry!) and averages are being calculated for the statistics to be more
meaningful.

Some of the statistics gathered are rather sloppy, but at least the duration is
tracked pretty tightly.
