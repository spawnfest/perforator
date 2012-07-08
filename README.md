Perforator
=====

Perforator is a unit-testing style performance testing tool.


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
