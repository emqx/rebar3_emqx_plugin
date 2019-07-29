Rebar3 EMQ X Plugin Template
============================

How to use templates
--------------------

To make the template available, you need to clone the repo to your
`~/.config/.rebar3/templates` directory:

``` bash
mkdir -p ~/.config/.rebar3/templates
git clone <this_repo_url> ~/.config/.rebar3/templates
```

Creating Erlang/OTP service layout
----------------------------------

If you want to create a new directory layout for your new service you can do the following:

``` bash
rebar3 new emqx-plugin <plugin-name>
```

Note: You need `rebar3` installed in your system.

This will generate the initial framework of an EMQ X plugin.

Requirements: erlang, git, rebar3

