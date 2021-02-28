## Rebar3 EMQ X Plugin Template

## How to use templates

To make the template available, you need to clone the repo to your
`~/.config/rebar3/templates` directory:

``` bash
mkdir -p ~/.config/rebar3/templates
git clone https://github.com/emqx/rebar3_emqx_plugin ~/.config/rebar3/templates
```

## Creating Erlang/OTP service layout

If you want to create a new directory layout for your new service you can do the following:

``` bash
rebar3 new emqx-plugin <plugin-name>
```
For example:
```bash
rebar3 new emqx-plugin emqx_auth_mysql
```

> Note: You need `rebar3` installed in your system.

This will generate the initial framework of an EMQ X plugin.

Requirements: `erlang`, `git`,`rebar3`
