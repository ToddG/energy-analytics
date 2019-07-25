# energy-analytics

Application container for 'harbour'.

Created by [erlang-seed](https://github.com/toddg/erlang-seed).

## dependencies

### local builds

* make
* docker
* docker-compose
* erlang
* rebar3

### container builds

* make
* docker
* docker-compose

Erlang and Rebar3 are only required if you are building locally using
`build`, `release`, `start`, `stop`, or `shell` targets. Neither Erlang or
Rebar3 is required to build within the containers, as the build container is
provisioned with both. This includes commands `cstart`, `cstop`, `cbuild`, and
`clog`.

## help

    $ make help

## quickstart

To start the entire app suite, including 'harbour', prometheus, grafana, and
node-exporter, do this:

    $ make cstart

To stop the entire app suite, do this:

    $ make cstop

To tail the logs, do this:

    $ make clog

## documentation

* http://zwrob.com/posts/monitoring-01/
* http://zwrob.com/posts/monitoring-02/
* http://zwrob.com/posts/monitoring-03/
* http://zwrob.com/posts/monitoring-04/
* http://zwrob.com/posts/erlang-01/

## notes

* https://stackoverflow.com/questions/17529623/erlang-location-of-hrl-files-in-multiple-applications#17533897

https://github.com/erlang/rebar3/issues/2129#issuecomment-515084409
