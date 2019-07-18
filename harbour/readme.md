# harbour


## configuration

`config/sys.config` specifies a `config_dir` that is used for configuring the
runtime system. Files in this directory are watched for changes and changes to
the config files are detected with `inotify` and are published throughout the
system using `pubsub_server.erl`. Components load their configs at startup via
their `init` function and then subscribe to change notifications via their
`handle_info()` handlers.

_config/sys.config_:
```Erlang
    {harbour, [
                {config_dir, "data/config"} 
    ]}
```

Files in the the `config_dir` are to be in the Erlang format for loading with
consult, and are organized as {Topic, Message}. These messages will be
broadcast to each subscriber as {pubsub {Topic, Message}}.

### Example
Take the `work.config` file that configures the `work_server.erl` gen_server:

_work.config._:
```Erlang
{work_server_config, [
        {database_name, "work"},
        {database_path, "data"},
        {manifest_refresh_frequency_ms, 3600000},
    ]
}.
```

Changes to this file will be published on the event bus as:


```Erlang
{pubsub,
    {work_server_config, [
	    {database_name, "work"},
	    {database_path, "data"},
	    {manifest_refresh_frequency_ms, 3600000},
	]
    }
}
```

The consumer of this event will filter by `pubsub` and then by the message topic which, in this case, is `work_server_config`.

See any of the server implementations for details.

## Questions

* how do we perform a db migration/upgrade/schema change w/ mnesia?
