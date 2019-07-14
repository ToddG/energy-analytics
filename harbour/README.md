# harbour


## configuration

`config/sys.config` specifies a `watch_dir` that is used for publishing updated system configuration to components.

`config/sys.config`:
```Erlang
    {harbour, [
                {watch_dir, "data/config"} 
    ]}
```

Files in the the `watch_dir` are to be in the Erlang format for loading with consult, and are organized as {Topic, Message}. These messages will be broadcast to each subscriber as {Topic, Message}.
