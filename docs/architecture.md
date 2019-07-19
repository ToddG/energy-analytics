# architecture

## harbour

### summary

                                                                                                             +----------------------+
                                                                                                             |                      |
                                                                                                             |    MANIFEST SERVER   |
                                                                                                             |                      |
                                                                                                    +--------+                      |
                                                                                                    |        |                      |
+----------------------+              +----------------------+      +----------------------+        |        |                      |
|                      |              |                      |      |                      |        |        |                      |
|                      |              |   CONFIG SERVER      |      |    PUBSUB SERVER     |        |        |                      |
|    INOTIFY           |              |                      |      |                      |        |        |                      |
|                      +-------------->                      +----->+    (event bus)       +-------->        +----------------------+
|                      |              |                      |      |                      |        |
|                      |              |                      |      |                      |        |
|                      |              |                      |      |                      |        |        +----------------------+
|                      |              |                      |      |                      |        |        |                      |
+----------------------+              |                      |      |                      |        |        |   WORK SERVER        |
                                      +---------^------------+      +----------------------+        |        |                      |
                                                                                                    |        |                      |
                                                                                                    +--------+                      |
                                                                                                    |        |                      |
                                                                                                    |        |                      |
                                                                                                    |        |                      |
                                                                                                    |        |                      |
                                                                                                    |        +----------------------+
                                                                                                    |
                                                                                                    |
                                                                                                    |        +----------------------+
                                                                                                    |        |                      |
                                                                                                    |        |    DOWNLOAD SERVER   |
                                                                                                    |        |                      |
                                                                                                    |        |                      |
                                                                                                    |        |                      |
                                                                                                    +--------+                      |
                                                                                                    |        |                      |
                                                                                                    |        |                      |
                                                                                                    |        |                      |
                                                                                                    |        +----------------------+
                                                                                                    |
                                                                                                    |
                                                                                                    |        +----------------------+
                                                                                                    |        |                      |
                                                                                                    |        |    DB SERVER         |
                                                                                                    |        |                      |
                                                                                                    |        |                      |
                                                                                                    |        |                      |
                                                                                                    +--------+                      |
                                                                                                             |                      |
                                                                                                             |                      |
                                                                                                             |                      |
                                                                                                             +----------------------+


### sequence

pubsub
```text
inotify -> config_server -> pubsub_server 
    (broadcasts config file changes on the event bus)

work_server:refresh() 
    -> manifest_server:reports()
    -> harbour_db:create(tasks)
```

#### api

* manifest_server:reports() : returns a list of urls for oasis reports
* work_server:refresh()     : queries the manifest_server  
                            

