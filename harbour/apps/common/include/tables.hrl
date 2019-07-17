%%%===================================================================
%%% In order for the CRUD semantics to work properly, namely the
%%% 'delete' method, we need to have the following structure fo
%%% each record (which defines a table in the db):
%%%
%%% The field is always 'id':
%%%
%%% 	{id, foo, bar, baz, ...}
%%%
%%% 'rstate' is an internal table that tracks the record state. The
%%% foreign key id, 'fkid', equals any table's 'id' like this:
%%%
%%% 	rstat.fkid =:= {anytable}.id
%%%
%%% element(1, Task) =:= task
%%% element(2, Task) =:= reference()
%%% element(1, Rstate) =:= rstate
%%% element(2, Rstate) =:= reference()
%%%===================================================================

-record(task,	{	id                      :: reference(),
                        type                    :: undefined | download_url,
                        data                    :: string(),
                        state = undefined       :: undefined | download_started | download_completed | stored_in_db
                      }).


-record(rstate,	{	fkid			:: reference(),
                        enabled = true          :: boolean(),
                        date_created            :: pos_integer(),
                        date_modified           :: pos_integer()
		}).
