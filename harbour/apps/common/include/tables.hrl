%%%===================================================================
%%% Expose the tables as macros so that if we ever have to do an
%%% inplace upgrade or some other migration, we don't have to hunt
%%% for all the usages. We can do tha A/B switch here.
%%%===================================================================
-define(TABLE_HARBOUR_REPORT_TASK,             harbour_report_task).
-define(TABLE_HARBOUR_RECORD_STATE,            harbour_rstate).

%%%===================================================================
%%% In order for the CRUD semantics to work properly, namely the
%%% 'delete' method, we need to have the following structure fo
%%% each record (which defines a table in the db):
%%%
%%% The field is always 'id':
%%%
%%% 	{id, foo, bar, baz, ...}
%%%
%%% 'harbour_rstate' is an internal table that tracks the record state. The
%%% foreign key id, 'fkid', equals any table's 'id' like this:
%%%
%%% 	harbour_rstate.fkid =:= {anytable}.id
%%%
%%% element(1, Task) =:= task
%%% element(2, Task) =:= reference()
%%% element(1, Rstate) =:= rstate
%%% element(2, Rstate) =:= reference()
%%%===================================================================

-record(harbour_report_task,	{   id                          :: reference(),
                                    url                         :: string(),
                                    filename                    :: string(),
                                    file_path   = undefined     :: undefined | string(),
                                    s3_url      = undefined     :: undefined | string(),
                                    state       = undefined     :: undefined | download_started | download_completed | stored_in_db | stored_in_s3
                      }).


-record(harbour_rstate,	{	fkid			        :: reference(),
                                enabled         = true          :: boolean(),
                                date_created                    :: pos_integer(),
                                date_modified                   :: pos_integer()
		}).

-type harbour_report_task()                                     :: #harbour_report_task{}.

