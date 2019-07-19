%%%===================================================================
%%% Expose the tables as macros so that if we ever have to do an
%%% inplace upgrade or some other migration, we don't have to hunt
%%% for all the usages. We can do tha A/B switch here.
%%%===================================================================
-define(TABLE_HARBOUR_REPORT_TASK,             harbour_report_task).

%%%===================================================================
%%% Tables
%%%
%%% url works as the key as it is unique
%%%===================================================================

-record(harbour_report_task,	{   url                             :: string(),
                                    filename                        :: string(),
                                    file_path       = undefined     :: undefined | string(),
                                    s3_url          = undefined     :: undefined | string(),
                                    state           = undefined     :: harbour_report_task_state(),
                                    date_created    = undefined     :: undefined | pos_integer(),
                                    date_modified   = undefined     :: undefined | pos_integer()
		}).

-type harbour_report_task()                                     :: #harbour_report_task{}.
-type harbour_report_task_state()                               :: undefined | download_started | download_completed | parse_started| parse_completed | archive_started | archive_completed.
