-record(oasis_report, {     message_header                  :: message_header(),
                            message_payload                 :: message_payload()
                      }).
-record(message_header, {   timedate                        :: string(),
                            source                          :: string(),
                            version                         :: string()
                        }).
-record(message_payload, {  name                            :: string(),
                            report_items                    :: [report_item()]
                         }).
-record(report_item, {      report_header                   :: report_header(),
                            report_data                     :: report_data()
                     }).
-record(report_header, {    system                          :: string(),
                            tz                              :: string(),
                            report                          :: string(),
                            mkt_type                        :: string(),
                            uom                             :: string(),
                            interval                        :: string(),
                            sec_per_interval                :: string()
                       }).
-record(report_data, {      data_item                       :: string(),
                            resource_name                   :: string(),
                            opr_date                        :: string(),
                            interval_num                    :: pos_integer(),
                            interval_start_gmt              :: string(),
                            interval_end_gmt                :: string(),
                            value                           :: number()
                     }).

-type oasis_report()                                        :: #oasis_report{}.
-type message_header()                                      :: #message_header{}.
-type message_payload()                                     :: #message_payload{}.
-type report_item()                                         :: #report_item{}.
-type report_header()                                       :: #report_header{}.
-type report_data()                                         :: #report_data{}.

