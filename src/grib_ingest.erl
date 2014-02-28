-module(grib_ingest).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([start/0]).
-export([retrieve_gribs/4,retrieve_gribs_simple/3,get_grib_info/1]).


start() ->
  application:start(inets),
  application:start(grib_ingest).


get_grib_info(Name) ->
  Pid = grib_ingest_server:find_server_pid(Name),
  grib_source_server:grib_source_info(Pid).


retrieve_gribs_simple(GSName,From,To) ->
  % simplest case - try to retrieve the gribs from the source GSName
  % if this does not work, fail straight away
  retrieve_gribs(undefined,From,To,calendar:universal_time(),0,[],[{use_grib_source,GSName},try_retrieve]).


% runs a sequential strategy to try and retrieve gribs
% Strategy is a list of commands to execute

-type grib_strategy_step() :: {use_grib_source,string()}|try_retrieve|shift_cycle|{wait_for_mins,pos_integer()}.
-type retr_result() :: {failure,[{atom(),calendar:datetime(),any(),any()}]}|{success,calendar:datetime(),calendar:datetime(),[string()]}.
-spec retrieve_gribs(calendar:datetime(),calendar:datetime(),calendar:datetime(),[grib_strategy_step()]) -> retr_result().

retrieve_gribs(From,To,AtTime,Strategy) ->
  retrieve_gribs(undefined,From,To,AtTime,0,[],Strategy).


retrieve_gribs(_,_,_,_,_,Report,[]) ->
  {failure,Report};
retrieve_gribs(Pid,From,To,AtTime,_Delta,Report,[Step={use_grib_source,NewSrc}|Rest]) ->
  case grib_ingest_server:find_server_pid(NewSrc) of
    not_found ->
      retrieve_gribs(Pid,From,To,AtTime,0,[{failed,calendar:local_time(),Step,grib_source_not_found}|Report],Rest);
    Pid1 ->
      retrieve_gribs(Pid1,From,To,AtTime,0,Report,Rest)
  end;
retrieve_gribs(Pid,From,To,AtTime,Delta,Report,[try_retrieve|Rest]) ->
  Res = grib_source_server:retrieve_gribs(Pid,From,To,AtTime,Delta),
  case Res of
    {CovFrom,CovTo,Paths} ->
      {success,CovFrom,CovTo,Paths};
    Error ->
      retrieve_gribs(Pid,From,To,AtTime,Delta,[{failed,calendar:local_time(),try_retrieve,Error}|Report],Rest)
  end;
retrieve_gribs(Pid,From,To,AtTime,Delta,Report,[shift_cycle|Rest]) ->
  retrieve_gribs(Pid,From,To,AtTime,Delta+1,Report,Rest);
retrieve_gribs(Pid,From,To,AtTime,Delta,Report,[{wait_for_mins,TimeoutMin}|Rest]) ->
  timer:sleep(TimeoutMin * 60 * 1000),
  retrieve_gribs(Pid,From,To,AtTime,Delta,Report,Rest).

