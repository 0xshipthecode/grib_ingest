-module(grib_source_server).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-behavior(gen_server).
-define(SERVER, ?MODULE).

-include("grib_ingest.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3,retrieve_gribs/5]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(GribSrc,StorDir,LogF) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [GribSrc,StorDir,LogF], []).


retrieve_gribs(Pid,From,To,AtTime,Delta) ->
  gen_server:call(Pid,{retrieve_gribs,From,To,AtTime,Delta},infinity).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  process_flag(trap_exit,true),
  {ok, Args}.


handle_call(Request, _From, State=[GS=#grib_source{name=N},StorDir,LogF]) ->
  case Request of
    {retrieve_gribs,From,To,AtTime,Delta} ->
      {reply, retrieve_gribs(From,To,AtTime,Delta,StorDir,GS,LogF), State};
    Other ->
      LogF(flash, "grib_server [~p] received an unexpected message ~p.", [N,Other]),
      {reply, invalid_request, State}
  end.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(shutdown, [#grib_source{name=N},_,LogF]) ->
  LogF(info, "grib_server [~p] is being shut down", [N]),
  ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

compute_manifest(From,To,AtTime,Delta,#grib_source{cycles=Cs,delay=Dhrs,file_hours=Fhrs,name_fun=F}) ->
  ToAdjusted = cycle_logic:shift_by_hours(AtTime,-Dhrs),
  LC0 = cycle_logic:latest_cycle_for_time(ToAdjusted),
  LC = cycle_logic:shift_cycle(LC0,Delta,Cs),
  case cycle_logic:gribs_for_interval(From,To,LC,Fhrs) of
    unsatisfiable ->
      unsatisfiable;
    GribIds=[{LC,F}|Rest] ->
      GribNames = lists:map(fun(X) -> lists:flatten(F(X)) end, GribIds),
      CovFrom = cycle_logic:shift_by_hours(LC,F),
      {LC,L} = lists:last(Rest),
      CovTo = cycle_logic:shift_by_hours(LC,L),
      {CovFrom,CovTo,GribNames}
  end.


-spec download_gribs([string()],string(),#grib_source{},fun()) -> [string()].
download_gribs(List,StorDir,#grib_source{name=N,url_prefix=Pfix,domain=D},LogF) ->
  Results = lists:map(fun (Id) ->
      Url = Pfix ++ "/" ++ Id,
      AbsPath = StorDir ++ "/" ++ D ++ "/" ++ Id,
      StartT = calendar:local_time(),
      filelib:ensure_dir(AbsPath),
      LogF(info, "grib_server [~p] -> downloading ~p to ~p.", [N,Id,AbsPath]),
      case httpc:request(get, {Url,[]}, [], [{stream, AbsPath}]) of
        {ok,saved_to_file} ->
          LogF(info, "grib_server [~p] -> finished downloading ~p after ~p seconds.",
            [N,Id,cycle_logic:seconds_elapsed(StartT)]),
          success;
        Error ->
          LogF(info, "grib_server [~p] -> failed to download ~p after ~p seconds with error ~p",
                [N,Id,cycle_logic:seconds_elapsed(StartT),Error]),
          Id
      end end, List),
  lists:filter(fun (X) -> not X == success end, Results).


-spec find_serverside_missing_gribs([string()],#grib_source{}) -> [string()].
find_serverside_missing_gribs(List,#grib_source{url_prefix=Pfix}) ->
  lists:filter(fun(Id) -> 
        case httpc:request(head, {Pfix ++ "/" ++ Id,[]},[],[]) of
          {ok, {{_,200,_},_,_}} ->
            true;
          _ ->
            false
        end
    end, List).


-spec retrieve_gribs(calendar:datetime(),calendar:datetime(),calendar:datetime(),integer(),string(),#grib_source{},fun()) ->
    {calendar:datetime(),calendar:datetime(),[string()]}|{failed_downloads,[string()]}|{gribs_missing,[string()]}|unsatisfiable.
retrieve_gribs(From,To,AtTime,Delta,StorDir,GS=#grib_source{name=N,domain=D},LogF) ->
  LogF(info, "grib_server [~p] -> received request from ~w to ~w at time ~w delta ~p", [N,From,To,AtTime,Delta]),
  case compute_manifest(From,To,AtTime,Delta,GS) of
    {CovFrom,CovTo,Ids} ->
      LogF(info,"grib_server [~p] -> manifest has ~p files covering interval from ~w to ~w", [N,length(Ids),CovFrom,CovTo]),
      NonLocals = lists:filter(fun (X) -> not filelib:is_regular(filename:join(StorDir,D,X)) end, Ids),
      case find_serverside_missing_gribs(NonLocals,GS) of
        [] ->
          LogF(info, "grib_server [~p] -> ~p gribs available locally, ~p available remotely, downloading now",
            [N,length(Ids)-length(NonLocals),length(NonLocals)]),
          case download_gribs(NonLocals,StorDir,GS,LogF) of
            [] ->
              LogF(info, "grib_server [~p] -> all ~p files downloaded successfully, returning manifest covering [~w,~w] with ~p total files.",
                    [N,length(NonLocals),CovFrom,CovTo,length(Ids)]),
                  {CovFrom,CovTo,lists:map(fun (X) -> filename:join(StorDir,D,X) end, Ids)};
            Failures ->
              {downloads_failed,Failures}
          end;
        MissingGribs ->
          LogF(warn, "grib_server [~p] -> ~p gribs are not available locally or remotely, unsatisfiable request.", [N,length(MissingGribs)]),
          {missing_gribs,MissingGribs}
      end;
    unsatisfiable ->
      LogF(warn, "grib_server [~p] -> cannot satisfy request from ~w to ~w at time ~w with Delta ~w", [N,From,To,AtTime,Delta])
  end.


