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
    gen_server:start_link(?MODULE, [GribSrc,StorDir,LogF], []).


retrieve_gribs(Pid,From,To,AtTime,Delta) ->
  gen_server:call(Pid,{retrieve_gribs,From,To,AtTime,Delta},infinity).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args=[GS=#grib_source{name=N},_,LogF]) ->
  LogF(info,"grib_source_server [~p] registering with grib_ingest_server.", [N]),
  grib_ingest_server:register_server(N,GS,self()),
  process_flag(trap_exit,true),
  {ok, Args}.


handle_call(Request, _From, State=[GS=#grib_source{name=N},StorDir,LogF]) ->
  case Request of
    {retrieve_gribs,From,To,AtTime,Delta} ->
      {reply, retrieve_gribs(From,To,AtTime,Delta,StorDir,GS,LogF), State};
    get_grib_source_info ->
      {reply, GS, State};
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
  grib_ingest_server:unregister_server(N),
  ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ------------------------------------------------------------------
%% Internal Functions
%% ------------------------------------------------------------------


-spec compute_manifest(calendar:datetime(),calendar:datetime(),calendar:datetime(),integer(),#grib_source{}) ->
          unsatisfiable|{calendar:datetime(),calendar:datetime(),calendar:datetime(),[string()]}.
compute_manifest(From,To,AtTime,Delta,#grib_source{cycles=Cs,delay=Dhrs,file_hours=Fhrs,name_fun=F}) ->
  LC0 = cycle_logic:latest_cycle_for_time(From,Cs),
  LC1 = cycle_logic:latest_cycle_for_time(cycle_logic:shift_by_hours(AtTime,-Dhrs),Cs),
  LC2 = cycle_logic:cull_cycle(LC0,LC1),
  LC = cycle_logic:shift_cycle(LC2,Delta,Cs),
  case cycle_logic:gribs_for_interval(From,To,LC,Fhrs) of
    unsatisfiable ->
      unsatisfiable;
    GribIds=[{LC,Fh0}|Rest] ->
      GribNames = lists:map(fun({Cycle,Hr}) -> lists:flatten(F(Cycle,Hr)) end, GribIds),
      CovFrom = cycle_logic:shift_by_hours(LC,Fh0),
      {LC,Fh1} = lists:last(Rest),
      CovTo = cycle_logic:shift_by_hours(LC,Fh1),
      {LC,CovFrom,CovTo,GribNames}
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
          LogF(info, "grib_server [~p] -> finished downloading ~p after ~p seconds.", [N,Id,cycle_logic:seconds_elapsed(StartT)]),
          success;
        Error ->
          LogF(info, "grib_server [~p] -> failed to download ~p after ~p seconds with error ~p",
            [N,Id,cycle_logic:seconds_elapsed(StartT),Error]),
          Id
      end end, List),
  lists:filter(fun (X) -> not (X == success) end, Results).


-spec check_remote_grib(string(),#grib_source{}) -> [{available,string(),integer()}|{missing,string()}].
check_remote_grib(Id,#grib_source{url_prefix=Pfix}) ->
  Url = Pfix ++ "/" ++ Id,
  case httpc:request(head, {Url,[]},[],[]) of
      {ok, {{_,200,_},Hdrs,_}} ->
        CL = proplists:get_value("content-length",Hdrs),
        {present,Id,CL};
      _ ->
        {missing,Id}
  end.


-spec check_local_grib(string()) -> boolean.
check_local_grib(Path) ->
  case filelib:is_regular(Path) of
    true ->
      case file:read_file(Path ++ ".info") of
        {ok,B} ->
          Len = list_to_integer(binary_to_list(B)),
          case filelib:file_size(Path) of
            Len ->
              false;
            _WrongLen ->
              file:delete(Path),
              file:delete(Path ++ ".info"),
              true
          end;
        _ ->
          true
      end;
    false ->
      true
  end.


-spec write_grib_info([{present,string(),pos_integer()}],string(),string()) -> ok.
write_grib_info(Infos,StorDir,Domain) ->
  filelib:ensure_dir(filename:join([StorDir,Domain,"fakefile"])),
  lists:map(fun ({present,Id,Len}) ->
        Path = filename:join([StorDir,Domain,Id ++ ".info"]),
        filelib:ensure_dir(Path),
        file:write_file(Path,Len) end, Infos),
  ok.


-spec retrieve_gribs(calendar:datetime(),calendar:datetime(),calendar:datetime(),integer(),string(),#grib_source{},fun()) ->
    {calendar:datetime(),calendar:datetime(),[string()]}|{failed_downloads,[string()]}|{gribs_missing,[string()]}|unsatisfiable.
retrieve_gribs(From,To,AtTime,Delta,StorDir,GS=#grib_source{name=N,domain=D},LogF) ->
  LogF(info, "grib_server [~p] -> received request from ~w to ~w at time ~w delta ~p", [N,From,To,AtTime,Delta]),
  case compute_manifest(From,To,AtTime,Delta,GS) of
    {Cycle,CovFrom,CovTo,Ids} ->
      LogF(info,"grib_server [~p] -> manifest has ~p files covering interval from ~w to ~w from cycle ~w", [N,length(Ids),CovFrom,CovTo,Cycle]),
      NonLocals = lists:filter(fun (X) -> check_local_grib(filename:join([StorDir,D,X])) end, Ids),
      case NonLocals of
        [] ->
          LogF(info, "grib_server [~p] -> all ~p gribs available locally, returning immediately.", [N,length(Ids)]),
          {CovFrom,CovTo,lists:map(fun (X) -> filename:join([StorDir,D,X]) end, Ids)};
        _NotEmpty ->
          RemoteInfo = lists:map(fun(X) -> check_remote_grib(X,GS) end,NonLocals),
          case lists:filter(fun ({missing,_}) -> true; (_) -> false end, RemoteInfo) of
            [] ->
              LogF(info, "grib_server [~p] -> of ~p files, ~p checked & available locally, ~p available remotely, downloading now",
                [N,length(Ids),length(Ids)-length(NonLocals),length(NonLocals)]),
              write_grib_info(RemoteInfo,StorDir,D),
              case download_gribs(NonLocals,StorDir,GS,LogF) of
                [] ->
                  LogF(info, "grib_server [~p] -> all ~p remote files downloaded successfully, returning manifest covering [~w,~w] with ~p total files.",
                        [N,length(NonLocals),CovFrom,CovTo,length(Ids)]),
                      {CovFrom,CovTo,lists:map(fun (X) -> filename:join([StorDir,D,X]) end, Ids)};
                Failures ->
                  {downloads_failed,Failures}
              end;
            MissingGribs ->
              LogF(warn, "grib_server [~p] -> ~p gribs are not available locally or remotely, unsatisfiable request.", [N,length(MissingGribs)]),
              {missing_gribs,lists:map(fun ({missing,X}) -> X end, MissingGribs)}
          end
      end;
    unsatisfiable ->
      LogF(warn, "grib_server [~p] -> cannot satisfy request from ~w to ~w at time ~w with Delta ~w", [N,From,To,AtTime,Delta])
  end.


