
-module(grib_server_sup).

-behaviour(supervisor).

-include("grib_ingest.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1,load_grib_source_def/1,start_grib_server/3,stop_grib_server/1]).

%% Helper macro for declaring children of supervisor
-define(GRIB_SRC_SRV(N, Args), {N, {grib_source_server, start_link, Args}, permanent, 5000, worker, [grib_source_server]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


init([StorDir0]) ->
  LogF = case application:get_env(grib_ingest,logging_function) of
    undefined ->
      fun (C,T,A) -> io:format("~p: " ++ T ++ "~n", [C|A]) end;
    {ok, F} ->
      F
  end,
  StorDir = case application:get_env(grib_ingest,grib_storage_dir) of
    undefiend ->
      StorDir0;
    {ok, StorDir1} ->
      StorDir1
  end,
  {ok,EtcFiles} = file:list_dir("etc"),
  GribDefFiles = lists:filter(fun (X) -> lists:suffix(".grib",X) end, EtcFiles),
  ChDefs = lists:map(fun (F) -> make_child_spec(F,StorDir,LogF) end, GribDefFiles),
  {ok, {{one_for_one, 5, 10}, ChDefs}}.


make_child_spec(File,StorDir,LogF) ->
  GS = load_grib_source_def("etc/" ++ File),
  #grib_source{name=N} = GS,
  ?GRIB_SRC_SRV(N,[GS,StorDir,LogF]).


start_grib_server(File,StorDir,LogF) ->
  ChSpec = make_child_spec(File,StorDir,LogF),
  supervisor:start_child(?MODULE,ChSpec).


stop_grib_server(Name) ->
  supervisor:terminate_child(?MODULE,Name).


-spec load_grib_source_def(string()) -> #grib_source{}.
load_grib_source_def(Path) ->
  {ok,B} =  file:read_file(Path),
  case erl_scan:string(binary_to_list(B)) of
    {ok,Toks,_} ->
      case erl_parse:parse_exprs(Toks) of
        {ok,Tree} ->
          {value, V, _Bindings} = erl_eval:exprs(Tree,erl_eval:new_bindings()),
          case V of
            #grib_source{} ->
              V;
            _ ->
              io:format("grib_ingest_sup: the file ~p does not contain a grib_source record.", [Path]),
              throw({not_grib_source,Path})
          end;
        {error, Info} ->
          io:format("grib_ingest_sup: failed to parse ~p with error ~p", [Path,Info]),
          throw({parse_error, Info})
      end;
    {error, Info, Loc} ->
      io:format("grib_ingest_sup: failed to scan ~p with error ~p at ~p",[Path,Info,Loc]),
      throw({scan_error,Info,Loc})
  end.

