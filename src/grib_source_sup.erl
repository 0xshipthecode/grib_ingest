
-module(grib_source_sup).

-behaviour(supervisor).

-include("grib_ingest.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1,load_grib_source_def/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


init([StorDir]) ->
  LogF = fun (C,T,A) -> io:format("~p: " ++ T ++ "~n", [C|A]) end,
  {ok,EtcFiles} = file:list_dir("etc"),
  GribDefFiles = lists:filter(fun (X) -> lists:suffix(".grib",X) end, EtcFiles),
  Defs = lists:map(fun (F) -> load_grib_source_def("etc/" ++ F) end, GribDefFiles),
  ChDefs = lists:map(fun (X) -> ?CHILD(grib_source_server,worker,[X,StorDir,LogF]) end, Defs),
  {ok, {{one_for_one, 5, 10}, ChDefs}}.


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

