-module(grib_ingest_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("grib_ingest.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([register_server/3,unregister_server/1,find_server_pid/1]).
-export([get_grib_source/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [dict:new()], []).

register_server(Name,GS,Pid) ->
  gen_server:call(?SERVER,{register_server,Name,GS,Pid},5000).

unregister_server(Name) ->
  gen_server:call(?SERVER,{unregister_server,Name},5000).

find_server_pid(Name) ->
  gen_server:call(?SERVER,{get_server_pid,Name},5000).

get_grib_source(Name) ->
  gen_server:call(?SERVER,{get_grib_source,Name},5000).


init(Args) ->
    {ok, Args}.

handle_call(Request, _From, State=[Dict]) ->
  case Request of
    {register_server,Name,GS,Pid} ->
      {reply,ok,[dict:store(Name,{Pid,GS},Dict)]};
    {unregister_server,Name} ->
      {reply,ok,[dict:erase(Name,Dict)]};
    {get_server_pid,Name} ->
      case dict:find(Name,Dict) of
        {ok,{Pid,_}} ->
          {reply,Pid,State};
        error ->
          {reply,not_found,State}
      end;
    {get_grib_source,Name} ->
      case dict:find(Name,Dict) of
        {ok,{_,GS}} ->
          {reply,GS,State};
        error ->
          {reply,not_found,State}
      end
  end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

