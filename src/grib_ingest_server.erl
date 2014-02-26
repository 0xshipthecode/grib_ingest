-module(grib_ingest_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([register_server/2,unregister_server/1,find_server_pid/1]).

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

register_server(Name,Pid) ->
  gen_server:call(?SERVER,{register_server,Name,Pid},5000).

unregister_server(Name) ->
  gen_server:call(?SERVER,{unregister_server,Name},5000).

find_server_pid(Name) ->
  gen_server:call(?SERVER,{get_server_pid,Name},5000).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(Request, _From, State=[Dict]) ->
  case Request of
    {register_server,Name,Pid} ->
      {reply,ok,[dict:store(Name,Pid,Dict)]};
    {unregister_server,Name} ->
      {reply,ok,[dict:erase(Name,Dict)]};
    {get_server_pid,Name} ->
      case dict:find(Name,Dict) of
        {ok,Pid} ->
          {reply,Pid,State};
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

