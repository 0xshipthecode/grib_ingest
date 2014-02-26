
-module(grib_ingest_sup).

-behaviour(supervisor).

-include("grib_ingest.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

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


init(_Args) ->
  {ok, {{one_for_one, 5, 10}, [?CHILD(grib_ingest_server,worker,[])]}}.
