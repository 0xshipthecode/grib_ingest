-module(grib_ingest_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, StartArgs) ->
  grib_ingest_sup:start_link(StartArgs),
  grib_source_sup:start_link(StartArgs).

stop(_State) ->
    ok.
