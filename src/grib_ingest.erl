
-module(grib_ingest).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([start/0]).



start() ->
  application:start(inets),
  application:start(grib_ingest).

