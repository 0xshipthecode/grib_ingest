
-module(cycle_logic).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([shift_cycle/3,latest_cycle_for_time/2,cull_cycle/2,gribs_for_time/3,gribs_for_interval/4,gribs_for_interval2/5]).
-export([shift_by_hours/2,seconds_between/2,seconds_elapsed/1,min_time/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-spec normalize_cycle(integer(),integer(),pos_integer()) -> {pos_integer(),integer()}.
normalize_cycle(CycleNdx,DayDelta,NumCycles) when CycleNdx < 1 ->
  normalize_cycle(CycleNdx + NumCycles,DayDelta-1,NumCycles);
normalize_cycle(CycleNdx,DayDelta,NumCycles) when CycleNdx > NumCycles ->
  normalize_cycle(CycleNdx-NumCycles,DayDelta+1,NumCycles);
normalize_cycle(CycleNdx,DayDelta,_NumCycles) ->
  {CycleNdx,DayDelta}.


% convert a cycle ndx that is possibly out of the range 1..NumCycles into a representation
% {CycleNdx,DaysShift} that indicates how many days forward/backward we should go to find
% our particular cycle normalize a general cycle representation in the form of {index of cycle, number of days to shift} to
% a representation where ehere index of cycle is from 1 to NumCycles.
normalize_cycle(CycleNdx,NumCycles) ->
  normalize_cycle(CycleNdx,0,NumCycles).


% shift the given cycle <delta> cycles back (delta < 0) or forward (delta > 0) in time.
% called to identify a possibly useable cycle if a previously used cycle for some reason
% did not yield all necessary GRIB files
-spec shift_cycle(calendar:datetime(),integer(),[non_neg_integer()]) -> calendar:datetime().
shift_cycle(Cycle,0,_DailyCycles) ->
  Cycle;
shift_cycle(Cycle0={_,{H,_,_}},Delta,DailyCycles) ->
  CycleNdx0 = string:chr(DailyCycles,H),
  {CycleNdx,DayDelta} = normalize_cycle(CycleNdx0+Delta,length(DailyCycles)),
  set_hour(shift_by_days(Cycle0,DayDelta),lists:nth(CycleNdx,DailyCycles)).


% ensure that <Cycle> is not later than <ByCycle>
-spec cull_cycle(calendar:datetime(),calendar:datetime()) -> calendar:datetime().
cull_cycle(Cycle, ByCycle) when Cycle > ByCycle ->
  ByCycle;
cull_cycle(Cycle, _) ->
  Cycle.


% retrieve the latest cycle which is before the specified datetime <TS>
latest_cycle_for_time(TS={Date,{H,_,_}},CycleStartHrs) ->
  case lists:takewhile(fun (X) -> X =< H end, CycleStartHrs) of
    [] -> latest_cycle_for_time(set_hour(shift_by_days(TS,-1),23),CycleStartHrs);
    EligibleCycles -> {Date,{lists:last(EligibleCycles),0,0}}
  end.


% Retrieve a list of grib file identifications in the form {WhichCycle,WhichForecastHour} that should be downloaded
% to cover the datetime TS.  If a grib2 file is not available for this specific hour, two grib files are returned
% the closest before and after the datetime <ts>
-spec gribs_for_time(calendar:datetime(),calendar:datetime(),[non_neg_integer()]) -> [{calendar:datetime(),non_neg_integer()}].
gribs_for_time(TS,Cycle,Fhours) ->
  Hrs = hours_between(Cycle,TS),
  {UpTo,After} = lists:splitwith(fun (X) -> X =< Hrs end, Fhours),
  LeftClosest = lists:last(UpTo),
  LCTime = shift_by_hours(Cycle,LeftClosest),
  case LCTime == TS of
    true ->
      [{Cycle,LeftClosest}];
    false ->
      case After of
        [] ->
          unsatisfiable;
        [RightClosest|_] ->
          [{Cycle,LeftClosest},{Cycle,RightClosest}]
      end
  end.


% Retrieve a list of grib file identifications in the form {WhichCycle,WhichForecastHour} that should be downloaded
% to cover the datetime TS.  If a grib2 file is not available for this specific hour, two grib files are returned
% the closest before and after the datetime <ts>.  This version of the function is also able to use the next cycle if
% available and the given datetime is beyond the last forecast hour of the latest_cycle_for_time.
-spec gribs_for_time2(calendar:datetime(),calendar:datetime(),[non_neg_integer()],[non_neg_integer()]) -> [{calendar:datetime(),non_neg_integer()}].
gribs_for_time2(TS,LC,Cycles,Fh) ->
  C1 = min_time(latest_cycle_for_time(TS,Cycles),LC),
  Hrs = hours_between(C1,TS),
  {UpTo,After} = lists:splitwith(fun (X) -> X =< Hrs end, Fh),
  LeftClosest = lists:last(UpTo),
  LCTime = shift_by_hours(C1,LeftClosest),
  case LCTime == TS of
    true ->
      [{C1,LeftClosest}];
    false ->
      case After of
        [] ->
          C2 = shift_cycle(C1,1,Cycles),
          case C2 =< LC of
            true ->
              [{C1,LeftClosest},{C2,hd(Fh)}];
            false ->
              unsatisfiable
          end;
        [RightClosest|_] ->
          [{C1,LeftClosest},{C1,RightClosest}]
      end
  end.


-spec gribs_for_interval2(calendar:datetime(),calendar:datetime(),calendar:datetime(),[non_neg_integer()],[non_neg_integer()],[{calendar:datetime(),non_neg_integer()}]) ->[{calendar:datetime(),non_neg_integer()}].
gribs_for_interval2(To,To,LatestCycle,Cycles,FcHrs,Res) ->
  case gribs_for_time2(To,LatestCycle,Cycles,FcHrs) of
    unsatisfiable ->
      unsatisfiable;
    Gs ->
      lists:usort(Gs++Res)
  end;
gribs_for_interval2(From,To,_LatestCycle,_Cycles,_FcHrs,_Res) when From > To ->
  [];
gribs_for_interval2(From,To,LatestCycle,Cycles,FcHrs,Res) when From < To ->
  NextTime = min_time(shift_by_seconds(From,3600),To),
  case gribs_for_time2(From,LatestCycle,Cycles,FcHrs) of
    unsatisfiable ->
      unsatisfiable;
    Gs ->
      gribs_for_interval2(NextTime,To,LatestCycle,Cycles,FcHrs,Gs++Res)
  end.

gribs_for_interval2(From,To,LatestCycle,Cycles,FcHrs) ->
  gribs_for_interval2(From,To,LatestCycle,Cycles,FcHrs,[]).


-spec gribs_for_interval(calendar:datetime(),calendar:datetime(),calendar:datetime(),[non_neg_integer()],[non_neg_integer()],find_start|find_end) -> [{calendar:datetime(),non_neg_integer()}].
gribs_for_interval(From,_To,Cycle,_,_,_) when From < Cycle ->
  unsatisfiable;
gribs_for_interval(_From,_To,_Cycle,[],_Res,_State) ->
  unsatisfiable;
gribs_for_interval(From,From,Cycle,Fhrs,[],find_start) ->
  gribs_for_time(From,Cycle,Fhrs);
gribs_for_interval(_From,_To,_Cycle,[_],_Res,find_start) ->
  unsatisfiable;
gribs_for_interval(From,To,Cycle,[FH1,FH2|Rest],[],find_start) ->
  T1 = shift_by_hours(Cycle,FH1),
  T2 = shift_by_hours(Cycle,FH2),
  case (T1 =< From) and (T2 > From) of
    true ->
      gribs_for_interval(From,To,Cycle,[FH2|Rest],[FH1],find_end);
    false ->
      gribs_for_interval(From,To,Cycle,[FH2|Rest],[],find_start)
  end;
gribs_for_interval(From,To,Cycle,[FH|Rest],Res,find_end) ->
  case shift_by_hours(Cycle,FH) >= To of
    true ->
      lists:reverse(lists:map(fun (X) -> {Cycle,X} end,[FH|Res]));
    false ->
      gribs_for_interval(From,To,Cycle,Rest,[FH|Res],find_end)
  end;
gribs_for_interval(_From,_To,_Cycle,[],_,find_end) ->
  unsatisfiable.

gribs_for_interval(From,To,Cycle,FHs) ->
  gribs_for_interval(From,To,Cycle,FHs,[],find_start).


%% --------------------------------------------------
%% Miscellaneous time functions for the cycle logic
%% --------------------------------------------------

set_hour({Date,{_H,M,S}},H) ->
  {Date,{H,M,S}}.

shift_by_seconds(T,Sec) ->
  S = calendar:datetime_to_gregorian_seconds(T),
  calendar:gregorian_seconds_to_datetime(S + Sec).

shift_by_days(T,D) ->
  shift_by_seconds(T, D*86400).

seconds_between(T1,T2) ->
  S1 = calendar:datetime_to_gregorian_seconds(T1),
  S2 = calendar:datetime_to_gregorian_seconds(T2),
  S2 - S1.

hours_between(T1,T2) ->
  seconds_between(T1,T2) div 3600.

seconds_elapsed(Since) ->
  S1 = calendar:datetime_to_gregorian_seconds(Since),
  S2 = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
  S2 - S1.

shift_by_hours(T,Hrs) ->
  shift_by_seconds(T, Hrs*3600).

min_time(T1,T2) when T1 < T2 ->
  T1;
min_time(_T1,T2) ->
  T2.

%% --------------------------------------------------
%% Unit tests
%% --------------------------------------------------

-ifdef(TEST).

shift_by_days_test() ->
  {{2012,4,4},{1,1,1}} = shift_by_days({{2012,4,2},{1,1,1}},2),
  {{2012,3,31},{1,1,1}} = shift_by_days({{2012,4,2},{1,1,1}},-2).

gribs_for_time_matching_time_test() ->
  C = {{2012,1,1},{12,0,0}},
  [{C,2}] = gribs_for_time({{2012,1,1},{14,0,0}}, C, [0,1,2,3,4]).

gribs_for_time_between_time_test() ->
  C = {{2012,1,1},{12,0,0}},
  [{C,2},{C,3}] = gribs_for_time({{2012,1,1},{14,30,0}}, C, [0,1,2,3,4]),
  [{C,2},{C,4}] = gribs_for_time({{2012,1,1},{15,0,0}}, C, [0,1,2,4]).

gribs_for_time_unsat_test() ->
  C = {{2012,1,1},{12,0,0}},
  unsatisfiable = gribs_for_time({{2012,1,1},{15,0,0}}, C, [0,1,2]).


normalize_cycle_noop_test() ->
  {2,0} = normalize_cycle(2,0,4).

normalize_cycle_back_test() ->
  {2,-1} = normalize_cycle(-2,0,4).

normalize_cycle_fwd_test() ->
  {2,2} = normalize_cycle(12,0,5).


gribs_for_interval_sat1_test() ->
  C = {{2012,1,1},{12,0,0}},
  [{C,0},{C,1},{C,2},{C,3}] = gribs_for_interval({{2012,1,1},{12,0,0}},{{2012,1,1},{15,0,0}},C,[0,1,2,3]).

gribs_for_interval_zero_interval_test() ->
  C = {{2012,1,1},{12,0,0}},
  [{C,3}] = gribs_for_interval({{2012,1,1},{15,0,0}},{{2012,1,1},{15,0,0}},C,[0,1,2,3]).

gribs_for_interval_unsat_test() ->
  C = {{2012,1,1},{12,0,0}},
  unsatisfiable = gribs_for_interval({{2012,1,1},{12,0,0}},{{2012,1,1},{15,0,1}},C,[0,1,2,3]).

gribs_for_interval_unsat2_test() ->
  C = {{2012,1,1},{12,0,0}},
  unsatisfiable = gribs_for_interval({{2012,1,1},{15,0,0}},{{2012,1,1},{15,0,1}},C,[0,1,2,3]).

gribs_for_interval_unsat3_test() ->
  C = {{2012,1,1},{12,0,0}},
  unsatisfiable = gribs_for_interval({{2012,1,1},{11,59,0}},{{2012,1,1},{12,30,0}},C,[0,1,2,3]).

gribs_for_interval_sat2_test() ->
  C = {{2012,1,1},{12,0,0}},
  [{C,1},{C,2}] = gribs_for_interval({{2012,1,1},{13,30,0}},{{2012,1,1},{13,40,0}},C,[0,1,2,3]).


gribs_for_interval2_cross_cycle_test() ->
  Gs = cycle_logic:gribs_for_interval2({{2013,1,1},{17,0,0}},{{2013,1,2},{4,0,0}},{{2014,1,1},{0,0,0}},[0],[0,3,6,9,12,15,18,21]),
  Gs =[{{{2013,1,1},{0,0,0}},15}, {{{2013,1,1},{0,0,0}},18}, {{{2013,1,1},{0,0,0}},21}, {{{2013,1,2},{0,0,0}},0}, {{{2013,1,2},{0,0,0}},3}, {{{2013,1,2},{0,0,0}},6}].

gribs_for_interval2_in_cycle_test() ->
  Gs = cycle_logic:gribs_for_interval2({{2013,1,1},{17,0,0}},{{2013,1,1},{19,0,0}},{{2014,1,1},{0,0,0}},[0],[0,3,6,9,12,15,18,21]),
  Gs =[{{{2013,1,1},{0,0,0}},15}, {{{2013,1,1},{0,0,0}},18}, {{{2013,1,1},{0,0,0}},21}].

gribs_for_interval2_unsat_test() ->
  unsatisfiable = cycle_logic:gribs_for_interval2({{2013,1,1},{17,0,0}},{{2013,1,2},{4,0,0}},{{2013,1,1},{0,0,0}},[0],[0,3,6,9,12,15,18,21]).


gribs_for_interval2_single_test() ->
  [{{{2013,1,1},{0,0,0}},15}] = cycle_logic:gribs_for_interval2({{2013,1,1},{15,0,0}},{{2013,1,1},{15,0,0}},{{2013,1,1},{0,0,0}},[0],[0,3,6,9,12,15,18,21]).
-endif.

