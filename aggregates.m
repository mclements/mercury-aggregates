%--------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%--------------------------------------------------%
% Copyright (C) 2022 Mark Clements.
% This file is distributed under the terms specified in COPYING.
%--------------------------------------------------%
%
% File: aggregates.m.
% Authors: mclements
% Stability: low.
%
% This module defines bag and set aggregates with nested submodules for
% floats and ints.
%
%--------------------------------------------------%
%--------------------------------------------------%

:- module aggregates.

:- interface.

:- import_module list.

    % bag_aggr(Predicate, Aggregator, Initial, Result) returns a bag aggregate
    % based on a predicate using an aggregator with an initial value. 
    % This uses bag semantics and assumes that the aggregator does not depend
    % on the order of the (unsorted) aggregate.
    %
:- pred bag_aggr(pred(T)::(pred(out) is nondet),
		 pred(T,U,U)::pred(in,in,out) is det, U::in, U::out) is det.

    % bag_count(Predicate, Result) returns a bag count.
    % This has the same semantics as bag_aggr.
    %
:- pred bag_count(pred(T)::(pred(out) is nondet), int::out) is det.

    % bag_max(Predicate, Initial, Result) returns a max aggregate.
    %
:- pred bag_max(pred(T)::(pred(out) is nondet), T::in, T::out) is det.

    % bag_min(Predicate, Initial, Result) returns a min aggregate.
    %
:- pred bag_min(pred(T)::(pred(out) is nondet), T::in, T::out) is det.

    % bag_solutions(Predicate, List) returns an unsorted bag of solutions
    % as a list.
:- pred bag_solutions(pred(T)::(pred(out) is nondet), list(T)::out) is det.

    % set_max(Predicate, Initial, Result) returns a max aggregate.
    %
:- pred set_max(pred(T)::(pred(out) is nondet), T::in, T::out) is det.

    % set_min(Predicate, Initial, Result) returns a min aggregate.
    %
:- pred set_min(pred(T)::(pred(out) is nondet), T::in, T::out) is det.

    % bag_solutions(Predicate, List) returns an unsorted bag of solutions
    % as a list.

    % set_count(Predicate, Result) returns a set count.
    %
:- pred set_count(pred(T)::(pred(out) is nondet), int::out) is det.

:- module aggregates.floats.
:- interface.
:- import_module float.

    % bag_max(Predicate, Result) returns a max aggregate.
    % Returns min if the count is less than 1.
    %
:- pred bag_max(pred(float)::(pred(out) is nondet), float::out) is det.

    % bag_min(Predicate, Result) returns a min aggregate.
    % Returns max if the count is less than 1.
    %
:- pred bag_min(pred(float)::(pred(out) is nondet), float::out) is det.

    % bag_sum(Predicate, Result) returns a bag sum aggregate.
    % This has the same semantics as bag_aggr.
    %
:- pred bag_sum(pred(float)::(pred(out) is nondet), float::out) is det.

    % bag_avg(Predicate, Result) returns a bag average aggregate.
    % This has the same semantics as bag_aggr.
    % Returns nan if the count is less than 1.
    %
:- pred bag_avg(pred(float)::(pred(out) is nondet), float::out) is det.

    % bag_mean(Predicate, Result) returns a bag average aggregate.
    % This has the same semantics as bag_aggr.
    % Returns nan if the count is less than 1.
    % This is an alias for bag_avg.
    %
:- pred bag_mean(pred(float)::(pred(out) is nondet), float::out) is det.

    % bag_geom_mean(Predicate, Result) returns a bag geometric average aggregate.
    % This has the same semantics as bag_aggr.
    % Returns nan if the count is less than 1.
    %
:- pred bag_geom_mean(pred(float)::(pred(out) is nondet), float::out) is det.

    % bag_var(Predicate, Result) returns a bag sample variance aggregate.
    % Returns nan if the count is less than 2.
    % This has the same semantics as bag_aggr.
    %
:- pred bag_var(pred(float)::(pred(out) is nondet), float::out) is det.

    % bag_median(Predicate, Result) returns a bag median aggregate.
    % Returns nan if the count is less than 1.
    %
:- pred bag_median(pred(float)::(pred(out) is nondet), float::out) is det.

    % bag_quantile(Predicate, Quantile, Result) returns a bag quantile aggregate.
    % Returns nan if the count is less than 1.
    %
:- pred bag_quantile(pred(float)::(pred(out) is nondet), float::in, float::out)
   is det.

    % bag_summary(Predicate, Quantile, Result) returns a bag summary
    % that includes {Minimum, FirstQuartile, Median, Mean, ThirdQuartile, Maximum}.
    % This has the same format as R's summary() for a numeric vector.
    %
:- pred bag_summary(pred(float)::(pred(out) is nondet),
		    {float,float,float,float,float,float}::out) is det.

    % set_max(Predicate, Result) returns a max aggregate.
    % Returns min if the count is less than 1.
    %
:- pred set_max(pred(float)::(pred(out) is nondet), float::out) is det.

    % set_min(Predicate, Result) returns a min aggregate.
    % Returns max if the count is less than 1.
    %
:- pred set_min(pred(float)::(pred(out) is nondet), float::out) is det.

    % set_sum(Predicate, Result) returns a set sum aggregate.
    %
:- pred set_sum(pred(float)::(pred(out) is nondet), float::out) is det.

    % set_avg(Predicate, Result) returns a set average aggregate.
    % Returns nan if the count is less than 1.
    %
:- pred set_avg(pred(float)::(pred(out) is nondet), float::out) is det.

    % set_mean(Predicate, Result) returns a set average aggregate.
    % Returns nan if the count is less than 1.
    % This is an alias for set_avg.
    %
:- pred set_mean(pred(float)::(pred(out) is nondet), float::out) is det.

    % set_geom_mean(Predicate, Result) returns a set geometric average aggregate.
    % Returns nan if the count is less than 1.
    %
:- pred set_geom_mean(pred(float)::(pred(out) is nondet), float::out) is det.

    % set_var(Predicate, Result) returns a set sample variance aggregate.
    % Returns nan if the count is less than 2.
    %
:- pred set_var(pred(float)::(pred(out) is nondet), float::out) is det.

    % set_median(Predicate, Result) returns a set median aggregate.
    % Returns nan if the count is less than 1.
    %
:- pred set_median(pred(float)::(pred(out) is nondet), float::out) is det.

    % set_quantile(Predicate, Quantile, Result) returns a bag quantile aggregate.
    % Returns nan if the count is less than 1.
    %
:- pred set_quantile(pred(float)::(pred(out) is nondet), float::in, float::out)
   is det.

    % set_summary(Predicate, Quantile, Result) returns a set summary
    % that includes {Minimum, FirstQuartile, Median, Mean, ThirdQuartile, Maximum}.
    % This has the same format as R's summary() for a distinct numeric vector.
    %
:- pred set_summary(pred(float)::(pred(out) is nondet),
		    {float,float,float,float,float,float}::out) is det.

    % bag_cum_sum(Predicate(By,X)::in, Predicate(By,CumSum)::out) takes a predicate
    % with a By value and an X value and outputs a predicate with the By value and a bag 
    % cumulative sum for the X values sorted by the By value.
    %
:- pred bag_cum_sum(pred(T,float)::in(pred(out,out) is nondet),
		    pred(T,float)::out(pred(out,out) is nondet)) is det.

    % bag_cum_sum(Predicate(By,X)) = Predicate(By,CumSum) takes a predicate
    % with a By value and an X value and returns a predicate with the By value and a bag 
    % cumulative sum for the X value sorted by the By value.
    %
:- func bag_cum_sum(pred(T,float)::in(pred(out,out) is nondet)) =
   (pred(T,float)::out(pred(out,out) is nondet)) is det.

    % bag_row_number(Predicate(By)::in, Predicate(By,RowNumber)::out) takes a predicate
    % with a By value and outputs a predicate with the By value and a bag 
    % cumulative sum for the X value sorted by the By value.
    %
:- pred bag_row_number(pred(T)::in(pred(out) is nondet),
		   pred(T,int)::out(pred(out,out) is nondet)) is det.
    % bag_row_number(Predicate(By)) = Predicate(By,RowNumber) takes a predicate
    % with a By value and returns a predicate with the By value and a bag 
    % row number sorted by the By value.
    %
:- func bag_row_number(pred(T)::in(pred(out) is nondet)) = 
   (pred(T,int)::out(pred(out,out) is nondet)) is det.

:- end_module aggregates.floats.

:- module aggregates.ints.
:- interface.
:- import_module int.

    % bag_max(Predicate, Result) returns a max aggregate.
    % Returns min_int if the count is less than 1.
    %
:- pred bag_max(pred(int)::(pred(out) is nondet), int::out) is det.

    % bag_min(Predicate, Result) returns a min aggregate.
    % Returns max_int if the count is less than 1.
    %
:- pred bag_min(pred(int)::(pred(out) is nondet), int::out) is det.

    % bag_sum(Predicate, Result) returns a bag sum aggregate.
    % This has the same semantics as bag_aggr.
    % Returns 0 if the count is less than 1.
    %
:- pred bag_sum(pred(int)::(pred(out) is nondet), int::out) is det.

    % set_max(Predicate, Result) returns a max aggregate.
    % Returns min_int if the count is less than 1.
    %
:- pred set_max(pred(int)::(pred(out) is nondet), int::out) is det.

    % set_min(Predicate, Result) returns a min aggregate.
    % Returns max_int if the count is less than 1.
    %
:- pred set_min(pred(int)::(pred(out) is nondet), int::out) is det.

    % set_sum(Predicate, Result) returns a set sum aggregate.
    % Returns 0 if the count is less than 1.
    %
:- pred set_sum(pred(int)::(pred(out) is nondet), int::out) is det.
:- end_module aggregates.ints.

:- implementation.

:- import_module int, solutions, bool.

:- pred count_max_2(pred(T)::(pred(out) is nondet), int::out) is det.
:- pred count_max_2_pred(T::in,bool::out,int::in,int::out) is det.
count_max_2_pred(_, Continue, Acc1, Acc2) :-
    Acc2 = Acc1+1, (if Acc2>=2 then Continue=no else Continue=yes).
count_max_2(Predicate, Result) :-
    promise_equivalent_solutions [Result] (
    do_while(Predicate, count_max_2_pred, 0, Result)).

bag_aggr(Predicate, Aggregator, Initial, Result) :-
    promise_equivalent_solutions[Result] (
	unsorted_aggregate(Predicate, Aggregator, Initial, Result)).
bag_count(Predicate, Count) :-
    bag_aggr(Predicate,
	     (pred(_X::in,Y::in,Z::out) is det :- Z = Y+1),
	     0,
	     Count).
bag_max(Predicate, Initial, Max) :-
    bag_aggr(Predicate,
	     (pred(X::in,Y::in,Z::out) is det :-
		  compare(R,X,Y),
		  (if R = (>) then Z = X else Z = Y)),
	     Initial, Max).
bag_min(Predicate, Initial, Min) :-
    bag_aggr(Predicate,
	     (pred(X::in,Y::in,Z::out) is det :-
		  compare(R,X,Y),
		  (if R = (<) then Z = X else Z = Y)),
	     Initial, Min).
bag_solutions(Predicate, List) :-
    promise_equivalent_solutions[List] (
	unsorted_solutions(Predicate,List)).
set_count(Predicate, Count) :-
    aggregate(Predicate,
	      (pred(_X::in,Y::in,Z::out) is det :- Z = Y+1),
	      0,
	      Count).
set_max(Predicate, Initial, Max) :-
    aggregate(Predicate,
	      (pred(X::in,Y::in,Z::out) is det :-
		   compare(R,X,Y),
		   (if R = (>) then Z = X else Z = Y)),
	      Initial, Max).
set_min(Predicate, Initial, Min) :-
    aggregate(Predicate,
	      (pred(X::in,Y::in,Z::out) is det :-
		   compare(R,X,Y),
		   (if R = (<) then Z = X else Z = Y)),
	      Initial, Min).

:- module aggregates.floats.
:- implementation.
:- import_module aggregates, std_util.
:- use_module math, string.
bag_min(Predicate, Min) :-
    count_max_2(Predicate, N2),
    bag_min(Predicate, max, Min1),
    (if N2=0 then Min=string.det_to_float("NaN") else Min=Min1).
bag_max(Predicate, Max) :- 
    count_max_2(Predicate, N2),
    bag_max(Predicate, min, Max1),
    (if N2=0 then Max=string.det_to_float("NaN") else Max=Max1).
bag_sum(Predicate, Sum) :-
    bag_aggr(Predicate,
	     (pred(X::in,Y::in,Z::out) is det :- Z = X+Y),
	     0.0,
	     Sum).
bag_summary(Predicate, {Min,Q1,Median,Mean,Q3,Max}) :-
    bag_min(Predicate,Min),
    bag_quantile(Predicate,0.25,Q1),
    bag_median(Predicate,Median),
    bag_avg(Predicate,Mean),
    bag_quantile(Predicate,0.75,Q3),
    bag_max(Predicate,Max).
:- pred bag_wtd_sum(pred(float)::(pred(out) is nondet), (func(float)=float)::in,
		     {int,float}::out) is det. 
bag_wtd_sum(Predicate, F, {N,WtdSum}) :-
    bag_aggr(Predicate,
	     (pred(X::in,{N0,Sum0}::in,{N1,Sum1}::out) is det :-
		  N1 = N0+1,
		  Sum1 = Sum0 + F(X)),
	     {0, 0.0},
	     {N, WtdSum}).
bag_avg(Predicate, Avg) :-
    bag_wtd_sum(Predicate, id, {N, Sum}),
    (if N < 1 then Avg = string.det_to_float("NaN") else Avg = Sum/float(N)).
bag_mean(Predicate,Mean) :- bag_avg(Predicate,Mean).
bag_geom_mean(Predicate,Avg) :-
    bag_wtd_sum(Predicate, math.ln, {N, SumLn}),
    (if N < 1 then Avg = string.det_to_float("NaN") else Avg = math.exp(SumLn/float(N))).
bag_var(Predicate, Var) :-
    bag_wtd_sum(Predicate, id, {N, Sum}),
    (N < 2 -> Var = string.det_to_float("NaN") ;
     Avg = Sum/float(N),
     bag_wtd_sum(Predicate,
		 (func(X) = (X-Avg)*(X-Avg)),
		 {_,SumSqDiff}),
     Var = SumSqDiff/float(N-1)).
bag_median(Predicate, Result) :-
    promise_equivalent_solutions[UnsortedList] (
	unsorted_solutions(Predicate,UnsortedList)),
    sort(UnsortedList,List),
    length(List,Length),
    (Length = 0 -> Result = string.det_to_float("NaN") ;
     I = Length // 2,
     (Length mod 2 = 1 -> Result = det_index1(List,I+1) ;
      Result = (det_index1(List,I)+det_index1(List,I+1))/2.0)).
bag_quantile(Predicate, Quantile, Result) :-
    promise_equivalent_solutions[UnsortedList] (
	unsorted_solutions(Predicate,UnsortedList)),
    sort(UnsortedList,List),
    length(List,Length),
    (Length = 0 -> Result = string.det_to_float("NaN") ;
     Index = float(Length-1)*Quantile,
     Low = floor_to_int(Index),
     High = ceiling_to_int(Index),
     Lambda = Index - float(Low),
     Result = (1.0-Lambda)*det_index0(List,Low)+Lambda*det_index0(List,High)).

set_min(Predicate, Min) :-
    count_max_2(Predicate, N2),
    set_min(Predicate, max, Min1),
    (if N2=0 then Min=string.det_to_float("NaN") else Min=Min1).
set_max(Predicate, Max) :- 
    count_max_2(Predicate, N2),
    set_max(Predicate, min, Max1),
    (if N2=0 then Max=string.det_to_float("NaN") else Max=Max1).
set_sum(Predicate, Sum) :-
    aggregate(Predicate,
	      (pred(X::in,Y::in,Z::out) is det :- Z = X+Y),
	      0.0,
		Sum).
set_summary(Predicate, {Min,Q1,Median,Mean,Q3,Max}) :-
    bag_min(Predicate,Min),
    set_quantile(Predicate,0.25,Q1),
    set_median(Predicate,Median),
    set_avg(Predicate,Mean),
    set_quantile(Predicate,0.75,Q3),
    bag_max(Predicate,Max).
:- pred set_wtd_sum(pred(float)::(pred(out) is nondet), (func(float)=float)::in,
		    {int,float}::out) is det. 
set_wtd_sum(Predicate, F, {N,WtdSum}) :-
    aggregate(Predicate,
	      (pred(X::in,{N0,Sum0}::in,{N1,Sum1}::out) is det :-
		   N1 = N0+1,
		   Sum1 = Sum0 + F(X)),
	      {0, 0.0},
	      {N, WtdSum}).
set_avg(Predicate, Avg) :-
    set_wtd_sum(Predicate, id, {N, Sum}),
    (if N < 1 then Avg = string.det_to_float("NaN") else Avg = Sum/float(N)).
set_mean(Predicate,Mean) :- set_avg(Predicate,Mean).
set_geom_mean(Predicate,Avg) :-
    set_wtd_sum(Predicate, math.ln, {N, SumLn}),
    (if N < 1 then Avg = string.det_to_float("NaN") else Avg = math.exp(SumLn/float(N))).
set_var(Predicate, Var) :-
    set_wtd_sum(Predicate, id, {N, Sum}),
    (N < 2 -> Var = string.det_to_float("NaN") ;
     Avg = Sum/float(N),
     set_wtd_sum(Predicate,
		 (func(X) = (X-Avg)*(X-Avg)),
		 {_,SumSqDiff}),
     Var = SumSqDiff/float(N-1)).
set_median(Predicate, Result) :-
    solutions(Predicate,List),
    length(List,Length),
    (Length = 0 -> Result = string.det_to_float("NaN") ;
     I = Length // 2,
     (Length mod 2 = 1 -> Result = det_index1(List,I+1) ;
      Result = (det_index1(List,I)+det_index1(List,I+1))/2.0)).
set_quantile(Predicate, Quantile, Result) :-
    solutions(Predicate,List),
    length(List,Length),
    (Length = 0 -> Result = string.det_to_float("NaN") ;
     Index = float(Length-1)*Quantile,
     Low = floor_to_int(Index),
     High = ceiling_to_int(Index),
     Lambda = Index - float(Low),
     Result = (1.0-Lambda)*det_index0(List,Low)+Lambda*det_index0(List,High)).

bag_cum_sum(Predicate,CumSums) :-
    Pred = (pred({By,X}::out) is nondet :- Predicate(By,X)),
    promise_equivalent_solutions[List] (
	unsorted_solutions(Pred, List)),
    sort((pred({A,_}::in, {B,_}::in, Result::out) is det :- Result = ordering(A,B)),
	 List, SortedList),
    foldl((pred({By,X}::in,{CumSum0,List0}::in,{CumSum1,List1}::out) is det :-
	       CumSum1 = CumSum0+X, List1 = [{By,CumSum1}|List0]),
	  SortedList,
	  {0.0, []},
	  {_, CumSumList}),
    CumSums = (pred(By::out,CumSumi::out) is nondet :- member({By,CumSumi},CumSumList)).
bag_cum_sum(Predicate) = Result :- bag_cum_sum(Predicate,Result).
bag_row_number(By,RowNumbers) :-
    promise_equivalent_solutions[UnsortedList] (
	unsorted_solutions(By,UnsortedList)), % bag semantics
    sort(UnsortedList,List1),                 % sorted
    foldl((pred(Byi::in,{I,ListIn}::in,Y::out) is det :- Y = {I+1,[{Byi,I+1}|ListIn]}),
	  List1, {0,[]}, {_,ListOut}),
    RowNumbers = (pred(Byi::out,Rowi::out) is nondet :- member({Byi,Rowi},ListOut)).
bag_row_number(Predicate) = Result :- bag_row_number(Predicate,Result).

:- end_module aggregates.floats.

:- module aggregates.ints.
:- implementation.
:- import_module aggregates.
bag_min(Predicate, Min) :- bag_min(Predicate, max_int, Min).
bag_max(Predicate, Max) :- bag_max(Predicate, min_int, Max).
bag_sum(Predicate, Sum) :-
    bag_aggr(Predicate,
	     (pred(X::in,Y::in,Z::out) is det :- Z = X+Y),
	     0,
	     Sum).
set_min(Predicate, Min) :- set_min(Predicate, max_int, Min).
set_max(Predicate, Max) :- set_max(Predicate, min_int, Max).
set_sum(Predicate, Sum) :-
    aggregate(Predicate,
	      (pred(X::in,Y::in,Z::out) is det :- Z = X+Y),
	      0,
	      Sum).
:- end_module aggregates.ints.

:- end_module aggregates.
