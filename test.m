:- module test.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, int, float, string, maybe, solutions.
:- import_module aggregates, aggregates.floats.

:- type maybe_date ---> date(year::int, month::int, day::int); no.
:- pred bag_max_date(pred(maybe_date)::(pred(out) is nondet), maybe_date::out) is det.
bag_max_date(Predicate, MaxDate) :-
    bag_max(Predicate, date(min_int,0,0), MaxDate1),
    (if MaxDate1 = date(min_int,0,0) then MaxDate = no else MaxDate = MaxDate1).

:- func nan = float.
nan = det_to_float("NaN").

:- pred patient(int::out, string::out) is multi.
patient(1001, "Hopper").
patient(4004, "Wirth").
patient(3003, "Kemeny").
patient(2002, "Gosling").
patient(5005, "Kurtz").

:- pred visit(int::out, maybe_date::out, float::out) is multi.
visit(2002, date(2020,09,10), 6.8).
visit(1001, date(2020,09,17), 5.5).
visit(4004, date(2020,09,24), 8.4).
visit(2002, date(2020,10,08), nan).
visit(1001, no, 6.6).
visit(3003, date(2020,11,12), nan).
visit(4004, date(2020,11,05), 7.0).
visit(1001, date(2020,11,19), 5.3).

main(!IO) :-
    print_line("{Id, Lastname, SumScores, AvgScores, MaxDate}", !IO),
    aggregate((pred({Id,Lastname,Sum,Avg,MaxDate}::out) is nondet :-
	           patient(Id,Lastname),
	           Scores = (pred(Score::out) is nondet :- visit(Id,_,Score), \+is_nan(Score)),
	  	   bag_sum(Scores, Sum),
	           bag_avg(Scores, Avg),
	           Dates = (pred(Date::out) is nondet :- visit(Id,Date,_), Date\=no),
	           bag_max_date(Dates, MaxDate)),
	      print_line,
	      !IO),
    AllScores = (pred(Score::out) is nondet :- visit(_,_,Score), \+is_nan(Score)),
    bag_var(AllScores,Var),
    bag_geom_mean(AllScores,Gmean),
    bag_summary(AllScores,Summary),
    print_line(Var,!IO),
    print_line(Gmean,!IO),
    print_line(Summary,!IO),
    AllScores2 = (pred(Score::out) is nondet :- visit(_,_,Score), \+is_nan(Score), Score>5.31),
    bag_median(AllScores2,Median2),
    print_line(Median2,!IO),
    AllScores3 = (pred(Score::out) is nondet :- visit(_,_,Score)),
    bag_avg(AllScores3,Avg3),
    print_line(Avg3,!IO),
    AllScores4 = (pred(Score::out) is nondet :- visit(_,_,Score), \+is_nan(Score), Score>100.0),
    bag_summary(AllScores4, Summary4),
    print_line(Summary4, !IO),
    print_line("{Id, RowNumber, Date, Score, CumScore}", !IO),
    aggregate((pred({Id,RowNumber,Datei,Scorei,CumSumi}::out) is nondet :-
	           patient(Id,_),
	           Combined = (pred(Date::out,Score::out) is nondet :- visit(Id,Date,Score)), 
		   Combined(Datei,Scorei),
	  	   bag_cum_sum(Combined)(Datei,CumSumi),
		   Dates = (pred(Date::out) is nondet :- Combined(Date,_)),
		   bag_row_number(Dates)(Datei,RowNumber)),
	      print_line,
	      !IO).

%% %% Combining two predicates:
%% :- pred combine_pred(pred(T1)::in(pred(out) is nondet),
%% 		     pred(T2)::in(pred(out) is nondet),
%% 		     pred({T1,T2})::out(pred(out) is nondet)).
%% combine_pred(P1, P2, P3) :-
%%     promise_equivalent_solutions[List1] (
%% 	unsorted_solutions(P1, List1)),
%%     promise_equivalent_solutions[List2] (
%% 	unsorted_solutions(P2, List2)),
%%     map_corresponding(pred(A::in,B::in,C::out) is det :- C = {A,B}, List1, List2, List3),
%%     P3 = (pred(Z::out) is nondet :- member(Z,List3)).

%% %% Sort a predicate
%% :- pred bag_sort_by_pred(pred(T1,T2)::in(pred(out,out) is nondet),
%% 			 pred(T1,T2)::out(pred(out,out) is nondet)).
%% bag_sort_by_pred(Predicate, Pout) :-
%%     Pred = (pred({By,X}::out) is nondet :- Predicate(By,X)),
%%     promise_equivalent_solutions[List1] (
%% 	unsorted_solutions(Pred, List1)),
%%     sort((pred({A,_}::in, {B,_}::in, Result::out) is det :- Result = ordering(A,B)),
%% 	 List1, SortedList),
%%     Pout = (pred(Byi::out,Xi::out) is nondet :- member({Byi,Xi},SortedList)).

%% :- pred mapi(pred(T1,int,T2)::in(pred(in,in,out) is det),
%% 	     list(T1)::in,
%% 	     list(T2)::out) is det.
%% mapi(P, L, R) :-
%%     Index = 0..(length(L)-1),
%%     map_corresponding(P, L, Index, R).

:- end_module test.

%% Output:

%% {Id, Lastname, SumScores, AvgScores, MaxDate}:
%% {1001, "Hopper", 17.4, 5.8, date(2020, 11, 19)}
%% {2002, "Gosling", 6.8, 6.8, date(2020, 10, 8)}
%% {3003, "Kemeny", 0.0, nan, date(2020, 11, 12)}
%% {4004, "Wirth", 15.4, 7.7, date(2020, 11, 5)}
%% {5005, "Kurtz", 0.0, nan, no}
%% 1.2680000000000002
%% 6.52145931025626
%% {5.3, 5.775, 6.699999999999999, 6.6000000000000005, 6.95, 8.4}
%% 6.8
%% nan
%% {nan, nan, nan, nan, nan, nan}

%% %% R code for the test statistics
%% > var(c(6.8,5.5,8.4,6.6,7,5.3))
%% [1] 1.268
%% > exp(mean(log(c(6.8,5.5,8.4,6.6,7,5.3))))
%% [1] 6.521459
%% > summary(c(6.8,5.5,8.4,6.6,7,5.3))
%%    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
%%   5.300   5.775   6.700   6.600   6.950   8.400 
%% > median(c(6.8,5.5,8.4,6.6,7))
%% [1] 6.8
%% > mean(c(6.8,5.5,8.4,6.6,7,5.3,NA))
%% [1] NA
