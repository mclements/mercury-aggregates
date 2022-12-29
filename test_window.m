:- module test_window.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, int, float, string, maybe, solutions.

:- type maybe_date ---> date(year::int, month::int, day::int); no.

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
    print_line("{Id, RowNumber, Date, Score, CumScore}", !IO),
    aggregate((pred({Id,RowNumber,Datei,Scorei,CumScorei}::out) is nondet :-
	           patient(Id,_), 
	           Combined = (pred(Date::out,Score::out) is nondet :- visit(Id,Date,Score)), 
		   Combined(Datei,Scorei),
	  	   bag_cum_sum(Combined)(Datei,CumScorei),
		   Dates = (pred(Date::out) is nondet :- Combined(Date,_)),
		   bag_row_number(Dates)(Datei,RowNumber)),
	      print_line,
	      !IO).

:- func bag_cum_sum(pred(T,float)::in(pred(out,out) is nondet)) =
   (pred(T,float)::out(pred(out,out) is nondet)) is det.
:- func bag_row_number(pred(T)::in(pred(out) is nondet)) = 
   (pred(T,int)::out(pred(out,out) is nondet)) is det.
bag_cum_sum(Predicate) = CumSums :-
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
bag_row_number(By) = RowNumbers :-
    promise_equivalent_solutions[UnsortedList] (
	unsorted_solutions(By,UnsortedList)), % bag semantics
    sort(UnsortedList,List1),                 % sorted
    foldl((pred(Byi::in,{I,ListIn}::in,Y::out) is det :- Y = {I+1,[{Byi,I+1}|ListIn]}),
	  List1, {0,[]}, {_,ListOut}),
    RowNumbers = (pred(Byi::out,Rowi::out) is nondet :- member({Byi,Rowi},ListOut)).
