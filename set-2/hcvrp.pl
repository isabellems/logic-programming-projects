:- set_flag(print_depth,1000).
:- lib(ic).
:- lib(ic_global).
:- lib(branch_and_bound).


vehicles([35, 40, 55, 15, 45, 25, 85, 55]).

clients([c(15,  77,  97), c(23, -28,  64), c(14,  77, -39),
         c(13,  32,  33), c(18,  32,   8), c(18, -42,  92),
         c(19,  -8,  -3), c(10,   7,  14), c(18,  82, -17),
         c(20, -48, -13), c(15,  53,  82), c(19,  39, -27),
         c(17, -48, -13), c(12,  53,  82), c(11,  39, -27),
         c(15, -48, -13), c(25,  53,  82), c(14, -39,   7),
         c(22,  17,   8), c(23, -38,  -7)]).


hcvrp(NCl, NVe, Timeout, Solution, Cost, Time) :- cputime(T1),
												  clients(Cl), vehicles(Vl), firstNClients(NCl, 0, Cl, DList, CList), firstNVehicles(NVe, 0, Vl, VList),
												  distances([(0,0)|CList],CList,Dist),
												  def_vars(FList, NCl, NVe),
												  constrain(FList, NCl, DList, VList),
												  cost(Dist,FList,NCl, CostList),flatten(CostList, FCostL), Cost #= sum(FCostL),
												  bb_min(search(FList,0,input_order,indomain,complete,[]),Cost,bb_options{timeout:Timeout}),
												  finalList(FList, NCl, Solution),
												  cputime(T2), Time is (T2-T1).

%% H lista tou apotelesmatos arxika tha exei to megisto dunato mhkos, dhladh oxhmata epi pelates
def_vars(FList, NCl, NVe) :-  Len is NCl*NVe, length(FList, Len), FList #:: [0..NCl].

%%Oi periorismoi einai na emfanizetai kathe pelaths mia akrivws fora sthn eniaia lista kai kathe forthgo na exei fortio mikrotero h iso tou capacity tou
constrain(FList, NCl, Orders, Capacities) :- countOCC(FList, NCl, 1), capacities(FList, Orders, Capacities, NCl), !.

%%Epistrefei mia lista me tis paraggelies twn N prwtwn pelatwn kai mia lista me tis suntetagmenes tous
firstNClients(NCl, NCl, _, [], []).
firstNClients(NCl, Count, [c(D,X,Y)|Cl], [D|DList] , [(X,Y)|CList]) :- C1 is Count + 1, firstNClients(NCl, C1, Cl, DList, CList) ,!.

%%Epistrefei ta capacities twn N prwtwn oxhmatwn
firstNVehicles(NVe, NVe, _, []).
firstNVehicles(NVe, Count, [H|Ve], [H|List]) :- C1 is Count + 1, firstNVehicles(NVe, C1, Ve, List) ,!.

%%Ypologizei tis apostaseis metaksi kathe zevgous pelatwn + tis apothikis sxhmatizontas mia lista listwn (N+1)(N+1)
distances([], _, []).
distances([(X,Y)|List], Clients, [L|DList]) :- clientDist((X,Y), Clients, L) , distances(List, Clients, DList),!.

%%Ypologizei lista me tis apostaseis metaksi enos pelath kai olwn twn upoloipwn (+ths apothikis)
clientDist((X,Y), [], [N|[]]) :- N is (X*X + Y*Y), N #=0,!.
clientDist((X,Y), [], [Int|[]]) :- N is (X*X + Y*Y), R is sqrt(N), D is R*1000, Round is round(D), Int is integer(Round),!.
clientDist((X1,Y1), [(X2,Y2)|Clients], [N|List]) :- N is ((X1-X2)*(X1-X2) + (Y1-Y2)*(Y1-Y2)), N #= 0, clientDist((X1,Y1), Clients, List),!. 
clientDist((X1,Y1), [(X2,Y2)|Clients], [Int|List]) :- N is ((X1-X2)*(X1-X2) + (Y1-Y2)*(Y1-Y2)), R is sqrt(N), D is R * 1000, Round is round(D), Int is integer(Round),
													 	clientDist((X1,Y1), Clients, List),!. 

%%Periorismos kathe pelaths na vrisketai mia fora akrivws sthn eniaia lista
countOCC(FList,NCl,NCl) :- occurrences(NCl, FList, 1),!.
countOCC(FList, NCl, Counter) :- occurrences(Counter, FList, 1), Counter1 is (Counter + 1), countOCC(FList, NCl, Counter1),!. 

%%Periorismos to fortio enos forthgou na einai mikrotero h iso tou capacity tou
capacities(_,_,[],_).
capacities([], _, _, _).
capacities(FList, Orders, [C|Caps], NCl) :- makelist(FList, Remain, List, NCl, 1), vehCapacity(List, Orders, Weight), 
		    								C #>= sum(Weight), capacities(Remain, Orders, Caps, NCl),!.

%%Epistrefei lista me tis paraggelies tou kathe forthgou
vehCapacity([], _, []).
vehCapacity([H|List], Orders, [H|Cap]) :- (H#=0), vehCapacity(List,Orders,Cap),!. 
vehCapacity([H|List], Orders, [X|Cap]) :- element(H, Orders, X), vehCapacity(List, Orders, Cap),!.

%%Epistrefei tmhma listas me mhkos NCl
makelist([H|FList], FList, [H|[]], NCl, NCl).
makelist([H|FList], Remain, [H|List], NCl, Counter) :- Counter1 is Counter + 1, makelist(FList, Remain, List, NCl, Counter1),!.

%%Epistrefei lista me ta kosth kathe forthgou
cost(_, [], _, []).
cost(Distances, List, NCl, [C|Costs]) :- makelist(List, Remain, New, NCl, 1), costEach(Distances, New, C, NCl, 1, 0), cost(Distances, Remain, NCl, Costs),!. 

%%Ypologizei to kostos enos forthgou
costEach(Distances, [H1], [C,C], N, 1 , Prev) :- returnLine(Distances, Line, 0, 1), myelement(H1, Line, C, 1),!.
costEach(Distances, [H1], [C1,C2], N, _, Prev) :-  (Prev #> 0), returnLine(Distances, Line, 0, 1), myelement(H1, Line, C1, 1), returnLine(Distances, Line1, H1, 1), myelement(Prev, Line1, C2, 1), !.
costEach(Distances, [H1], [C1], N, _, Prev) :-  returnLine(Distances, Line, 0, 1), myelement(H1, Line, C1, 1), !.
costEach(Distances, [H1|List], [C1|Costs], N, 1, Prev) :- (H1#>0), returnLine(Distances, Line, 0, 1), myelement(H1, Line, C1, 1), Counter1 is 2, costEach(Distances, List, Costs, N, 2, H1),!.
costEach(Distances, [H1|List], [C1|Costs], N, 1, Prev) :- C1 is 0, costEach(Distances, List, Costs, N, 2, Prev),!.
costEach(Distances, [H1|List], [C1|Costs], N, Counter, Prev) :- (H1#>0), (Prev #> 0), returnLine(Distances, Line, H1, 1), myelement(Prev, Line, C1, 1), Counter1 is Counter+1, costEach(Distances, List, Costs, N, Counter1, H1),!.
costEach(Distances, [H1|List], [C1|Costs], N, Counter, Prev) :- (H1#>0), C1 is 0, Counter1 is Counter+1, costEach(Distances, List, Costs, N, Counter1, H1),!.
costEach(Distances, [H1|List], [C1|Costs], N, Counter, Prev) :- C1 is 0, Counter1 is Counter+1, costEach(Distances, List, Costs, N, Counter1, Prev),!.

%%Paromoia sunarthsh me thn element pou an dexthei 0 ws index epistrefei 0 kai oxi No. (Htan aparaithth se prohgoumenh dokimastikh ulopoihsh)
myelement(_, [], 0, _).
myelement(Index, _, 0, _) :- Index #=< 0,!.
myelement(Index, [H|Counter], H, Index).
myelement(Index, [H|Counter], Value, C) :- C1 is C + 1, myelement(Index, Counter, Value, C1),!.
 
%%Epistrefei sugkekrimenh lista apo lista listwn
returnLine([], [], _, _). 
returnLine([H|Distances], H, Counter, Counter1) :- Counter1 #= (Counter+1),!.
returnLine([H|Distances], Line, Num, Counter) :- Counter1 is Counter + 1, returnLine(Distances, Line, Num, Counter1),!.

%%Metatrepei thn eniaia lista se lista listwn agnnoontas ta 0 kai sxhmatizontas to teliko apotelesma.
finalList([], _, []).
finalList(List, NCl, [F|Final]) :- subList(List, Remain, F, NCl, 1), finalList(Remain, NCl, Final),!.

%%Ftiaxnei thn kathe upolista ths listas tou telikou apotelesmatos
subList([H|List], List, [], NCl, NCl) :- H #= 0.
subList([H|List], List, [H|[]] , NCl, NCl).
subList([H|List], Remain, S, NCl, Counter) :- H #= 0, Counter1 is Counter + 1, subList(List, Remain, S, NCl, Counter1),!.
subList([H|List], Remain, [H|S], NCl, Counter) :- Counter1 is Counter + 1, subList(List, Remain, S, NCl, Counter1),!.

