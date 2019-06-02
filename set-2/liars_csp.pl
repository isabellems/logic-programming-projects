:- set_flag(print_depth,1000).
:- lib(ic).

liars_csp(A,L) :- length(A,LenA),
				  def_vars(LenA,L,Sum),
				  Sum #= sum(L),
				  constrain(A,L,Sum),
                  search(L,0,input_order,indomain,complete,[]).



def_vars(A,L,Sum) :- length(L,A),
      				 	 L #:: [0,1],
      				 	 Sum #=< A.


constrain([],[],Sum).
constrain([A|T],[H|L],Sum) :- H #= (A #> Sum) , constrain(T,L,Sum).

genrand(N, List) :-
   length(List, N),
   make_list(N, List).
make_list(_, []).
make_list(N, [X|List]) :-
   random(R),
   X is R mod (N+1),
   make_list(N, List).
