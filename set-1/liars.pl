:- set_flag(print_depth, 1000).


%%Ypologizei ton arithmo pseutwn se mia lista apo 0 kai 1 (1 gia tous pseftes kai 0 gia tous mh pseftes)
liars_number([],V,V).
liars_number([A|L],N1,N):- (A == 0), liars_number(L,N1,N),!.
liars_number([A|L],C,N):- N1 is C + 1, liars_number(L,N1,N).

%%Elegxei me vash enan sugkekrimeno elaxisto arithmo pseftwn, an mia lista einai swsth.
check_list(N,[],[]).
check_list(N,[A|B],[H|T]):- (H == 1), (A > N), check_list(N,B,T).
check_list(N,[A|B],[H|T]):- (H == 0), (A =< N), check_list(N,B,T).

%%Ftiaxnei mia lista symfwna me dedomeno elaxisto arithmo pseftwn
make_list(N,[],[]).
make_list(N,[A|B],[H|T]):- (A > N), H is 1, make_list(N,B,T).
make_list(N,[A|B],[H|T]):- (A =< N), H is 0, make_list(N,B,T).

%%Metraei to megethos mias listas
my_length([],0).
my_length([A|B],L):- my_length(B,L1), L is L1+1.

%%Ftiaxnei listes me elaxisto arithmo pseftwn apo 0 ews N, opou N to megethos ths listas pou dinetai 
%%kai epistrefei thn swsth an uparxei
foo(A,L,J,R):- make_list(J,A,R), liars_number(R,0,X), check_list(X,A,R).
foo(A,L,J,R):- J1 is J + 1, J1 =< L, foo(A,L,J1,R).

liars(A,R):- my_length(A,L), foo(A,L,0,R).


