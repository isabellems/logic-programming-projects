:- set_flag(print_depth,1000).
:- lib(ic).
:- lib(branch_and_bound).

tents(RowTents, ColumnTents, Trees, Tents) :- length(RowTents, R),
										 length(ColumnTents, C),
										 def_vars(R, C, Bin),
										 constrain(RowTents, ColumnTents, R, C, Trees, Bin),
										 Cost #= sum(Bin),
                                     	 bb_min(search(Bin,0,input_order,indomain,complete,[]),Cost,bb_options{strategy:continue}),
                                     	 def_vars(R, C, Bin1),
										 constrain(RowTents, ColumnTents, R, C, Trees, Bin1),
										 Cost #= sum(Bin1),
                                     	 search(Bin1,0,input_order,indomain,complete,[]),
                                     	 binary_tran(Bin1, Tents, 1, 1, R, C).

def_vars(R, C, Tents) :- Len is R*C,
						 length(Tents,Len),
						 Tents #:: [0,1].

constrain(RowTents, ColumnTents, R, C, Trees, Tents) :- trees_position(Trees,Tents, 1, 1, R, C),
												 rows_list(0, R, C, Tents, SumListR), 
												 collumns_list(0, C, Tents, SumListC),
												 check_rows(RowTents, SumListR),
												 check_columns(ColumnTents, SumListC),
												 tree_tent_c(Trees, R, C, Tents),
												 tents_alone(Tents, Tents, 1, 1, R, C).

%%Periorismos na uparxoun oses tentes se mia grammh kathorizontai apo thn lista me tis megistes times pou dinetai
check_rows([], _).
check_rows([H1|Rows], [_|SumList]) :- H1 #<0 , check_rows(Rows, SumList),!.
check_rows([H1|Rows], [H2|SumList]) :- sum(H2) #=< H1, check_rows(Rows, SumList),!.

%%Periorismos na uparxoun oses tentes se mia sthlh kathorizontai apo thn lista me tis megistes times pou dinetai
check_columns([], _).
check_columns([H1|Columns], [_|SumList]) :- H1 #<0 , check_columns(Columns, SumList),!.
check_columns([H1|Columns], [H2|SumList]) :- sum(H2) #=< H1, check_columns(Columns, SumList),!.

%%Ftiaxnei lista apo tis listes twn tentwn se kathe grammh
rows_list(Rows, Rows, _, _, []).
rows_list(P, Rows, C, Tents, [S|L]) :- row(Tents, 0, C, NewTents, S) , P1 is P + 1,
										   rows_list(P1, Rows, C, NewTents, L),!.

%%Ftiaxnei lista apo tis listes twn tentwn se kathe sthlh
collumns_list(Columns, Columns, _, []).
collumns_list(P, Columns, Tents, [S|L]) :- column(Tents, 0, P, Columns, S), P1 is P + 1,
												 collumns_list(P1, Columns, Tents, L),!.

%%Epistrefei lista me tis tentes se mia grammh
row(Tents, P, NumR, Tents, []) :- P  #= NumR.
row([R|Tents], P, NumR, NewTents, [R|L]) :- P1 is P + 1, row(Tents, P1, NumR, NewTents, L),!.

%%Epistrefei lista me tis tentes se mia sthlh
column([], _, _, _,  []).
column([C|Tents], P, NumC, Col, [C|L]) :- P #= NumC, NumC1 is NumC + Col, P1 is P + 1, column(Tents, P1, NumC1, Col, L),!.
column([_|Tents], P, NumC, Col, L) :- P1 is P + 1, column(Tents, P1, NumC, Col, L),!.

%%Periorismos gia to na mhn uparxei tenta se idia thesh me dentro.
trees_position([],_,_,_,_,_).
trees_position(_,[],_,_,_,_).
trees_position([R-C|Trees], [_|Tents], Row, Col, Rs, Cs) :- ((R #\= Row) ; (C #\= Col)), Col #\= Cs,
															  Col1 is Col + 1,
															  trees_position([R-C|Trees], Tents, Row, Col1, Rs, Cs),!.
trees_position([R-C|Trees], [_|Tents], Row, Col, Rs, Cs) :- ((R #\= Row) ; (C #\= Col)), Col #= Cs,
															  Row1 is Row + 1,
															  trees_position([R-C|Trees], Tents, Row1, 1, Rs, Cs),!.
trees_position([R-C|Trees], [T2|Tents], Row, Col, Rs, Cs) :- R #= Row, C #= Col, T2 #= 0, Col #= Cs,
															  Row1 is Row + 1,
															  trees_position(Trees, Tents, Row1, 1, Rs, Cs),!.
trees_position([R-C|Trees], [T2|Tents], Row, Col, Rs, Cs) :-  R #= Row, C #= Col, T2 #= 0, Col #\= Cs,
															  Col1 is Col + 1,
															  trees_position(Trees, Tents, Row, Col1, Rs, Cs),!.

%%Periorismos na uparxei tenta se mia toulaxiston geitonikh thesh enos dentrou
tree_tent_c([],_,_,_).
tree_tent_c([R-C|Trees], Rs, Cs, Tents) :- tree_c(R-C, Tents, 1, 1, Rs, Cs, L), sum(L) #>= 1, tree_tent_c(Trees, Rs, Cs, Tents),!.

%%Epistrefei lista me tis geitonikes theseis enos dentrou
tree_c(R-C, _, Row, Col, Rs, _, []) :- (Row #> Rs) ; (Row #= (R+1), Col #= (C + 2)).
tree_c(R-C, [T|Tents], Row, Col , Rs, Cs, [T|L]) :- Col #\= Cs, ((Row #= (R - 1) ; Row #= (R + 1)), (Col #= (C - 1) ; Col #= C ; Col #= (C + 1))), Col1 is Col +1,
													tree_c(R-C, Tents, Row, Col1, Rs, Cs, L), !.
tree_c(R-C, [T|Tents], Row, Col , Rs, Cs, [T|L]) :- Col #= Cs, ((Row #= (R - 1) ; Row #= (R + 1)), (Col #= (C - 1) ; Col #= C ; Col #= (C + 1))), Row1 is Row +1,
													tree_c(R-C, Tents, Row1, 1, Rs, Cs, L), !.
tree_c(R-C, [T|Tents], Row, Col , Rs, Cs, [T|L]) :- Col #\= Cs, ((Row #= R) , (Col #= (C - 1) ; Col #= (C + 1))), Col1 is Col +1,
													 tree_c(R-C, Tents, Row, Col1, Rs, Cs, L), !.
tree_c(R-C, [T|Tents], Row, Col , Rs, Cs, [T|L]) :- Col #= Cs, (Row #= R, (Col #= (C - 1) ; Col #= (C + 1))), Row1 is Row +1,
													 tree_c(R-C, Tents, Row1, 1, Rs, Cs, L), !.
tree_c(R-C, [_|Tents], Row, Col , Rs, Cs, L) :- Col #\= Cs, Col1 is Col +1, tree_c(R-C, Tents, Row, Col1, Rs, Cs, L),!.
tree_c(R-C, [_|Tents], Row, Col , Rs, Cs, L) :- Col #= Cs, Row1 is Row +1, tree_c(R-C, Tents, Row1, 1, Rs, Cs, L),!.

%%Periorismos na mhn uparxoun alles tentes se geitonikes theseis mias tentas
tents_alone([], _, _, _, _, _).
tents_alone([T|Tents], Tents1, Row, Col, Rs, Cs) :-  Col #\= Cs, tents_a(T, Tents1, Row, Col, 1, 1, Rs, Cs, S), T #=1 => sum(S) #= 1, Col1 is Col + 1,
											 		tents_alone(Tents, Tents1, Row, Col1, Rs, Cs),!.
tents_alone([T|Tents], Tents1, Row, Col, Rs, Cs) :- Col #= Cs, tents_a(T, Tents1, Row, Col, 1, 1, Rs, Cs, S),  T #=1 => sum(S) #= 1, Row1 is Row + 1,
											 		tents_alone(Tents, Tents1, Row1, 1, Rs, Cs),!.

%%Epistrefei lista me thn tenta kai ths geitonikes theseis ths
tents_a(Tent, _, R, C, Row, Col, Rs, _, [Tent]) :- (Row #> Rs) ; (Row #= (R + 1), Col #= (C + 2)).
tents_a(Tent, [T|Tents], R, C, Row, Col, Rs, Cs, [T|L]) :- Col #\= Cs, ((Row #= (R - 1) ; Row #= (R + 1)), (Col #= (C - 1) ; Col #= C ; Col #= (C + 1))), Col1 is Col +1,
													 tents_a(Tent, Tents, R, C, Row, Col1, Rs, Cs, L), !.
tents_a(Tent, [T|Tents], R, C, Row, Col, Rs ,Cs, [T|L]) :- Col #= Cs, ((Row #= (R - 1) ; Row #= (R + 1)), (Col #= (C - 1) ; Col #= C ; Col #= (C + 1))), Row1 is Row +1,
													 tents_a(Tent, Tents, R, C, Row1, 1, Rs, Cs, L), !.
tents_a(Tent, [T|Tents], R, C, Row, Col, Rs ,Cs, [T|L]) :- Col #\= Cs, ((Row #= R) , (Col #= (C - 1) ; Col #= (C + 1))), Col1 is Col +1,
													 tents_a(Tent, Tents, R, C, Row, Col1, Rs, Cs, L), !.
tents_a(Tent, [T|Tents], R, C, Row, Col ,Rs ,Cs, [T|L]) :- Col #= Cs, (Row #= R, (Col #= (C - 1) ; Col #= (C + 1))), Row1 is Row +1,
													 tents_a(Tent, Tents, R, C, Row1, 1, Rs, Cs, L),!.
tents_a(Tent, [_|Tents], R, C, Row, Col, Rs ,Cs, L) :- Col #\= Cs, Col1 is Col +1, tents_a(Tent, Tents, R, C, Row, Col1, Rs, Cs, L),!.
tents_a(Tent, [_|Tents], R, C, Row, Col ,Rs ,Cs, L) :- Col #= Cs, Row1 is Row +1, tents_a(Tent, Tents, R, C, Row1, 1, Rs, Cs, L),!.

%%Metatrepei thn lista apo dyadikh morfh se dekadikh
binary_tran([], [], _, _, _, _).
binary_tran([B|Bin], [Row-Col|Tents], Row, Col, Rs, Cs) :- B #= 1, Col #\= Cs, Col1 is Col + 1, binary_tran(Bin, Tents, Row, Col1, Rs, Cs),!.
binary_tran([B|Bin], [Row-Col|Tents], Row, Col, Rs, Cs) :- B #= 1, Col #= Cs, Row1 is Row + 1, binary_tran(Bin, Tents, Row1, 1, Rs, Cs),!.
binary_tran([B|Bin], Tents, Row, Col, Rs, Cs) :- B #= 0, Col #\= Cs, Col1 is Col + 1, binary_tran(Bin, Tents, Row, Col1, Rs, Cs),!.
binary_tran([B|Bin], Tents, Row, Col, Rs, Cs) :- B #= 0, Col #= Cs, Row1 is Row + 1, binary_tran(Bin, Tents, Row1, 1, Rs, Cs),!.
