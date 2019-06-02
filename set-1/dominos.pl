:- set_flag(print_depth, 1000).

dominos([(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6), 
		 	   (1,1),(1,2),(1,3),(1,4),(1,5),(1,6), 
		 	   		 (2,2),(2,3),(2,4),(2,5),(2,6), 
		 	   		 	   (3,3),(3,4),(3,5),(3,6), 
		 	   		 	         (4,4),(4,5),(4,6), 
		 	   		 	               (5,5),(5,6),
		 	   		 	                     (6,6)]).

frame([[3,1,2,6,6,1,2,2],
       [3,4,1,5,3,0,3,6],
       [5,6,6,1,2,4,5,0],
       [5,6,4,1,3,3,0,0],
       [6,1,0,6,3,2,4,0],
       [4,1,5,2,4,3,5,5],
       [4,1,0,2,4,5,2,0]]).


my_length([],0).
my_length([A|B],L):- my_length(B,L1), L is L1+1.

%%Vriskei tis pithanes theseis enos tile se mia seira tou frame
row(_,[H],_,_,L,L).
row((A,B),[H1,H2|T],X,Y,[(X,Y,X1,Y)|L],K) :- (A==H1), (B==H2), X1 is X + 1, row((A,B),[H2|T],X1,Y,L,K),!.
row((A,B),[H1,H2|T],X,Y,[(X1,Y,X,Y)|L],K) :- (A==H2), (B==H1), X1 is X + 1, row((A,B),[H2|T],X1,Y,L,K),!.
row((A,B),[H1,H2|T],X,Y,L,K) :- X1 is X + 1, row((A,B),[H2|T],X1,Y,L,K).

%%Vriskei tis pithanes theseis enos stoixeiou se kathe seira tou frame
h_tile_placement(_,[],_,L,L).
h_tile_placement((A,B),[H|T],Y,P,K) :- row((A,B),H,0,Y,L,K),Y1 is Y + 1, h_tile_placement((A,B),T,Y1,P,L),!.

%%Vriskei tis pithanes theseis enos tile se mia sthlh tou frame
col((A,B),[[],[]],_,_,L,L).
col((A,B),[[H1|T1],[H2|T2]|T],X,Y,[(X,Y,X,Y1)|L],K):- (A==H1), (B==H2), X1 is X + 1, Y1 is Y + 1, col((A,B),[T1,T2|T],X1,Y,L,K),!.
col((A,B),[[H1|T1],[H2|T2]|T],X,Y,[(X,Y1,X,Y)|L],K):- (A==H2), (B==H1), X1 is X + 1, Y1 is Y + 1, col((A,B),[T1,T2|T],X1,Y,L,K),!.
col((A,B),[[H1|T1],[H2|T2]|T],X,Y,L,K):- X1 is X + 1, col((A,B),[T1,T2|T],X1,Y,L,K).

%%Vriskei tis pithanes theseis enos tile se kathe sthlh tou frame
v_tile_placement(_,[H],_,L,L).
v_tile_placement((A,B),[H1,H2|T],Y,P,K) :- col((A,B),[H1,H2],0,Y,L,K), Y1 is Y + 1, v_tile_placement((A,B),[H2|T],Y1,P,L),!.

%%Vriskei tis pithanes theseis enos tile se olo to frame
place_tile((A,B),[(A,B),Len|P],K) :- frame(Frame), h_tile_placement((A,B),Frame,0,L,K), frame(Frame), v_tile_placement((A,B),Frame,0,P,L), my_length(P,Len).

%%Vriskei tis pithanes theseis olwn twn tiles se olo to frame
place_dominos([],L,L).
place_dominos([H|T],[P|L],K) :- place_tile(H,P,[]), place_dominos(T,L,K).

%%Ananewsh ypolistas. Oi theseis pou tairiazoun me tis suntetagmenes pou dinontai afairountai apo ayth. 
update_inner_list(_,[(A,B),L2],[],Len,Len,(A,B)).
update_inner_list((A,B,C,D),[Tile,Len,(E,F,G,H)|L],[(E,F,G,H)|K],L3,L1,Tile) :- (((A\=E);(B\=F)),((C\=G);(D\=H)),((A\=G);(B\=H)),((C\=E);(D\=F))),
                                                                                update_inner_list((A,B,C,D),[Tile,Len|L],K,L2,L1,Tile),
                                                                                L3 is L2 + 1,!. 
update_inner_list((A,B,C,D),[Tile,Len,(E,F,G,H)|L],K,L2,L1,Tile) :- update_inner_list((A,B,C,D),[Tile,Len|L],K,L2,L1,Tile),!.                               
                         		
%%Ananewsh olhs ths listas. 
update_list(_,[],[]).
update_list((A,B,C,D),[H|L],[[Tile,Len|M]|K]) :- update_inner_list((A,B,C,D),H,M,Len,0,Tile), (Len\=0), update_list((A,B,C,D),L,K),!.
update_list((A,B,C,D),[H|L],[Tile,Len|K]) :- update_inner_list((A,B,C,D),H,M,Len,0,Tile), update_list((A,B,C,D),L,K),!.

%%Taxinomhsh analoga me to mhkos kathe listas.
quick_sort(List,Sorted):- q_sort(List,[],Sorted).
q_sort([],Acc,Acc).
q_sort([H|T],Acc,Sorted):- pivoting(H,T,L1,L2),
	                       q_sort(L1,Acc,Sorted1),q_sort(L2,[H|Sorted1],Sorted),!.
   
pivoting(H,[],[],[]).
pivoting([Tile2,Len2|H],[[Tile1,Len1|X]|T],[[Tile1,Len1|X]|L],G):- Len1>Len2, pivoting([Tile2,Len2|H],T,L,G).
pivoting([Tile2,Len2|H],[[Tile1,Len1|X]|T],L,[[Tile1,Len1|X]|G]):- Len1=<Len2, pivoting([Tile2,Len2|H],T,L,G).

%%Topothethsh tiles sto frame. Oi theseis epistrefontai se lista.
placement([],[]).
placement([[Tile,Len,Pos|_]|L],[(Tile,Pos)|PrevF]) :- update_list(Pos,L,UpList), quick_sort(UpList,Sorted), placement(Sorted,PrevF).
placement([[Tile,Len,_|B]|L],PrevF) :- placement([[Tile,Len|B]|L],PrevF).

%%Elegxos gia to an ena tile vrisketai se 2 syntetagmenes
connected_pos([(Tile,X3,Y3,X4,Y4)|List],X1,Y1,X2,Y2) :- ((X1==X3), (Y1==Y3), (X2==X4), (Y2==Y4)) ; ((X1==X4), (Y1==Y4), (X2==X3), (Y2==Y3)),!.
connected_pos([(Tile,X3,Y3,X4,Y4)|List],X1,Y1,X2,Y2) :- connected_pos(List,X1,Y1,X2,Y2),!.

%%Ektypwsh grammhs
row_result([],_,_,_).
row_result([H|Row],X,Y,List) :- X1 is X + 1, connected_pos(List,X,Y,X1,Y), write(H), write("-"), row_result(Row,X1,Y,List),!.
row_result([H|Row],X,Y,List) :- X1 is X + 1, Y1 is Y + 1, write(H), write(" "), row_result(Row,X1,Y,List),!.

%%Ektypwsh sthlhs
col_result([],_,_,_).
col_result([H|Row],X,Y,List) :- Y1 is Y + 1, connected_pos(List,X,Y,X,Y1), write("|"), write(" "), X1 is X + 1, col_result(Row,X1,Y,List),!.
col_result([H|Row],X,Y,List) :- write(" "), write(" "), X1 is X + 1, col_result(Row,X1,Y,List),!.

%%Ektypwsh frame
print_result([],_,_,_).
print_result([H|Frame],X,Y,List) :- row_result(H,X,Y,List), write("\n"), col_result(H,X,Y,List), write("\n"), Y1 is Y + 1, print_result(Frame,X,Y1,List),!.

put_dominos :- dominos(D), frame(F), place_dominos(D,PosList,[]), quick_sort(PosList,Sorted), placement(Sorted,L), print_result(F,0,0,L).

