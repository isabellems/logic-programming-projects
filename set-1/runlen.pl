
decode_rl([],[]).
decode_rl([(A,B)|T],[A|L]):- (B > 1), B1 is B - 1, decode_rl([(A,B1)|T],L).
decode_rl([(A,_)|T],[A|L]):- !,decode_rl(T,L).
decode_rl([A|T],[A|L]):- decode_rl(T,L).

%%Ypologizei to plhthos enos stoixeiou sthn lista
my_length([],X,L,L).
my_length([A|T],X,L,L):- X \= A,!.
my_length([A|T],X,C,L):- L1 is C+1, my_length(T,X,L1,L).

%%Apokoptei sugkekrimeno arithmo stoixeiwn apo thn lista
cut_list([A|T],N,L):- (L > 0), L1 is L - 1, cut_list(T,N,L1), !.
cut_list(N,N,L).

encode_rl([],[]).
encode_rl([A|T],[A|L]):- my_length([A|T],A,0,M), (M==1), cut_list([A|T],B,M), encode_rl(B,L),!.
encode_rl([A|T],[(A,M)|L]):- my_length([A|T],A,0,M), cut_list([A|T],B,M), encode_rl(B,L).

/**To apotelesma ths ekteleshs thw entolhs ?- encode_rl([p(3),p(X),q(X),q(Y),q(4)], L). einai:
X=X
Y=Y
L=[(p(3),2),(q(X),3)]
Arxika pairnei to p(3) kai to sugkrinei me to epomeno stoixeio, dhladh to p(X). Ta stoixeia einai idia
an thewrhsoume X=3. Kovontai ta duo prwta stoixeia kai ksanakaleitai h encode_rl gia thn upoloiph lista,
ara pairnei to q(X) kai to sugkrinei me to q(Y). Ta thewrei idia stoixeia afou exei metavlhtes pou den 
exoun parei timh. Telos ta sugkrinei me to q(4) kai ta thewrei idia an thewrhsoume X=Y=4.
Epeidh sthn nea lista sto tuple topotheteitai to prwto stoixeio emfanizetai (q(X),3).*/