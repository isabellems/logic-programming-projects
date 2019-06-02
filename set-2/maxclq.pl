:- set_flag(print_depth,1000).
:- lib(ic).
:- lib(branch_and_bound).

create_graph(NNodes, Density, Graph) :-
   cr_gr(1, 2, NNodes, Density, [], Graph).

cr_gr(NNodes, _, NNodes, _, Graph, Graph).
cr_gr(N1, N2, NNodes, Density, SoFarGraph, Graph) :-
   N1 < NNodes,
   N2 > NNodes,
   NN1 is N1 + 1,
   NN2 is NN1 + 1,
   cr_gr(NN1, NN2, NNodes, Density, SoFarGraph, Graph).
cr_gr(N1, N2, NNodes, Density, SoFarGraph, Graph) :-
   N1 < NNodes,
   N2 =< NNodes,
   rand(1, 100, Rand),
   (Rand =< Density ->
      append(SoFarGraph, [N1 - N2], NewSoFarGraph) ;
      NewSoFarGraph = SoFarGraph),
   NN2 is N2 + 1,
   cr_gr(N1, NN2, NNodes, Density, NewSoFarGraph, Graph).

rand(N1, N2, R) :-
   random(R1),
   R is R1 mod (N2 - N1 + 1) + N1.

maxclq(Nodes,Density,Clique,Size) :- create_graph(Nodes, Density, Graph),
                                     def_vars(Nodes,Bin),
                                     invert_graph(1,2,Nodes,Graph,Inv),
                                     constrain(Bin,Inv),
                                     Cost #= Nodes-sum(Bin),
                                     bb_min(search(Bin,0,input_order,indomain,complete,[]),Cost,_),
                                     decimal(Bin,1,0,Size,Clique).

def_vars(Nodes,Bin) :- length(Bin,Nodes),
                        Bin #:: [0,1].

constrain(_,[]).
constrain(Bin,[N1-N2|Inv]) :- bin_sum(Bin,N1,N2,1,Sum), constrain(Bin,Inv).

%%Sunthikh na ta zevgh pou den enwnontai metaksi ston grafo , sthn binary lista na mhn exoun athroisma panw apo 1
bin_sum([H|Bin],N1,N2,N3,Sum) :- (N3 #= N1), N4 is N3 + 1, bin_sum(Bin,N1,N2,N4,S1), S1 + H #=< 1.
bin_sum([H|Bin],N1,N2,N3,H) :- (N3 #= N2).
bin_sum([H|Bin],N1,N2,N3,Sum) :- (N3 #\= N1), (N3 #\= N2), N4 is N3 + 1, bin_sum(Bin,N1,N2,N4,Sum).

%%Metatroph binary se decimal kai epistrofh size
decimal([],_,S,S,[]).
decimal([H|Bin],Node1,S,Size,Res) :- (H#=0), !, Node2 is Node1 + 1, decimal(Bin,Node2,S,Size,Res).
decimal([H|Bin],Node1,S,Size,[Node1|Res]) :- (H#=1), Node2 is Node1 + 1, decimal(Bin,Node2,S,S1,Res), Size is S1 + 1.

%%Epistrofh anapodou grafou, lista me tis akmes pou den enwnontai.
invert_graph(Node1,Node2,Nodes,[N1-N2|Graph],[Node1-Node2|Inv]) :- (Node1 #< Nodes),(Node2 #=< Nodes),
                                                                   ((N1 #> Node1); (N2 #>Node2)),
                                                                   Node3 is Node2 + 1, invert_graph(Node1,Node3,Nodes,[N1-N2|Graph],Inv),!.
invert_graph(Node1,Node2,Nodes,[N1-N2|Graph],Inv) :- (Node1 #< Nodes),(Node2 #=< Nodes),
                                                     ((N1 #=< Node1);(N2 #<Node2)),
                                                     Node3 is Node2 + 1, invert_graph(Node1,Node3,Nodes,Graph,Inv),!.
invert_graph(Node1,Node2,Nodes,[],[Node1-Node2|Inv]) :- (Node1 #< Nodes), (Node2 #=< Nodes),
                                                         Node3 is Node2 + 1, invert_graph(Node1,Node3,Nodes,[],Inv),!.
invert_graph(Node1,Node2,Nodes,[],Inv) :- (Node1 #< Nodes),Node3 is Node1 + 1,Node4 is Node3 + 1,
                                                         invert_graph(Node3,Node4,Nodes,[],Inv),!.
invert_graph(Node1,Node2,Nodes,Graph,Inv) :- (Node1 #< Nodes),(Node2 #=< Nodes),
                                              Node3 is Node2 + 1,invert_graph(Node1,Node3,Nodes,Graph,Inv),!.
invert_graph(Node1,Node2,Nodes,Graph,Inv) :- (Node1 #< Nodes),Node3 is Node1 + 1,Node4 is Node3 + 1,
                                             invert_graph(Node3,Node4,Nodes,Graph,Inv),!.
invert_graph(Node1,Node2,Nodes,Graph,[]) :- (Node1 #= Nodes),!.

