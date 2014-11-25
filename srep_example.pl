:- module(srep_example, []).

/** <module> S-representation example

@author Wouter Beek
@version 2014/11
*/

:- use_module(library(aggregate)).
:- use_module(library(random)).

:- use_module(plGraphViz(gv_file)).

:- use_module(plGraphDraw(graph_build)).

:- initialization(srep_example).

srep_example:-
  srep_example(1000).

srep_example(N):-
  aggregate_all(
    set(From-To),
    (
      between(1, N, _),
      random_between(1, N, From),
      random_between(1, N, To)
    ),
    Edges
  ),
  build_graph(Edges, Graph, []),
  graph_to_gv_file(Graph, _, []).