:- module(srep_example, []).

/** <module> S-representation example

@author Wouter Beek
@version 2014/11
*/

:- use_module(library(aggregate)).
:- use_module(library(random)).

:- use_module(plGraphViz(gv_file)).

:- use_module(plGraphDraw(build_export_graph)).

:- initialization(srep_example).

srep_example:-
  srep_example(1000).

srep_example(N):-
  aggregate_all(
    set(edge(From,_,To)),
    (
      between(1, N, _),
      random_between(1, N, From),
      random_between(1, N, To)
    ),
    Edges
  ),
  build_export_graph(Edges, ExportGraph, []),
  export_graph_to_gv_file(ExportGraph, _, []).
