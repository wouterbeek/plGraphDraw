:- module(srep_example, []).

/** <module> S-representation example

@author Wouter Beek
@version 2014/11-2014/12
*/

:- use_module(library(aggregate)).
:- use_module(library(random)).

:- use_module(plGraph(l_graph/l_graph)).

:- use_module(plGraphViz(gv_file)).

:- use_module(plGraphDraw(build_export_graph)).

:- initialization(srep_example).

srep_example:-
  srep_example(1000).

srep_example(N):-
  aggregate_all(
    set(edge(V,_,W)),
    (
      between(1, N, _),
      random_between(1, N, V),
      random_between(1, N, W)
    ),
    Es
  ),
  l_edges_vertices(Es, Vs),
  build_export_graph(Vs, Es, ExportG, []),
  export_graph_to_gv_file(ExportG, _, []).



