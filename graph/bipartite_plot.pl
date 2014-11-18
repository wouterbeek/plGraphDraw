:- module(
  bipartite_plot,
  [
    bipartite_plot/4 % +Vertices:ordset
                     % +Options:list(nvpair)
                     % +Vertex
                     % -Point:compound
  ]
).

/** <module> Graph plotting: Bipartite

Bipartite graph plotting.

@author Wouter Beek
@version 2014/11
*/

:- use_module(library(lists), exclude([delete/3])).

:- use_module(plGraph(graph_edge)).
:- use_module(plGraph(graph_type)).
:- use_module(plGraph_plot(plot_generics)).

:- use_module(plSvg(svg_dcg)).

:- predicate_optiosn(bipartite_plot/2, 2, [
     pass_to(surface_size/2, 2)
   ]).



%! bipartite_plot(+Graph:ugraph, +Options:list(nvpair))// .

bipartite_plot(G, Options) -->
  {
    bipartite(G, Vs1, Vs2),
    
    % Global parameters.
    surface_size(SurfaceSize, Options),
    R = 0.5,
    
    % Coordinates of bipartite lines.
    XLine1 is 0.0,
    XLine2 is Width,
    Y1Line is 0.0,
    Y2Line is Height,
    
    % Distance between vertices on bipartite lines.
    length(Vs, NumberOfVs),
    Distance is Height / (NumberOfVs - 1),
    
    edges(G, Es)
  },
  svg:svg(
    [],
    [
      \line(point(XLine1,Y1Line,_), point(XLine1,Y2Line,_), []),
      \line(point(XLine2,Y1Line,_), point(XLine2,Y2Line,_), []),
      \vertices(XLine1, R, Vs1),
      \vertices(XLine2, R, Vs2),
      \edges(Vs1, Vs2, XLine1, XLine2, Distance, Es)
    ]
  ).



%! edges(
%!   +Vertices1:ordset,
%!   +Vertices2:ordset,
%!   +X1:float,
%!   +X2:float,
%!   +Distance:float,
%!   +Edges:ordset(pair)
%! )// .

edges(Vs1, Vs2, X1, X2, Distance, [V1-V2|T]) -->
  {
    nth0(I1, Vs1, V1),
    Y1 is Distance1 * I1,
    nth0(I2, Vs2, V2),
    Y2 is Distance2 * I2
  },
  line(point(X1,Y1,_), point(X2,Y2,_), []),
  edges(Vs1, Vs2, X1, X2, Distance, T).
edges(_, _, _, _, _, []) --> [].



%! vertices(+X:float, +Radius:float, +Vertices:ordset)// .

vertices(X, R, Vs) -->
  vertices(Vs, X, R, Distance, Vs).


%! vertices(
%!   +Vertices:ordset,
%!   +X:float,
%!   +Radius:float,
%!   +Distance:float,
%!   +Vertices:ordset
%! )// .

vertices(Vs, X, R, Distance, [H|T]) -->
  {
    nth0(I, Vs, H),
    Y is Distance * I
  },
  circle(point(X,Y,_), R, []),
  vertices(Vs, X, R, Distance, T).
vertices(_, _, _, _, []) --> "".
