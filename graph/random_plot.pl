:- module(
  random_plot,
  [
    random_plot//2, % +Graph:ugraph
                    % +Options:list(nvpair)
    random_vertex_point/4 % +Vertices:ordset
                          % +Options:list(nvpair)
                          % +Vertex
                          % -Point:compound
  ]
).

/** <module> Plotting graphs: Random

Random graph plotting.

@author Wouter Beek
@version 2014/11
*/

:- use_module(library(apply)).
:- use_module(library(random)).

:- use_module(plGraph_plot(plot_generics)).

:- use_module(plSvg(svg_shape)).

:- predicate_options(random_plot/2, 2, [
     pass_to(surface_size/2, 2)
   ]).
:- predicate_options(random_vertex_point/4, 2, [
     pass_to(surface_size/2, 2)
   ]).



%! random_plot(+Graph:ugraph, +Options:list(nvpair))// .

random_plot(G, Options) -->
  {
    surface_size(Width-Height, Options),
    R = 0.5,
    vertices(G, Vs)
  },
  svg:svg(
    [height=Height,version='1.1',width=Width],
    [\random_plot_vertices(Width-Height, R, Vs)]
  ).


%! random_plot_vertices(
%!   +SurfaceSize:pair(float),
%!   +Radius:float,
%!   +Vertices:list
% )// .

random_plot_vertices(Width-Height, R, [H|T]) -->
  {random_point([Width,Height], [X,Y])},
  circle(point(X,Y,_), R, []),
  random_plot_vertices(Width-Height, R, T).
random_plot_vertices(_, _, []) --> [].



%! random_coord(
%!   +Vertices:ordset,
%!   +Options:list(nvpair),
%!   +Vertex,
%!   -Point:compound
%! ) is det.
% Assigns random positions to vertices.
%
% `Point` is a compound term of the following form:
% ~~~{.pl}
% point(X:float,Y:float,Changeable:boolean)
% ~~~
%
% The following options are supported:
%   * =|surface_size(+pair(float))|=

random_coord(_, Options, _, Point):-
  surface_size(SurfaceSize, Options),
  random_point(SurfaceSize, Point).



% HELPERS

%! random_point(+SurfaceSize:pair(float), -RandomPoint:compound) is det.

random_point(Width-Height, point(X,Y,false)):-
  random(0.0, Width, X),
  random(0.0, Height, Y).
