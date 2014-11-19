:- module(
  random_plot,
  [
    random_plot//2, % +Graph:ugraph
                    % +Options:list(nvpair)
    random_coord/4 % +Vertices:ordset
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

:- use_module(plSvg(svg_dcg)).

:- use_module(plGraphDraw(plot_generics)).

:- predicate_options(random_plot/2, 2, [
     pass_to(surface_size/2, 2)
   ]).
:- predicate_options(random_coord/4, 2, [
     pass_to(surface_size/2, 2)
   ]).



%! random_plot(+Graph:ugraph, +Options:list(nvpair))// .

random_plot(Graph, Options) -->
  {
    surface_size(Width-Height, Options),
    Radius = 0.5,
    vertices(Graph, Vs)
  },
  html(
    svg(
      [height=Height,version='1.1',width=Width],
      [\random_plot_vertices(Width-Height, Radius, Vs)]
    )
  ).


%! random_plot_vertices(
%!   +SurfaceSize:pair(float),
%!   +Radius:float,
%!   +Vertices:list
%! )// .

random_plot_vertices(Width-Height, Radius, [V|Vs]) -->
  {
    random_point([Width,Height], [X,Y]),
    with_output_to(atom(VId), write_canonical(V))
  },
  html([
    circle(point(X,Y,_), Radius, [id=VId], title([], V)),
    \random_plot_vertices(Width-Height, Radius, Vs)
  ]).
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
