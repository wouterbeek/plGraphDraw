:- module(
  plot_generics,
  [
    points_to_surface_size/2, % +Points:list(compound)
                              % -SurfaceSize:pair(float)
    surface_size/2 % -SurfaceSize:pair(float)
                   % +Options:list(nvpair)
  ]
).

/** <module> Graph plotting: Generics

Methods for processing and geenrating vertex coordinates.

These are used for visualizing graphs and for calculating the vertex
positions in iterations of spring embedding.

@author Wouter Beek
@version 2014/11
*/

:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(option)).
:- use_module(library(settings)).

:- predicate_options(surface_size/2, 2, [
     surface_size(+pair(float))
   ]).

:- setting(
  default_surface_size,
  pair(float),
  10.0-10.0,
  'The default size of the drawing surface.'
).



%! points_to_surface_size(
%!   +Points:list(compound),
%!   -SurfaceSize:pair(float)
%! ) is det.
% Returns the size structure that is big enough to display
% the given vertex coordinates.

points_to_surface_size(Points, Width-Height):-
  % Maximum width and height are determined w.r.t. the given coordinates.
  maplist(point_components, Points, Xs, Ys),
  max_list(Xs, Width),
  max_list(Ys, Height).



%! surface_size(-SurfaceSize:pair(float), +Options:list(nvpair)) is det.

surface_size(X-Y, Options):-
  setting(default_surface_size, Default),
  option(surface_size(X-Y), Options, Default).



% HELPERS

%! point_components(+Point:compound, +X:float, +Y:float) is semidet.
%! point_components(+Point:compound, -X:float, -Y:float) is det.

point_components(point(X,Y,_), X, Y).
