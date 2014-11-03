:- module(
  circular_plot,
  [
    circular_plot//2, % +Graph:ugraph
                      % +Options:list(nvpair)
    circular_vertex_point/4 % +Vertices:ordset
                            % +Options:list(nvpair)
                            % +Vertex
                            % -Point:compound
  ]
).

/** <module> Plotting graphs: Circular

Circular graph plotting.

@author Wouter Beek
@version 2014/11
*/

:- use_module(plGraph_plot(plot_generics)).

:- use_module(plSvg(svg_shape)).

:- predicate_options(random_plot/4, 2, [
     surface_size(+pair(float))
   ]).



%! circular_plot(+Graph:ugraph, +Options:list(nvpair))// .

circular_plot(G, Options) -->
  {
    % Generic settings.
    surface_size(Width-Height, Options),
    VRadius = 0.5,
    
    % Graph circle properties.
    GRadius is max(Width, Height) / 2,
    X0 = GRadius,
    Y0 = GRadius,
    
    vertices(G, Vs)
  },
  svg:svg(
    [],
    [
      \circle(point(X0,Y0,_), GR, []),
      \vertices(VRadius, Vs)
    ]
  ).


%! vertices(+Radius:float, +Vertices:list)// .

vertices(VRadius, Vs) -->
  {length(Vs, NumberOfVs)},
  vertices(VRadius, NumberOfVs, Vs, 0).


%! vertices(
%!   +Radius:float,
%!   +NumberOfVertices:nonneg,
%!   +Vertices:list,
%!   +Index:nonneg
%! )// .

vertices(VRadius, NumberOfVs, [H|T], I1) -->
  {
    % Vertex-specific angle.
    VAngle is 2 * pi / NumberOfVs,
    
    % (X,Y)-coordinates.
    X is VRadius + VRadius * cos(I1 * VAngle),
    Y is VRadius + VRadius * sin(I1 * VAngle)
  },
  circle(point(X,Y,_), VRadius, []),
  vertices(VRadius, NumberVs, T, I2).
vertices(_, _, [], _) --> [].



%! circular_vertex_point(
%!   +Vertices:ordset,
%!   +Options:list(nvpair),
%!   +Vertice,
%!   -Point:compound
%! ) is det.
% Returns the coordinate of the given vertex, so that all vertices in a
% graph are plotted on a circle.
%
% `Point` is a compound term of the following form:
% ~~~{.pl}
% point(X:float,Y:float,Changeable:boolean)
% ~~~
%
% The following options are supported:
%   * =|surface_size(+pair(float))|=

circular_vertex_point(Vs, Options, V, point(X,Y,true)):-
  surface_size(Width-Height, Options),
  R = 0.5,
  
  % Graph radius.
  GRadius is max(Widht, Height) / 2,

  % Vertex index.
  nth0chk(I, Vs, V),

  % Angle.
  length(Vs, NumberOfVertices),
  % Specifically cater for the case in which there are no vertices.
  (   NumberOfVertices =:= 0
  ->  AnglePerVertice is 2 * pi
  ;   AnglePerVertice is 2 * pi / NumberOfVertices
  ),
  
  % (X,Y)-coordinate.
  X is R + R * cos(I * AnglePerVertice),
  Y is R + R * sin(I * AnglePerVertice).
