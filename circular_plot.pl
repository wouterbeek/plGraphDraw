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

:- use_module(plc(generics/list_ext)).

:- use_module(plSvg(svg_dcg)).

:- use_module(plGraphDraw(plot_generics)).

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
  html(
    svg(
      [],
      [
        \circle(point(X0,Y0,_), GRadius, [], []),
        \vertices(VRadius, Vs)
      ]
    )
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

vertices(VRadius, NumberOfVs, [V|Vs], Index1) -->
  {
    % Vertex-specific angle.
    VAngle is 2 * pi / NumberOfVs,
    
    % (X,Y)-coordinates.
    X is VRadius + VRadius * cos(Index1 * VAngle),
    Y is VRadius + VRadius * sin(Index1 * VAngle),
    
    % Identifier for vertex.
    with_output_to(atom(VId), write_canonical(V)),
    
    % On to the next index.
    Index2 is Index1 + 1
  },
  html([
    circle(point(X,Y,_), VRadius, [id=VId]),
    \vertices(VRadius, NumberOfVs, Vs, Index2)
  ]).
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
% ```prolog
% point(X:float,Y:float,Changeable:boolean)
% ```
%
% The following options are supported:
%   * =|surface_size(+pair(float))|=

circular_vertex_point(Vs, Options, V, point(X,Y,true)):-
  surface_size(Width-Height, Options),
  
  % Graph radius.
  Radius is max(Width, Height) / 2,

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
  X is Radius + Radius * cos(I * AnglePerVertice),
  Y is Radius + Radius * sin(I * AnglePerVertice).

