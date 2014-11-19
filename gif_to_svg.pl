:- module(
  gif_to_svg,
  [
    gif_to_svg//1 % +Gif:compound
  ]
).

/** <module> GIF to SVG

Convert from GIF (Graph Interchange Format) to SVG.

@author Wouter Beek
@version 2013/07, 2014/11
*/

:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(lists), except([delete/3])).
:- use_module(library(option)).
:- use_module(library(settings)).

:- setting(
  default_surface_size,
  pair(float),
  10.0-10.0,
  'The default size of the surface on which graphs are displayed.'
).



%! edge( +Vertices:ordset, +Edge:compound)// .
% Generates the SVG element for the given edge term.

edge(Vs, edge(FromVId,ToVId,EAttrs1), Element):-
  {
    % Ids.
    nth0(FromVId, Vs, FromV),
    nth0(ToVId, Vs, ToV),

    % X1 and Y1.
    nth0(FromVId, Vs, vertex(FromVId,_,FromVAttrs)),
    option(pos(point(X1,Y1,_)), FromVAttrs),

    % X2 and Y2.
    nth0(ToVId, Vs, vertex(ToVId,_,ToVAttrs)),
    option(pos(point(X2,Y2,_)), ToVAttrs),

    % Name.
    option(label(EName), EAttrs, nolabel),

    % Color
    select_option(color(EColor), EAttrs, black)
  },
  svg:line(X1, Y1, X2, Y2, [id=EName,stroke=EColor]).

edges([]) --> [].
edges([H|T]) --> edge(H), edges(T).



%! gif_to_svg(+Gif:compound)// .
% Writes a graph in GIF representation as SVG markup.
%
% @tbd Add support for ranks.
% @tbd Use graph name.

gif_to_svg(graph(Vs,_,Es,GAttrs)) -->
  {
    % Graph name.
    option(label(GName), GAttrs, nolabel),

    % Graph surface size.
    setting(default_surface_size, DefaultSurfaceSize),
    option(surface(Width-Height), GAttrs, DefaultSurfaceSize)
  },
  html(
    \rdf_ns(svg),
    svg:svg(
      [class=graph,height=Height,id=GName,version='1.1',width=Width],
      [\vertices(Vs),\edges(Vs, Es)]
    )
  ).



%! vertex(+Vertices:ordset, +VertexTerm:compound)// .

vertex(Vs, vertex(VId,_,VAttrs)) -->
  {
    % Id.
    nth0(VId, Vs, _),

    % X0 and Y0.
    option(pos(point(X0,Y0,_)), VAttrs),

    % Radius.
    setting(default_vertex_radius, DefaultR),
    option(radius(R), VAttrs, DefaultR),

    % Name.
    option(label(VName), VAttrs, nolabel),

    % Color.
    option(color(VColor), VAttrs, blank)
  },
  svg:circle(X0, Y0, R, [id=VName,stroke=VColor]).

vertices([]) --> [].
vertices([H|T]) --> vertex(H), vertices(T).
