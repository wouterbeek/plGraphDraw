:- module(
  export_graph_to_svg,
  [
    export_graph_to_svg//1 % +Graph:compound
  ]
).

/** <module> GIF to SVG

Convert from GIF (Graph Interchange Format) to SVG.

@author Wouter Beek
@version 2013/07, 2014/11
*/

:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(option)).
:- use_module(library(settings)).

:- use_module(plSvg(svg_dcg)).

:- setting(
  default_surface_size,
  pair(float),
  10.0-10.0,
  'The default size of the surface on which graphs are displayed.'
).



%! edge( +Vertices:ordset, +Edge:compound)// .
% Generates the SVG element for the given edge term.

edge(Vs, edge(FromVId,ToVId,EAttrs)) -->
  {
    % Ids.
    nth0(FromVId, Vs, FromV),
    nth0(ToVId, Vs, ToV),

    % X1 and Y1.
    nth0(FromVId, Vs, vertex(FromVId,FromVAttrs)),
    option(pos(point(X1,Y1,_)), FromVAttrs),

    % X2 and Y2.
    nth0(ToVId, Vs, vertex(ToVId,ToVAttrs)),
    option(pos(point(X2,Y2,_)), ToVAttrs),

    % Name.
    option(label(EName), EAttrs, nolabel),

    % Color
    select_option(color(EColor), EAttrs, black),
    
    % Id
    with_output_to(atom(EId), write_canonical(FromV-ToV))
  },
  html(
    line(
      point(X1,Y1,false),
      point(X2,Y2,false),
      [id=EId,stroke=EColor],
      title([], EName)
    )
  ).

edges(_, []) --> [].
edges(Vs, [H|T]) --> edge(Vs, H), edges(Vs, T).



%! export_graph_to_svg(+Graph:compound)// .
% Writes a graph in GIF representation as SVG markup.
%
% @tbd Add support for ranks.
% @tbd Use graph name.

export_graph_to_svg(graph(Vs,Es,GAttrs)) -->
  {
    % Graph name.
    option(label(GName), GAttrs, nolabel),

    % Graph surface size.
    setting(default_surface_size, DefaultSurfaceSize),
    option(surface(Width-Height), GAttrs, DefaultSurfaceSize)
  },
  html([
    \rdf_ns(svg),
    svg(
      [class=graph,height=Height,id=GName,version='1.1',width=Width],
      [\vertices(Vs),\edges(Vs, Es)]
    )
  ]).



%! vertex(+Vertices:ordset, +VertexTerm:compound)// .

vertex(Vs, vertex(VId,VAttrs)) -->
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
  html(
    circle(point(X0,Y0,false), R, [id=VId,stroke=VColor], title([], VName))
  ).

vertices(Vs) -->
  vertices(Vs, Vs).

vertices(_, []) --> [].
vertices(Vs, [H|T]) --> vertex(Vs, H), vertices(Vs, T).

