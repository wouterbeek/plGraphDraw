:- module(
  build_export_graph,
  [
    build_export_graph/4 % +Vs:ordset
                         % +Es:ordset(compound)
                         % -ExportG:compound
                         % +Options:list(nvpair)
  ]
).

/** <module> Build graph representation for export

Support for building GIF representations.

# Graph Intermediate Format (GIF)

## Graph

```prolog
graph(Vs:ordset(compound),Ranks,Es:compound,Attributes:list(nvpair))
```

### Edge

```prolog
edge(FromVertexId,ToVertexId,Attributes:list(nvpair))
```

### Rank

```prolog
rank(RankVertex:compound,ContentVertices:ordset(compound))
```

### Vertex

```prolog
vertex(Id,Attributes:list(nvpair))
```

# Property functions

Edge label:
  1. [[graph_edge]] edge_label/2

Vertex coordinates:
  1. [[circle_coords]] circular_coord/4
  2. [[random_coords]] random_coord/4

---

@author Wouter Beek
@version 2014/06-2014/07, 2014/10-2014/12
*/

:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(ordsets)).

:- use_module(generics(list_ext)).
:- use_module(generics(option_ext)).

:- use_module(plGraph(s_graph/s_graph)).
:- use_module(plGraph(s_graph/s_graph_edge)).

:- predicate_options(build_export_graph/4, 4, [
     pass_to(edge_term/3, 3),
     pass_to(graph_attributes/2, 2),
     pass_to(vertex_term/3, 3)
   ]).
:- predicate_options(edge_term/3, 3, [
     edge_arrowhead(+callable),
     edge_color(+callable),
     edge_id(+callable),
     edge_label(+callable),
     edge_style(+callable)
   ]).
:- predicate_options(graph_attributes/2, 2, [
     graph_charset(+oneof(['iso-8859-1','Latin1','UTF-8'])),
     graph_colorscheme(+oneof([none,svg,x11])),
     graph_directed(+boolean),
     graph_fontsize(+float),
     graph_label(+callable),
     graph_overlap(+boolean)
   ]).

:- predicate_options(vertex_term/3, 3, [
     vertex_color(+callable),
     vertex_id(+callable),
     vertex_image(+callable),
     vertex_label(+callable),
     vertex_peripheries(+callable),
     vertex_position(+callable),
     vertex_shape(+callable),
     vertex_uri(+callable)
   ]).

:- meta_predicate(build_export_graph(+,+,-,:)).

is_meta(edge_arrowhead).
is_meta(edge_color).
is_meta(edge_id).
is_meta(edge_label).
is_meta(edge_style).
is_meta(vertex_color).
is_meta(vertex_id).
is_meta(vertex_image).
is_meta(vertex_label).
is_meta(vertex_peripheries).
is_meta(vertex_position).
is_meta(vertex_shape).
is_meta(vertex_uri).





%! build_export_graph(
%!   +Vs:ordset,
%!   +Es:ordset,
%!   -ExportG:compound,
%!   +Options:list(nvpair)
%! ) is det.

build_export_graph(Vs, Es, graph(VTerms,ETerms,GAttrs), Options1):-
  meta_options(is_meta, Options1, Options2),

  % V terms.
  maplist(vertex_term0(Vs, Options2), Vs, VTerms),

  % Edge terms.
  maplist(edge_term0(Vs, Options2), Es, ETerms),

  % Graph attributes.
  graph_attributes(GAttrs, Options2).

vertex_term0(Vs, Options, V, VTerm):-
  vertex_term(Vs, V, VTerm, Options).

edge_term0(Vs, Options, E, ETerm):-
  edge_term(Vs, E, ETerm, Options).



%! edge_term(+Vs:ordset, +E, -ETerm:compound, +Options:list(nvpair)) is det.
% The following options are supported:
%   - `edge_arrowhead(+callable)`
%     No default.
%   - `edge_color(+callable)`
%     No default.
%   - `edge_id(+callable)`
%     Function that assignes the unique identifiers for an edge's
%     incident vertices.
%   - `edge_label(+callable)`
%     No default.
%   - `edge_style(+callable)`
%     No default.

edge_term(Vs, E, edge(FromId,ToId,EAttrs), Options):-
  % Arrowhead
  if_option(edge_arrowhead(ArrowheadFunction), Options,
    call(ArrowheadFunction, E, EArrowhead)
  ),
  
  % Color.
  if_option(edge_color(ColorFunction), Options,
    call(ColorFunction, E, EColor)
  ),
  
  % Id.
  (   option(edge_id(IdFunction), Options)
  ->  call(IdFunction, E, FromId, ToId)
  ;     E = edge(FromV,_,ToV),
        nth0chk(FromId, Vs, FromV),
        nth0chk(ToId, Vs, ToV)
  ),

  
  % Label.
  if_option(edge_label(LabelFunction), Options,
    call(LabelFunction, E, ELabel)
  ),
  
  % Style.
  if_option(edge_style(StyleFunction), Options,
    call(StyleFunction, E, EStyle)
  ),

  merge_options(
    [arrowhead=EArrowhead,color=EColor,label=ELabel,style=EStyle],
    EAttrs
  ).



%! graph_attributes(-GAttrs:list(nvpair), +Options:list(nvpair)) is det.
% The following options are supported:
%   - `graph_charset(+oneof(['iso-8859-1','Latin1','UTF-8']))`
%     The name of the character set that is used to encode text in the graph.
%     Default: `UTF-8`.
%   - `graph_colorscheme(+oneof([none,svg,x11]))`
%     The colorscheme from which the color in this graph are taken.
%     Default: `svg`.
%   - `graph_directed(+boolean)`
%     Whether the graph is directed (`true`) or undirected (`false`).
%     Default: `false`.
%   - `graph_fontsize(+float)`
%     The font size of text in the graph.
%     Default: `11.0`.
%   - `graph_label(+callable)`
%     The graph label.
%     No default.
%   - `graph_overlap(+boolean)`
%     Whether the vertices are allowed to overlap.
%     Default: `false`.

graph_attributes(GAttrs, Options):-
  % Characer set.
  option(graph_charset(Charset), Options, 'UTF-8'),
  % Colorscheme.
  option(graph_colorscheme(Colorscheme), Options, svg),
  % Directed.
  option(graph_directed(Directed), Options, false),
  % Fontsize.
  option(graph_fontsize(Fontsize), Options, 11.0),
  % Label.
  if_option(graph_label(GLabel), Options,
    true
  ),
  % Overlap.
  option(graph_overlap(Overlap), Options, false),

  merge_options(
    [
      charset=Charset,
      colorscheme=Colorscheme,
      directed=Directed,
      fontsize=Fontsize,
      label=GLabel,
      overlap=Overlap
    ],
    GAttrs
  ).



%! vertex_term(+Vs:ordset, +V, -VTerm:compound, +Options:list(nvpair)) is det.
% The following options are supported:
%   - `vertex_color(:ColorFunction)`
%     A function that assigns colors to vertices.
%     No default.
%   - `vertex_id(:ColorFunction)`
%     A functions that assigns unique identifiers to vertices.
%   - `vertex_image(:ImageFunction)`
%     A function that assigns images to vertices.
%     No default.
%   - `vertex_label(:LabelFunction)`
%     A function that assigns labels to vertices.
%     No default.
%   - `vertex_peripheries(:PeripheriesFunction)`
%     A function that assinges peripheries to vertices.
%     No default.
%   - `vertex_position(:PositionFunction)`
%     No default.
%   - `vertex_shape(:ShapeFunction)`
%     A function that assinges shapes to vertices.
%     No default.
%   - `vertex_uri(:UriFunction)`

vertex_term(Vs, V, vertex(VId,VAttrs), Options):-
  % Color.
  if_option(vertex_color(ColorFunction), Options,
    call(ColorFunction, V, VColor)
  ),
  
  % Id.
  (   option(vertex_id(IdFunction), Options)
  ->  call(IdFunction, V, VId)
  ;   nth0chk(VId, Vs, V)
  ),
  
  % Image.
  ignore(
    if_option(vertex_image(ImageFunction), Options,
      call(ImageFunction, V, VImage)
    )
  ),

  % Label.
  if_option(vertex_label(LabelFunction), Options,
    call(LabelFunction, V, VLabel)
  ),

  % Peripheries.
  if_option(vertex_peripheries(PeripheriesFunction), Options,
    call(PeripheriesFunction, V, VPeripheries)
  ),

  % Position.
  if_option(vertex_position(PositionFunction), Options,
    call(PositionFunction, Vs, Options, V, VPosition)
  ),

  % Shape.
  if_option(vertex_shape(ShapeFunction), Options,
    call(ShapeFunction, V, VShape)
  ),
  
  % URI
  if_option(vertex_uri(UriFunction), Options,
    call(UriFunction, V, VUri)
  ),

  merge_options(
    [
      color=VColor,
      image=VImage,
      label=VLabel,
      peripheries=VPeripheries,
      pos=VPosition,
      shape=VShape,
      'URL'=VUri
    ],
    VAttrs
  ).
