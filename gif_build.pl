:- module(
  gif_build,
  [
    build_gif/3, % +Edges:ordset
                 % -Gif:compound
                 % +Options:list(nvpair)
    build_gif/4 % +Vertices:ordset
                % +Edges:ordset
                % -Gif:compound
                % +Options:list(nvpair)
  ]
).

/** <module> GraphViz Graph Interchange Format (GIF) build

Support for building GIF representations.

# Graph Intermediate Format (GIF)

## Graph

~~~{.pl}
graph(Vertices:ordset(compound),Ranks,Edges:compound,Attributes:list(nvpair))
~~~

### Edge

~~~{.pl}
edge(FromVertexId,ToVertexId,Attributes:list(nvpair))
~~~

### Rank

~~~{.pl}
rank(RankVertex:compound,ContentVertices:ordset(compound))
~~~

### Vertex

~~~{.pl}
vertex(Id,Attributes:list(nvpair))
~~~

# Property functions

Edge label:
  1. [[graph_edge]] edge_label/2

Vertex coordinates:
  1. [[circle_coords]] circular_coord/4
  2. [[random_coords]] random_coord/4

--

@author Wouter Beek
@version 2014/06-2014/07, 2014/10-2014/11
*/

:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(ordsets)).

:- use_module(generics(list_ext)).
:- use_module(generics(option_ext)).

:- use_module(plGraph(graph_srep)).

:- predicate_options(build_gif/3, 3, [
     pass_to(build_gif/4, 4)
   ]).
:- predicate_options(build_gif/4, 4, [
     pass_to(edge_term/3, 3),
     pass_to(graph_attributes/2, 2),
     pass_to(vertex_term/3, 3)
   ]).
:- predicate_options(edge_term/3, 3, [
     edge_arrowhead(+callable),
     edge_color(+callable),
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
     vertex_image(+callable),
     vertex_label(+callable),
     vertex_peripheries(+callable),
     vertex_position(+callable),
     vertex_shape(+callable)
   ]).

:- meta_predicate(build_gif(+,-,:)).
:- meta_predicate(build_gif(+,+,-,:)).

is_meta(edge_arrowhead).
is_meta(edge_color).
is_meta(edge_label).
is_meta(edge_style).
is_meta(vertex_color).
is_meta(vertex_image).
is_meta(vertex_label).
is_meta(vertex_peripheries).
is_meta(vertex_position).
is_meta(vertex_shape).



%! build_gif(+Edges:ordset, -Gif:compound, +Options:list(nvpair)) is det.

build_gif(Es, Gif, Options):-
  edges_to_vertices(Es, Vs),
  build_gif(Vs, Es, Gif, Options).

%! build_gif(
%!   +Vertices:ordset,
%!   +Edges:ordset,
%!   -Gif:compound,
%!   +Options:list(nvpair)
%! ) is det.

build_gif(Vs, Es, graph(VTerms,ETerms,GAttrs), Options1):-
  meta_options(is_meta, Options1, Options2),
  
  % Vertex terms.
  maplist(vertex_term0(Vs, Options2), Vs, VTerms),

  % Edge terms.
  maplist(edge_term0(Vs, Options2), Es, ETerms),

  % Graph attributes.
  graph_attributes(GAttrs, Options2).
vertex_term0(Vs, Options, V, VTerm):-
  vertex_term(Vs, V, VTerm, Options).
edge_term0(Vs, Options, E, ETerm):-
  edge_term(Vs, E, ETerm, Options).


%! edge_term(
%!   +Vertices:ordset,
%!   +Edge:pair,
%!   -EdgeTerm:compound,
%!   +Options:list(nvpair)
%! ) is det.
% The following options are supported:
%   1. =|edge_arrowhead(+atom)|=
%      No default.
%   2. =|edge_color(+atom)|=
%      No default.
%   3. =|edge_label(+atom)|=
%      No default.
%   4. =|edge_style(+atom)|=
%      No default.

edge_term(Vs, E, edge(FromId,ToId,EAttrs), Options):-
  edge_components(E, FromV, ToV),
  nth0chk(FromId, Vs, FromV),
  nth0chk(ToId, Vs, ToV),

  % Arrowhead
  if_option(edge_arrowhead(ArrowheadFunction), Options,
    call(ArrowheadFunction, E, EArrowhead)
  ),
  % Color.
  if_option(edge_color(ColorFunction), Options,
    call(ColorFunction, E, EColor)
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


%! graph_attributes(
%!   -GraphAttributes:list(nvpair),
%!   +Options:list(nvpair)
%! ) is det.
% The following options are supported:
%   1. =|graph_charset(+oneof(['iso-8859-1','Latin1','UTF-8']))|=
%      The name of the character set that is used to encode text in the graph.
%      Default: `UTF-8`.
%   2. =|graph_colorscheme(+oneof([none,svg,x11]))|=
%      The colorscheme from which the color in this graph are taken.
%      Default: `svg`.
%   3. =|graph_directed(+boolean)|=
%      Whether the graph is directed (`true`) or undirected (`false`).
%      Default: `false`.
%   4. =|graph_fontsize(+float)|=
%      The font size of text in the graph.
%      Default: `11.0`.
%   5. =|graph_label(+callable)|=
%      The graph label.
%      No default.
%   6. =|graph_overlap(+boolean)|=
%      Whether the vertices are allowed to overlap.
%      Default: `false`.

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


%! vertex_term(
%!   +Vertices:ordset,
%!   +Vertex,
%!   -VertexTerm:compound,
%!   +Options:list(nvpair)
%! ) is det.
% The following options are supported:
%   1. =|vertex_color(:ColorFunction)|=
%      A function that assigns colors to vertices.
%      No default.
%   2. =|vertex_image(:ImageFunction)|=
%      A function that assinges images to vertices.
%      No default.
%   3. =|vertex_label(:LabelFunction)|=
%      A function that assigns labels to vertices.
%      No default.
%   4. =|vertex_peripheries(:PeripheriesFunction)|=
%      A function that assinges peripheries to vertices.
%      No default.
%   5. =|vertex_position(:PositionFunction)|=
%      No default.
%   6. =|vertex_shape(:ShapeFunction)|=
%      A function that assinges shapes to vertices.
%      No default.

vertex_term(Vs, V, vertex(Id,V,VAttrs), Options):-
  nth0chk(Id, Vs, V),

  % Color.
  if_option(vertex_color(ColorFunction), Options,
    call(ColorFunction, V, VColor)
  ),
  
  % Image.
  if_option(vertex_image(ImageFunction), Options,
    call(ImageFunction, V, VImage)
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
  
  merge_options(
    [
      color=VColor,
      image=VImage,
      label=VLabel,
      peripheries=VPeripheries,
      pos=VPosition,
      shape=VShape
    ],
    VAttrs
  ).
