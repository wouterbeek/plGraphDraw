:- module(
  gv_tree,
  [
    tree_to_gv_file/3 % +Tree:compound
                      % ?ToFile:atom
                      % +Options:list(nvpair)
  ]
).

/** <module> GraphViz tree

Export trees to GraphViz.

@author Wouter Beek
@version 2014/06-2014/08, 2014/11
*/

:- use_module(library(aggregate)).

:- use_module(plGraphViz(gv_file)).

:- predicate_options(tree_to_gif/3, 3, [
     pass_to(build_gif/4, 4)
   ]).
:- predicate_options(tree_to_gv_file/3, 3, [
     pass_to(gif_to_gv_file/3, 3),
     pass_to(tree_to_gif/3, 3)
   ]).



%! tree_to_gv_file(
%!   +Tree:compound,
%!   ?ToFile:atom,
%!   +Options:list(nvpair)
%! ) is det.
% Stores the given tree term into a GraphViz file.
%
% The following options are passed on to gif_to_gv_file/3:
%   * =|method(+Method:oneof([dot,sfdp])|=
%     The algorithm used by GraphViz for positioning the tree nodes.
%     Either =dot= (default) or =sfdp=.
%   * =|to_file_type(+FileType:oneof([dot,jpeg,pdf,svg,xdot])|=
%     The file type of the generated GraphViz file.
%     Default: `pdf`.

tree_to_gv_file(Tree, ToFile, Options):-
  tree_to_gif(Tree, Gif, Options),
  gif_to_gv_file(Gif, ToFile, Options).



%! tree_to_gif(+Tree:pair, -Gif:compound, +Options:list(nvpair)) is det.
% The following options are passed on to gif_to_gv_file/3:
%   * =|directed(+boolean)|=
%     Whether the graph is directed (`true`) or undirected (`false`).
%     Default: `false`.
%   * =|edge_arrowhead(+atom)|=
%   * =|edge_color(+atom)|=
%   * =|edge_label(+atom)|=
%   * =|edge_style(+atom)|=
%   * =|graph_colorscheme(+oneof([none,svg,x11]))|=
%     The colorscheme from which the color in this graph are taken.
%     Default: `svg`.
%   * =|graph_label(+atom)|=
%     The graph label.
%     No default.
%   * =|vertex_color(+ColorFunction)|=
%     A function that assigns colors to vertices.
%     No default.
%   * =|vertex_image(+ImageFunction)|=
%     A function that assinges images to vertices.
%     No default.
%   * =|vertex_label(+LabelFunction)|=
%     A function that assigns labels to vertices.
%     No default.
%   * =|vertex_peripheries(+PeripheriesFunction)|=
%     A function that assinges peripheries to vertices.
%     No default.
%   * =|vertex_shape(+ShapeFunction)|=
%     A function that assinges shapes to vertices.
%     No default.

tree_to_gif(H-T, Gif, Options):-
  tree_to_vertices_edges(Tree, Vs, Es),
  build_gif(Vs, Es, Gif, Options).
