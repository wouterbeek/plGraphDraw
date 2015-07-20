:- module(
  export_graph_to_html_table,
  [
    export_graph_to_html_table//2 % +ExportGraph:compound
                                  % +Options:list(nvpair)
  ]
).

/** <module> Export graphs as HTML tables

Generate HTML tables representing graphs using the GIF representation.

@author Wouter Beek
@version 2013/07, 2014/11
*/

:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).
:- use_module(library(wl/format/wl_table)).

:- use_module(plc(dcg/dcg_arrow)).
:- use_module(plc(dcg/dcg_atom)).
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(generics/list_ext)).
:- use_module(plc(prolog/pl_log)).

:- use_module(plGraph(s_graph/s_graph_edge)).

:- use_module(plHtml(html_dcg)).

:- predicate_options(export_graph_to_html_table//2, 2, [
  border_width(+boolean),
  include_edges(+boolean),
  pass_to(wl_direct_table//2, 2)
]).





%! export_graph_to_html_table(
%!   +ExportGraph:compound,
%!   +Options:list(nvpair)
%! )// is det.

export_graph_to_html_table(graph(_,ETerms,GAttrs), Options1) -->
  {
    % Create the header row and data rows.
    % Decide whether edge labels are included or not.
    option(include_edges(IncludeEdges), Options1),
    (   IncludeEdges == true
    ->  HeaderRow = ['From','To']
    ;   HeaderRow = ['From','Edge','To']
    ),
    maplist(edge_row(IncludeEdges), ETerms, DataRows),
    
    % Specify table border width.
    option(border_width(BorderWidth), Options1, 1),
    atom_phrase(html_style(border-BorderWidth), Style),
    merge_options([style=Style], Options1, Options2),
    
    % Graph name, if any.
    option(name(GName), GAttrs, noname),
    
    merge_options(
      [
        caption(html(['Table representing graph ',GName,'.'])),
        cell(graph_cell(Options2))
      ],
      Options2,
      Options3
    )
  },
  wl_direct_table([head(HeaderRow)|DataRows], Options3).



%! edge_row(+IncludeEdge:boolean, +EdgeTerm:compound, -DataRow:list) is det.

edge_row(false, edge(From,To,_), [From,Edge,To]):-
  atom_phrase(transition(atom(From), atom(To)), Edge).
edge_row(true, edge(From,To,_), [From,To]).



%! graph_cell(+Options:list(nvpair), +EdgeTerm:compound) is det.

graph_cell(Options, edge(FromId,ToId,_)) -->
  {
    % Color.
    (   option(edge_color(ColorFunction), Options)
    ->  call(ColorFunction, FromId-ToId, EColor)
    ;   EColor = black
    ),
    atom_phrase(html_style(color-EColor), Style),
    
    % Label.
    (   option(edge_label(LabelFunction), Options)
    ->  call(LabelFunction, FromId-ToId, ELabel)
    ;   with_output_to(atom(ELabel), write_canonical_blobs(FromId-ToId))
    )
  },
  html(div([class=edge,syle=Style], ELabel)).
graph_cell(Options, vertex(_,V,_)) -->
  {
    % Color.
    (   option(vertex_color(ColorFunction), Options)
    ->  call(ColorFunction, V, VColor)
    ;   VColor = black
    ),
    atom_phrase(html_style(color-VColor), Style),
    
    % Label.
    (   option(vertex_label(LabelFunction), Options)
    ->  call(LabelFunction, V, VLabel)
    ;   with_output_to(atom(VLabel), write_canonical_blobs(V))
    )
  },
  html(div([class=vertex,style=Style], VLabel)).
