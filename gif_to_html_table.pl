:- module(
  gif_to_html_table,
  [
    gif_to_html_table//2 % +Gif:compound
                         % +Options:list(nvpair)
  ]
).

/** <module> HTML: GIF table

Generate HTML tables representing graphs using the GIF representation.

@author Wouter Beek
@version 2013/07, 2014/11
*/

:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).

:- use_module(generics(list_ext)).
:- use_module(pl(pl_log)).

:- use_module(plDcg(dcg_arrow)).
:- use_module(plDcg(dcg_atom)).
:- use_module(plDcg(dcg_generics)).

:- use_module(plGraph(graph_edge)).

:- use_module(plHtml(html_dcg)).
:- use_module(plHtml(html_table)).

:- predicate_options(gif_to_html_table//2, 2, [
     border_width(+boolean),
     include_edges(+boolean),
     pass_to(html_table//3, 3)
   ]).



%! gif_to_html_table(+Gif:compound, +Options:list(nvpair))// is det.

gif_to_html_table(graph(VTerms,_,ETerms,GAttrs), Options1) -->
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
    dcg_phrase(html_style(border-BorderWidth), Style),
    merge_options([style=Style], Options1, Options2),
    
    % Graph name, if any.
    option(name(GName), GAttrs, noname)
  },
  html_table(
    html(['Table representing graph ',GName,'.']),
    gif_cell(VTerms, Options2),
    [HeaderRow|DataRows],
    Options2
  ).



%! edge_row(+IncludeEdge:boolean, +EdgeTerm:compound, -DataRow:list) is det.

edge_row(false, edge(From,To,_), [From,Edge,To]):-
  dcg_phrase(transition(atom(From), atom(To)), Edge).
edge_row(true, edge(From,To,_), [From,To]).



%! gif_cell(
%!   +VTerms:list(compound),
%!   +Options:list(nvpair),
%!   +ETerm:compound
%! ) is det.

gif_cell(VTerms, Options, edge(FromId,ToId,_)) -->
  {
    nth0chk(FromId, VTerms, vertex(FromId,From,_)),
    nth0chk(ToId, VTerms, vertex(ToId,To,_)),
    edge_components(E, From, To),
    
    % Color.
    (   option(edge_color(ColorFunction), Options)
    ->  call(ColorFunction, E, EColor)
    ;   EColor = black
    ),
    dcg_phrase(html_style(color-EColor), Style),
    
    % Label.
    (   option(edge_label(LabelFunction), Options)
    ->  call(LabelFunction, E, ELabel)
    ;   with_output_to(atom(ELabel), write_canonical_blobs(E))
    )
  },
  html(div([class=edge,syle=Style], ELabel)).
gif_cell(_, Options, vertex(_,V,_)) -->
  {
    % Color.
    (   option(vertex_color(ColorFunction), Options)
    ->  call(ColorFunction, V, VColor)
    ;   VColor = black
    ),
    dcg_phrase(html_style(color-VColor), Style),
    
    % Label.
    (   option(vertex_label(LabelFunction), Options)
    ->  call(LabelFunction, V, VLabel)
    ;   with_output_to(atom(VLabel), write_canonical_blobs(V))
    )
  },
  html(div([class=vertex,style=Style], VLabel)).
