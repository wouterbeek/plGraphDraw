:- module(
  fca_web,
  [
    concept_lattice//2, % +ContextName:atom
                        % +Options:list(nvpair)
    fca_web/2 % +Request:list(nvpair)
              % +HtmlStyle
  ]
).

/** <module> FCA: Web

Web-based interface to currently loaded FCA contexts.

@author Wouter Beek
@version 2014/10
*/

:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(option)).

:- use_module(plHttp(request_ext)).

:- use_module(plXml(xml_dom)).

:- use_module(plGraph(graph_srep)).

:- use_module(plGraphViz(gv_file)).

:- use_module(plHtml(html_table)).

:- use_module(plRdf(graph/rdf_graph)).

:- use_module(plLattice(fca/bordat)).
:- use_module(plLattice(fca/fca_concept)).
:- use_module(plLattice(fca/fca_context)).

:- use_module(plGraphDraw(graph/gif_build)).

:- meta_predicate(concept_lattice(+,:,?,?)).

is_meta(vertex_label).

:- predicate_options(concept_lattice//2, 2, [
     vertex_label(+callable)
   ]).



fca_web(Request, HtmlStyle):-
  request_query_nvpair(Request, fca, ContextName), !,
  reply_html_page(
    HtmlStyle,
    title(['FCA - ',ContextName]),
    html([
      \context_table(ContextName),
      \concept_lattice(ContextName, [])
    ])
  ).
fca_web(_, HtmlStyle):-
  reply_html_page(
    HtmlStyle,
    title('FCA - Overview'),
    \fca_overview
  ).


%! concept_lattice(+ContextName:atom, +Options1:list(nvpair))// is det.
% The following options are supported:
%    * =|vertex_label(:Goal)|=

concept_lattice(ContextName, Options1) -->
  {
    meta_options(is_meta, Options1, Options2),

    generate_hasse(ContextName),

    % Collect the edges for the Gif representation.
    aggregate_all(
      set((Os1-As1)-(Os2-As2)),
      bordat:edge(ContextName, Os1-As1, Os2-As2),
      Edges
    ),

    % Cover the special case in which there are no edges
    % but there is a vertex.
    (   Edges == []
    ->  aggregate_all(
          set(Vertex),
          bordat:concept(ContextName, Vertex),
          Vertices
        )
    ;   edges_to_vertices(Edges, Vertices)
    ),

    % Set the default vertex label goal in case none is specified.
    merge_options(Options2, [vertex_label(concept_label)], Options3),

    % Build the Gif representation.
    merge_options(
      [directed(false),graph_label(ContextName)],
      Options3,
      Options4
    ),
    build_gif(Vertices, Edges, Gif, Options4),

    % Export the Gif representation to SVG.
    gif_to_svg_dom(Gif, Svg, [method(dot)])
  },

  % Insert the SVG into the Web page.
  xml_dom_as_atom(Svg).


context_table(ContextName) -->
  {
    aggregate_all(
      set(A),
      context_attribute(ContextName, A),
      As
    ),
    findall(
      [O|Booleans],
      (
        context_object(ContextName, O),
        findall(
          Boolean,
          (
            member(A, As),
            (   i(ContextName, O, A)
            ->  Boolean = 'X'
            ;   Boolean = ''
            )
          ),
          Booleans
        )
      ),
      Rows
    )
  },
  html_table(
    html('FCA context table.'),
    [As|Rows],
    [header_column(true),header_row(true)]
  ).


fca_overview -->
  {
    aggregate_all(
      set(ContextName),
      (
        context_attribute(ContextName, _),
        % Explicitly exclude RDF graphs from the FCA overview.
        % RDF graphs are treated in plTabular.
        \+ is_rdf_graph(ContextName)
      ),
      ContextNames
    ),
    findall(
      [url(Url,ContextName)],
      (
        member(ContextName, ContextNames),
        http_link_to_id(fca, [fca(ContextName)], Url)
      ),
      Rows
    )
  },
  html_table(
    html('Overview of currently loaded FCA contexts.'),
    [['FCA']|Rows],
    [header_column(true),indexed(true)]
  ).



% HELPERS

%! concept_label(+Concept:pair(ordset), -Label:atom) is det.

concept_label(Concept, Label):-
  concept_attributes(Concept, Attributes),
  with_output_to(atom(Label), write_term(Attributes, [])).
