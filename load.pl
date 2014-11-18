% The load file for the plGraphDraw project.

:- dynamic(user:project/2).
:- multifile(user:project/2).
   user:project(
     plGraphDraw,
     'Graph theory library written in SWI-Prolog.'
   ).

:- use_module(load_project).
:- load_project([
     mt-'ModelTheory',
     plc-'Prolog-Library-Collection',
     plDcg,
     plGraph,
     plGraphViz,
     plHtml,
     plHttp,
     plLangTag,
     plLattice,
     plRdf,
     plSet,
     plSvg,
     plTms,
     plTree,
     plUri,
     plXml,
     plXsd
   ]).
