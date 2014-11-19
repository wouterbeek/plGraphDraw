:- module(
    coords_to_html_table,
    [
      coords_table//2 % +Coordinates:list(pair(float))
                      % +Options:list(nvpair)
    ]
  ).

/** <module> Coordinates to HTML table

Generates HTML tables representing collections of coordinates.

@author Wouter Beek
@version 2014/11
*/

:- use_module(library(apply)).
:- use_module(library(http/html_write)).

:- use_module(generics(pair_ext)).

:- use_module(plHtml(html_table)).

:- predicate_options(coords_table//2, 2, [
     pass_to(html_table//3, 3)
   ]).



%! coords_table(
%!   +Coordinates:list(pair(float)),
%!   +Options:list(nvpair)
%! )// is det.
% Generates an HTML table showing vertex coordinates.

coords_table(Coords, Options) -->
  {
    HeaderRow = ['X','Y'],
    maplist(pair_list, Coords, DataRows)
  },
  html_table(html('Coordinates table.'), [HeaderRow|DataRows], Options).
