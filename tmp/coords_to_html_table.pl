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
@version 2014/11, 2015/05
*/

:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).

:- use_module(plc(generics/pair_ext)).

:- use_module(library(wl/format/wl_table)).

:- predicate_options(coords_table//2, 2, [
  pass_to(wl_direct_table//2, 2)
]).





%! coords_table(
%!   +Coordinates:list(pair(float)),
%!   +Options:list(nvpair)
%! )// is det.
% Generates an HTML table showing vertex coordinates.

coords_table(Coords, Options0) -->
  {
    HeaderRow = ['X','Y'],
    maplist(pair_list, Coords, DataRows),
    merge_options([caption(html('Coordinates table.'))], Options0, Options)
  },
  wl_direct_table([head(HeaderRow)|DataRows], Options).
