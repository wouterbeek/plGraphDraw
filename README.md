plGraphDraw
===========

Simplified graph drawing in Prolog build on top of library
 [plGraphViz](https://github.com/wouterbeek/plGraphViz)
 that exports Prolog representations of graphs to GraphViz.

The library allows such Prolog representations to be easily generated
 with hooks for often-used attributes.

---

### Installation

~~~prolog
$ git clone https://github.com/wouterbeek/plGraphDraw.git
$ cd plGraphDraw
$ git submodule update --init
~~~

### Draw a sample graph

~~~prolog
$ swipl run.pl
?- use_module(plGraphDraw(srep_example)).
~~~

A randomized graph is generated and exported to file
 `PATH/plGraphDraw/data/tmp.pdf`.

An example of such a random drawing:

![](https://raw.githubusercontent.com/wouterbeek/plGraphDraw/master/example.png "Example graph.")

---

Developed during 2014 by [Wouter Beek](http://www.wouterbeek.com).
