# Labelling of Argumentation Frameworks 

Here be implementations of labelling algorithms for argumentation frameworks.

Labelling algorithms traverse an argumentation graph and partition it into IN, OUT, and UNDECIDED partitions.
They may also generate multiple partitions depending on their out look on life.

IN nodes are winning arguments, out nodes are losing nodes and undecided nodes are undecided.

**Grounded** semantics are skeptical. So the grounded labelling algorithm can only generates one labelling. 
It leaves undecided nodes as undecided.
 
**Preferred** semantics are more optimistic. They will generate multiple labellings when there are multiple 
ways to resolve the graph. You can think of this as saying, well maybe we are both right.


Reading: 

* http://www.umiacs.umd.edu/~horty/courses/readings/dung-1995-acceptability.pdf