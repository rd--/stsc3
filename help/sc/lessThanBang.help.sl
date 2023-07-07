# <! -- first argument

- _aUgen <! anotherUgen_ == - _lessThanBang(aUgen, anotherUgen)_

Evaluates to _aUgen_ after attaching _anotherUgen_ to it.

Used to require that _anotherUgen_ be included in the graph of which _aUgen_ is a part where it would otherwise not be discovered by traversing the completed graph.

This if required where graphs have multiple end points, such as graphs using _LocalOut_ &etc.
