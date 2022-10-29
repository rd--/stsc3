export var stc = {};

stc.iter_map = function(iter, proc) {
    return iter.children.map(proc);
}

stc.as_iter_map = function(node, proc) {
    return stc.iter_map(node.asIteration(), proc);
}
