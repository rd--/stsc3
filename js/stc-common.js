export function iter_map(iter, proc) {
    return iter.children.map(proc);
}

export function as_iter_map(node, proc) {
    return stc.iter_map(node.asIteration(), proc);
}
