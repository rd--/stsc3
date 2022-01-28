var Module = {
    preRun: [],
    postRun: [],
    print: (function() {
        return function(text) {
            if (arguments.length > 1) text = Array.prototype.slice.call(arguments).join(' ');
            console.log(text);
        };
    })(),
    printErr: function(text) {
        if (arguments.length > 1) text = Array.prototype.slice.call(arguments).join(' ');
        console.error(text);
    },
    setStatus: function(text) {
    },
    totalDependencies: 0,
    monitorRunDependencies: function(left) {
    },
    onRuntimeInitialized: function() {
        console.log('onRuntimeInitialized');
    }
};

window.onerror = function(event) {
};
