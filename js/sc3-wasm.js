function sendOsc(oscMessage) {
    Module.oscDriver.send(57120, 57110, osc.writePacket(oscMessage));
};

function bootScsynth(numInputs, numOutputs) {
    var args = Module['arguments'];
    args[args.indexOf('-i') + 1] = String(numInputs);
    args[args.indexOf('-o') + 1] = String(numOutputs);
    args.push('-w', '512');
    //args.push('-m', '131072'); // fixed at scsynth/wasm compile time, see README
    Module.callMain(args);
}

function play(u) {
    var g = new Graph('sc3.js', Out(0, u));
    var d = g.encodeSyndef();
    console.log('play: scsyndef #', d.length);
    sendOsc(d_recv_then(d, osc.writePacket(s_new0('sc3.js', -1, 1, 0))));
}

function reset() {
    sendOsc(g_freeAll1(0));
}
