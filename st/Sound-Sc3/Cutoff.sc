// EnvGen of simple cutoff envelope
Cutoff AbstractUGen {
    *sustainTime:releaseTime:curve {
        arg p1, p2, p3;
        ^EnvSpec(#[1, 1, 0], [p1, p2], p3, nil, nil, 0).asEnvGen
    }
    *primaryFactoryMethod {
        ^'sustainTime:releaseTime:curve:'
    }
}
