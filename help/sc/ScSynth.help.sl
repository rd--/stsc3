# ScSynth -- synthesiser

```
system.defaultScSynth.inspect
workspace::onDone := { :oscMessage | ['onDone', oscMessage].postLine }
system.defaultScSynth.addOscListener('/done', workspace::onDone)
system.defaultScSynth.removeOscListener('/done', workspace::onDone)
```
