# playEvery -- schedule sound generating procedure at intervals

- _playEvery(aProcedure, interval)_

Evaluate _aProcedure.play_ now, and re-schedule recursively after _interval.value.seconds_.

Scheduling is on _system::clock_.
