# playEvery - schedule sound generating procedure at intervals

- _playEvery(soundProcedure, interval)_

Play _soundProcedure_ now, and re-schedule recursively after _interval.value.seconds_.

Scheduling is on _system::clock_.
