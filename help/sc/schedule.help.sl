# schedule -- process

- _schedule(aClock, deltaTime, aProcedure:/1)_
- _schedule(deltaTime, aProcedure:/1)_ ⇒ _aClock = workspace::clock_
- _schedule(aProcedure:/1)_ ⇒ _deltaTime = 0_

Schedule _aProcedure_ for _deltaTime_ (in seconds) at _aClock_.
When _aProcedure_ is evaluated, with the current logical time as parameter, the answer is either an interval at which to re-schedule, or _nil_ to halt.

* * *

See also: _repeatEvery_, _scheduleInjecting_, _Clock_
