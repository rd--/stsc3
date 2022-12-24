# schedule -- process

- _schedule(aClock, deltaTime, aProcedure)_
- _schedule(deltaTime, aProcedure)_ ⇒ _aClock = system::clock_
- _schedule(aProcedure)_ ⇒ _deltaTime = 0_

Schedule _aProcedure_ for _deltaTime_ at _aClock_.
When _aProcedure_ is evaluated the answer is either an interval at which to re-schedule, or _nil_ to halt.
