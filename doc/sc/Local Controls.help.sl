# Local Controls

Local controls are control inputs that are local to the synthesiser definition.
They have a name, an index and a default value.
Messages can be sent to the synthesiser to set these controls.

The ordinary way to make local controls is from an _IdentityDictionary_.

```
var ctl = (freq: 440, amp: 0.1).localControls;
SinOsc(ctl::freq, 0) * ctl::amp
```

If the default values for a control are an array then an array of control inputs are created, the names qualified by integer suffixes.
In the graph below there are three controls, _freq1_, _freq2_ and _amp_.

```
var ctl = (freq: [440, 440 + 3], amp: 0.1).localControls;
SinOsc(ctl::freq, 0) * ctl::amp
```

The controls are sequenced from left to right.
The array to reset all three controls above to their initial values is _[440, 440 + 3, 0.1]_.
