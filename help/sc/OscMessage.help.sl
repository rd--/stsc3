# OscMessage -- open sound control

- _OscMessage(address, parameterArray)_

An _Open Sound Control_ message consists of an _address_ string and a (possibly empty) _parameterArray_.

```
OscMessage('/x', []).encode = [47, 120, 0, 0, 44, 0, 0, 0].ByteArray
OscMessage('/y', [3.141]).encode = [47, 121, 0, 0, 44, 102, 0, 0, 64, 73, 6, 37].ByteArray
```
