"Line ; note Line is an existing class in Squeak..."
| f |
f := Line start: 200 end: 17000 dur: 5 doneAction: 1.
SinOsc freq: f phase: 0 mul: 0.05

"Line ; self deleting"
a := Line start: 0.1 end: 0 dur: 1 doneAction: 2.
SinOsc freq: 440 phase: 0 mul: a
