"Line ; note Line is an existing class in Squeak..."
| f |
f := Line start: 200 end: 17000 dur: 5 doneAction: 1.
SinOsc freq: f kr phase: 0 mul: 0.05
