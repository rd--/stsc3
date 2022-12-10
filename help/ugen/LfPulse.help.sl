;; LfPulse ; phase value = (0, 1), offset to lowest point
LfPulse(110, 1 * 0.5, 0.5) * 0.1

;; LfPulse ; as envelope
SinOsc(200, 0) * Lag(LfPulse(7.83, 0, 0.5) > 0, 0.05) * 0.2

;; LfPulse ; as envelope
SinOsc(230, 0) * Lag(LfPulse(MouseX(2.3, 23, 1, 0.2), 0, 0.5).Max(0), 0.01) * 0.2

;; LfPulse ; 50 Hz wave
LfPulse(50, 0, 0.5) * 0.05

;; LfPulse ; modulating frequency
LfPulse(XLn(1, 200, 10), 0, 0.2) * 0.05

;; LfPulse ; amplitude modulation
LfPulse(XLn(1, 200, 10), 0, 0.2) * SinOsc(440, 0) * 0.1

;; LfPulse ; used as both oscillator and lfo
LfPulse(LfPulse(3, 0, 0.3) * 200 + 200, 0, 0.2) * 0.05
