!Number methodsFor: 'arithmetic'!
 midicps
  "Convert from fractional midi note number to frequency (in hertz)"
  ^ 440 * (2 ** ((self - 69) * (1 / 12))) !
!
