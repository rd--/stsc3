!Number methodsFor: 'arithmetic'!
 midicps
  "Convert from fractional midi note number to frequency (in hertz)"
  ^440 * (2 raisedTo: ((self - 69) * (1 / 12)))
 !
!

!Float methodsFor: 'arithmetic'!
 linExpFrom: lo to: hi
  "Return myself (I am in range 0.0 to 1.0) to the indicated exponential range"
  ^lo * ((hi / lo) ln * self) exp
 !
!