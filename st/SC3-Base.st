!Array methodsFor: 'rearranging'!
 transpose
  "Transpose rows and columns of two-dimensional array"
  ^ (1 to: self first size) collect: [:c | self collect: [:r | r at: c]] !
!

!Number methodsFor: 'enumerating'!
 to: n collect: f
  "Array of (self to: n) collected with f"
  ^ ((self to: n) asArray) collect: [:i| f value: i] !
 arrayFill: f
  "Array of (1 to: self) collected with f"
  ^ 1 to: self collect: [:i| f value: i] !
 replicate: x
  "Array of self places each being x"
  ^ self arrayFill: [:i| x] !
 mixFill: f
  "sum of arrayFill"
  ^ (self arrayFill: f) sum !
!
