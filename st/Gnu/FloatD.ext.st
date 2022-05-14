!FloatD methodsFor: 'stsc3'!
+ aNumber
     <primitive: VMpr_FloatD_plus>
    ^ aNumber adaptToFloat: self andSend: #+!
- aNumber
     <primitive: VMpr_FloatD_minus>
    ^ aNumber adaptToFloat: self andSend: #-!
* aNumber
    <primitive: VMpr_FloatD_times>
    ^ aNumber adaptToFloat: self andSend: #*!
/ aNumber
    <primitive: VMpr_FloatD_divide>
    ^ aNumber adaptToFloat: self andSend: #/!
< aNumber
    <primitive: VMpr_FloatD_lt>
    ^ aNumber adaptToFloat: self andCompare: #<!
> aNumber
    <primitive: VMpr_FloatD_gt>
    ^ aNumber adaptToFloat: self andCompare: #>!
<= aNumber
    <primitive: VMpr_FloatD_le>
    ^ aNumber adaptToFloat: self andCompare: #<=!
>= aNumber
    <primitive: VMpr_FloatD_ge>
    ^ aNumber adaptToFloat: self andCompare: #>=!
!
