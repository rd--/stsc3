;; tw 0224 (f0) ; http://www.fredrikolofsson.com/f0blog/?q=node/617
var c = 200000;
var b = BufAlloc(2, c).clearBuf;
var d = BufRd(2, b, SinOsc([2, 3] * 9, 0) * c, 0, 2);
var w = BufWr(b, SinOsc([99, 145], 0).Abs * c, 1, SinOsc(3 / [2, 3], 0) / 3);
d.transpose.sum * 0.5 <! w
