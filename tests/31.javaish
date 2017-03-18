var a = 31160;
var b = 1476;
if (a < b) {
  var temp = a;
  a = b;
  b = temp;
}
var r = a % b;
while (r != 0) {
  a = b;
  b = r;
  r = a % b;
}
return b;