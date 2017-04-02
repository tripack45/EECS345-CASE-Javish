function gcd(a, &b) {
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
}
function main () {
  var x = 14;
  var y = 3 * x - 7;
  gcd(x,y);
  return x+y;
}