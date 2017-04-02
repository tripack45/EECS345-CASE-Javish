function min(x, y, z) {
  if (x < y) {
    if (x < z)
      return x;
    else if (z < x)
      return z;
  }
  else if (y > z)
    return z;
  else
    return y;
}

var x = 10;
var y = 20;
var z = 30;

var min1 = min(x,y,z);
var min2 = min(z,y,x);

function main() {
  var min3 = min(y,z,x);

  if (min1 == min3)
    if (min1 == min2)
      if (min2 == min3)
        return 1;
  return 0;
}