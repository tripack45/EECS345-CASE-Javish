var x = 0;
var j = 1;

try {
  while (j >= 0) {
    var i = 10;
    while (i >= 0) {
      try {
        if (i == 0)
          throw 1000000;
        x = x + 10*i / i;
      }
      catch(e) {
        if (j == 0)
          throw 1000000;
        x = x + e / j;
      }
      i = i - 1;
    }
    j = j - 1;
  }
}
catch (e2) {
  x = x * 2;
}
return x;