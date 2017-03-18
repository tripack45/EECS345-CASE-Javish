var x = 10;
var result = 1;

try {
  while (x < 10000) {
     result = result - 1;
     x = x + 10;

     if (x > 1000) {
       throw x;
     }
     else if (x > 100) {
        break;
     }
  }
}
finally {
  result = result + x;
}
return result;