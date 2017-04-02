function main() {
  var result;
  var base;

  function getpow(a) {
     var x;

     function setanswer(n) {
        result = n;
     }

     function recurse(m) {
       if (m > 0) {
         x = x * base;
         recurse(m-1);
       }
       else
         setanswer(x);
     }

     x = 1;
     recurse(a);
  }
  base = 2;
  getpow(6);
  return result;
}