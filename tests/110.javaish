class A {
  static function divide(x, y) {
    if (y == 0)
      throw new Zero();
    return x / y;
  }

  static function main() {
    var x = 0;
    var j = 1;

    try { 
     while (j >= 0) {
      var i = 10;
      while (i >= 0) {
        try {
          x = x + divide(10*i, i);
        }
        catch(e) {
          x = x + divide(e.getValue(), j);
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
  }
}

class Zero {
  var value = 10;

  function getValue() {
    return value;
  }
}