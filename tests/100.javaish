class A {
  var count = 0;

  function subtract(a, b) {
    if (a < b) {
       throw b - a;
    }
    else
       return a - b;
  }
}

class B extends A {
  function divide(a, b) {
    if (b == 0)
      throw a;
    else
      return a / b;
  }

  function reduce(a, b) {
    while (a > 1 || a < -1) {
      try {
        a = divide(a, b);
        if (a == 2)
          break;
      }
      catch (e) {
        return subtract(a, b); 
      }
      finally {
        count = count + 1;
      }
    }
    return a;
  }
}

class C {
  function main() {
    var x;
    var b;

    b = new B();

    try {
      x = b.reduce(10, 5);
      x = x + b.reduce(81, 3);
      x = x + b.reduce(5, 0);
      x = x + b.reduce(-2, 0);
      x = x + b.reduce(12, 4);
    }
    catch (a) {
      x = x * a;
    }
    finally {
      x = -1 * x;
    }
    return x - b.count * 100;
  }
}