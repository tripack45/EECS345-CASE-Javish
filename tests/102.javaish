class A {
  var x = 10;
  var y = 20;

  function add(a, b) {
    return a + b;
  }

  function add(a,b,c) {
    return a + b + c;
  }
}

class B extends A {
  var x = 2;
  var y = 30;

  function add(a,b) {
    return a*b;
  }

  static function main() {
    var b = new B();
    return b.add(b.x,b.y) + b.add(b.x,b.x,b.x);
  }
}