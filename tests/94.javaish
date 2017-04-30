class A {
  var x = 1;
  var y = 2;

  function m() {
    return this.m2();
  }

  function m2() {
    return x+y;
  }
}

class B extends A {
  var y = 22;
  var z = 3;

  function m() {
    return super.m();
  }

  function m2() {
    return x+y+z;
  }
}

class C extends B {
  var y = 222;
  var w = 4;

  function m() {
    return super.m();
  }

  static function main() {
    return new C().m();
  }
}