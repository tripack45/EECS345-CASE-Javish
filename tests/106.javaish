class A {
  static var x = 10;
  static var y = 20;

  static function add(a, b) {
    return a + b;
  }

  static function main() {
    return A.add(x, A.y);
  }
}

class B extends A {
  static var y = 200;
  static var z = 300;

  static function main() {
    return add(B.x+A.y,B.z+y);
  }
}