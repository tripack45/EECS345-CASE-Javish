class A {

  var x = 100;

  function setX(x) {
    this.x = x;
  }

  function add(a) {
    return a.x + this.x;
  }

  static function main() {
    var a1 = new A();
    var a2 = new A();
    a1.setX(30);
    a2.setX(6);
    return a1.add(a2);
  }
}