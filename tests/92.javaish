class A {

  var x = 100;

  function setX(x) {
    this.x = x;
  }

  function getX() {
    return this.x;
  }

  function add(a) {
    return a.getX() + this.getX();
  }

  static function main() {
    var a1 = new A();
    var a2 = new A();
    a1.setX(50);
    a2.setX(4);
    return a1.add(a2);
  }
}