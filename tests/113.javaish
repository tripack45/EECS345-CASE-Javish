class Shape {
  function area();

  function changeSize(factor);
}

class Circle extends Shape {
  var radius;

  function setRadius(radius) {
    this.radius = radius;
  }

  function area() {
    return radius * radius * 3;
  }

  static function main() {
    var s = new Circle();
    s.setRadius(5);
    s.changeSize(2);
    return s.area();
  }
}