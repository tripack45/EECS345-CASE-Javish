class Shape {
  function area() {
    return 0;
  }
}

class Rectangle extends Shape {
  var height;
  var width;

  function setHeight(h) {
    height = h;
  }

  function setWidth(w) {
    width = w;
  }

  function getHeight() {
    return height;
  }

  function getWidth() {
    return width;
  }

  function area() {
    return getWidth() * getHeight();
  }
}

class Square extends Rectangle {
  function setSize(size) {
    super.setWidth(size);
  }

  function getHeight() {
    return super.getWidth();
  }

  function setHeight(h) {
    super.setWidth(h);
  }

  static function main() {
    var s = new Square();
    var sum = 0;
    s.setSize(10);
    sum = sum + s.area();
    s.setHeight(4);
    sum = sum + s.area();
    s.setWidth(1);
    sum = sum + s.area();
    return sum;
  }
}