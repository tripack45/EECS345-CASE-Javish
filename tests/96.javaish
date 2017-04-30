class Shape {
  function area() {
    return 0;
  }

  function largerThan(s) {
    return this.area() > s.area();
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
    var s1 = new Square();
    var s2 = new Rectangle();
    var s3 = new Square();
    s1.setSize(5);
    s2.setHeight(8);
    s2.setWidth(4);
    s3.setWidth(3);

    var max = s1;
    if (s2.largerThan(max))
      max = s2;
    if (s3.largerThan(max))
      max = s3;
 
    return max.area();
  }
}