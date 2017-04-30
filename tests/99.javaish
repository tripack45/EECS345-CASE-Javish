class List {
  var val;
  var next;

  function getNext() {
    return next;
  }

  function makeList(x) {
    if (x == 0)
      next = 0;
    else {
      next = new List();
      next.setVal(getVal()+1);
      next.makeList(x-1);
    }
  }

  function setVal(x) {
    val = x;
  }

  function getVal() {
    return val;
  }

  function expand() {
    var p = this;
    while (p != 0) {
      function exp(a) {
        while (a != 0) {
          this.setVal(this.getVal() + p.getVal() * a.getVal());
          a = a.getNext();
        }
      }
      exp(p);
      p = p.getNext();
    }
  }


  static function main() {
    var l = new List();
    l.val = 1;
    l.makeList(5);
    l.expand();
    return l.getVal();
  }
}