class List {
  var val;
  var next;

  function getNext() {
    return next;
  }

  function setNext(x) {
    if (x == 0)
      next = 0;
    else {
      next = new List();
      next.setVal(val+1);
      next.setNext(x-1);
    }
  }

  function setVal(x) {
    val = x;
  }

  static function main() {
    var l = new List();
    l.setVal(10);
    l.setNext(5);
    return l.getNext().getNext().getNext().getNext().getNext().val;
  }
}