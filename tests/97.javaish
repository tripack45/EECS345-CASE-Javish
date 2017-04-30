class List {
  var val;
  var next;

  function getNext() {
    return next;
  }

  function setNext(next) {
    this.next = next;
  }

  function makeList(x) {
    if (x == 0)
      next = 0;
    else {
      next = new List();
      next.setVal(val+1);
      next.makeList(x-1);
    }
  }

  function setVal(x) {
    val = x;
  }

  function reverse() {
    if (getNext() == 0)
      return this;
    else
      return getNext().reverse().append(this);
  }

  function append(x) {
    var p = this;
    while (p.getNext() != 0)
      p = p.getNext();
    p.setNext(x);
    x.setNext(0);
    return this;
  }

  static function main() {
    var l = new List();
    l.setVal(1);
    l.makeList(5);
    l = l.reverse();

    var result = 0;
    var p = l;
    var c = 1;
    while (p != 0) {
      result = result + c * p.val;
      c = c * 10;
      p = p.getNext();
    }
    return result;
  }
}