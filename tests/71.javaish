function divide(x, y) {
  if (y == 0)
    throw y;
  return x / y;
}

function main() {
  var x;

  try {
    x = divide(10, 5) * 10;
    x = x + divide(5, 1);
  }
  catch(e) {
    x = e;
  }
  finally {
    x = x + 100;
  }
  return x;
}