function collatz(n) {
  var counteven = 0;
  var countodd = 0;

  function evenstep(n) {
    counteven = counteven + 1;
    return n / 2;
  }

  function oddstep(n) {
    countodd = countodd + 1;
    return 3 * n + 1;
  }

  while (n != 1) {
    if (n % 2 == 0)
      n = evenstep(n);
    else
      n = oddstep(n);
  }
  return counteven + countodd;
}


function main() {
  return collatz(111);
}