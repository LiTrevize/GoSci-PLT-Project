func gcd(int a, int b) int {
  for (a != b) {
    if (b < a) {
      a = a - b;
    } else {
      b = b - a;
    }
  }
  return a;
}

func main() int {
    int a;
    int b;
    int c;
    a = 1;
    b = 2;
    c = a + b;
    print(c);
    print(gcd(8, 6));
    return 0;
}