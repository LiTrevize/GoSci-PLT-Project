/* The GCD algorithm in NanoC */
/* TODO: change to GoSci */
int a;
int b;

int gcd(int a, int b) {
  while (a != b) {
    if (b < a) a = a - b;
    else b = b - a;
  }
  return a;
}

int main() {
  int x;
  int y;
  float z;
  char c;
  string s;
  a = 18;
  b = 9;
  x = 2;
  y = 14;
  z = 1.23;
  c = 'c';
  s = "abc";
  print(gcd(x,y));
  print(gcd(3,15));
  print(gcd(99,121));
  print(gcd(a,b));
  return 0;
}
