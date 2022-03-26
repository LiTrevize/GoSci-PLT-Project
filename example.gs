/* The GCD algorithm in NanoC */
/* TODO: change to GoSci */
int a;
float b [m][s -1];

unit U {}

unit km {
  1000 m
}

unit L {
  m | km
}

vartype Num {
  int | float
}

int gcd(int a, int b) {
  while (a != b) {
    if (b < a) a = a - b;
    else b = b - a;
  }
  return a;
}

int max(int a, int b) {
  if (a < b) return b;
  else return a;
}

int main() {
  int x;
  float z [s];
  char c;
  string s;
  a = 18;
  b = 2.9;
  x = 2;
  z = 2.9 + 5.8;
  c = 'c';
  s = "abc";
  print(gcd(3,15));
  print(gcd(a,x));
  return 0;
}
