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
  for (a != b) {
    if (b < a) {
      a = a - b;
    } else {
      b = b - a;
    }
  }
  return a;
}

int main() {
  int a;
  float b;
  char c;
  string s;
  bool t;
  int x;
  int i;
  float z [s];
  
  a = 10;
  b = 18.1;
  x = 2 ^ 3;
  z = 2.9;
  z = -b + z;
  z = 2.9 * 5.8 ^ 2 / 1.0;
  c = 'c';
  s = "helloworld";
  t = (true && (!false)) || false;
  ++b;
  --z;
  !t;
  
  for (i = 0; i < a; ++i) {
    print(i);
  }

  if (z >= b) {
      b = 100.0;
  }
  if (b > z) {
      z = 100.0;
  }

  switch (x+1;x) {
    case 1, 2:
      return x;
    case 3:
      break;
    default:
      return x - 1;
  }

  match (v := c) {
    case int:
      return 1;
    case float:
      return 2;
    default:
      break;
  }

  print(gcd(3,15));
  print(gcd(a,x));
  return 0;
}
