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

int max(int a, int b) {
  if (a < b) { return b; }
  else { return a; }
}

int main() {
  int a;
  float b [s];
  char c;
  int e;
  string s;
  bool t;
  
  a = 18;
  b = 2.9;
  b = -a + b;
  b = 2.9 * 5.8;
  c = 'c';
  s = "helloworld";
  e = a * b / 99 % 1;
  t = (true && (!false)) || false;
  ++a;
  --x;
  !t;
  
  for (i = 0; i < a; i=i+1) {
    print(i);
  }

  if (b >= a) {
      a = 100;
  }
  if (a > b) {
      b = 100;
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
