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
  int x;
  float z [s];
  char c;
  string s;
  int i;
  bool t;
  
  a = 18;
  b = 2.9;
  x = -a + b;
  z = 2.9 + 5.8;
  c = 'c';
  s = "abc";
  t = (true && (!false)) || false;
  ++a;
  --x;
  !t;
  
  for (i = 0; i < a; i=i+1) {
    print(i);
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
