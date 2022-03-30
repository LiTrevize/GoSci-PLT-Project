/* The GCD algorithm in NanoC */
/* TODO: change to GoSci */
int a;
float vel [m][s -1];

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
  bool flag;
  int x;
  int i;
  float t [s];
  float acc [m][s -2];
  
  a = 10;
  b = 18.1;
  x = 2 ^ 3;
  t = -b;
  t = 2.9 * 5.8 ^ 2 / 1.0;

  /* unit auto convertion
     [s 2] = [s] ^ 2
     [m][s -2] = [m][s -1] / [s]
   */
  t ^ 2;
  acc = vel / t;
  
  c = 'c';
  s = "helloworld";
  flag = (true && (!false)) || false;
  ++b;
  --t;
  !flag;
  
  for (i = 0; i < a; ++i) {
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
