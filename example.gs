/* The GCD algorithm in NanoC */
/* TODO: change to GoSci */
int a;
float vel [m][s -1];

unit U {}

unit km {
  1000.0 m
}

vartype Num {
  int | float
}

struct Person {
  string name;
  int age;
}

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

func main() int{
  int a;
  float b;
  char c;
  string s;
  bool flag;
  int x;
  int i;
  float t [s];
  float acc [m][s -2];
  float acc2 [km][s -2];
  Num number;
  Person student;
  
  a = 10;
  b = 18.1;
  x = 2 ^ 3;
  t = -b;
  t = 2.9 * 5.8 ^ 2 / 1.0;

  /* unit auto checking and conversion */
  acc = vel / t;
  t ^ 3;  /* [s 3] */
  acc * vel;  /* [m 2][s -3] */
  acc / acc;  /* [] */
  acc2 = acc;  /* acc / 1000 */
  acc = acc2;  /* acc2 * 1000 */
  acc2 + acc;  /* acc / 1000 */
  acc + acc2;  /* acc2 * 1000 */
  
  c = 'c';
  s = "helloworld";
  flag = (true && (!false)) || false;
  !flag;
  
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
