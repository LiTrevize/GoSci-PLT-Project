/* GoSci code */
int a = 1;
float vel [m][s -1] = 2.5;

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
  int a = 10;
  float b = 18.1;
  char c;
  string s;
  bool flag;
  int x = (a/5)^3;
  int i = 0;
  float t [s] = -b;
  float acc [m][s -2];
  float acc2 [km][s -2];
  Num number = 2.5;
  Person student;
  
  t = 2.9 * 5.8 ^ 2 / 1.0;
  student = Person{"superman", 9999};
  student.name = "alice";
  student.age = 12;
  student.age = student.age + 1;

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

  match (v := number) {
    case int:
      v = v + 1;
      return v;
    case float:
      v = v * 2.0;
      return -1;
    default:
      break;
  }

  print(gcd(3,15));
  print(gcd(a,x));
  return 0;
}
