struct Person {
  string name;
  int age;
}
func foo() Person {
  Person student;
  student = Person{"Batman", 1};

  return student;

  
}
func main() int {
    Person ret;
    ret = foo();
    print(ret.age);
    print("end");
}