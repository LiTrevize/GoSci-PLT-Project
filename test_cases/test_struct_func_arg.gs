struct Person {
  string name;
  int age;
}

func foo(Person student) int {
  print(student.name);
}

func main() int {
    Person arg;
    arg = Person{"Xiao Ming", 12};
    foo(arg);
    print("end");
}