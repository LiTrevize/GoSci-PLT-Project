struct Person {
  string name;
  int age;
}

func main() int {
    Person student;
    student = Person{"Batman", 1};

    student.age = 1 + student.age;
    print(student.age);
    print("end");
}