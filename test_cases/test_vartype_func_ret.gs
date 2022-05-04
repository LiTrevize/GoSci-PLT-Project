vartype Num {
    int | float
}

func add(int a, int b) Num {
    Num sum;
    sum = a + b;
    return sum;
}

func main() int {
    Num sum;
    sum = add(2, 3);
    match (v := sum) {
        case int:
            print(v);
        case float:
            print(v);
        default:
            print("no value");
    }
    return 0;
}