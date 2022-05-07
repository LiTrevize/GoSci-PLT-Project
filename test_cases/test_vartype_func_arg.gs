vartype Num {
    int | float
}

func printNum(Num num) int {
    match (v := num) {
        case int:
            print(v*2);
        case float:
            print(v+1.0);
        default:
            print("no value");
    }
    return 0;
}

func main() int {
    Num n;
    n = 2.5;
    printNum(n);
    return 0;
}