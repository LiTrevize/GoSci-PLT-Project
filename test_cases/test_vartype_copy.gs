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
    Num num;
    Num num2;
    num2 = 2.5;
    num = num2;
    match (v := num) {
        case int:
            print(v);
        case float:
            print(v);
        default:
            print("no value");
    }
    return 0;
}