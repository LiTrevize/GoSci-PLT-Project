vartype Num {
    int | float
}

func main() int {
    Num num = 2.5;
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