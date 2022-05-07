vartype Num {
    int | float
}

func main() int {
    Num num;
    match (v := num) {
        case int:
            print(v*2);
        case float:
            print(v+1.0);
        default:
            print("no value");
    }
    print("end");
    return 0;
}