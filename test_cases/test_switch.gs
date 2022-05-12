func main() int {
    int a = 0;
    int b = 1;

    for (; b < 10 ;b = b + 1) {
        switch(a = b + 2; a) {
            case 1, 2, 3:
                print("a");
            case 4:
                print("f");
                fallthrough;
            case 6, 7, 8:
                print("b");
                print("c");
            case 9:
                print("d");
            case 10:
                print("e");
                break;
                b = 0;
        }
    }

    return 0;
}