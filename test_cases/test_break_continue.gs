func main() int{
    int a = 0;
    int b = 5;
    int c = 10;

    for (a<b) {
        a = a + 1;
        for (b < c) {
            b = b + 1;
            if (b == c - 2) {
                for (a != 5) {
                    if (a < 4) {
                        a = a + 1;
                        continue;
                    } else {
                        break;
                    }
                    continue;
                }
                print(a);
                print(b);
                break;
            }
        }
        print(a);
        print(b);
        if (a + b == 18) {
            return 1;
        }
    }
    return 0;
}