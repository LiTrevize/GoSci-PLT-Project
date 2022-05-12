func main() int {
    int a = 0;
    int b = 1;

    for (b = 0; b < 5;b = b + 1) {
        switch (/*true*/) {
            case false:
                print("false");

            default:
                print(b);
        }

        switch (a + b) {
            case 2, 4, 6:
                print("even");
            case 1, 3, 5, 7:
                print("odd");
            default:
                print("oops");
                fallthrough;
        }
    }

    return 0;
}