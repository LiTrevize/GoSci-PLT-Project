unit km {
    1000.0 m
}

unit g {
    0.001 kg
}

func main() int {
    float g [kg -1][m 3][s -2];
    float r [m];
    float m1 [kg];
    float m2 [kg];
    float f [kg][m][s -2];

    g = 6.67e-11;
    r = 100.0 [km];
    m1 = 10.0;
    m2 = 1.0;
    f = g * m1 * m2 / r / r;
    print(f);

    return 0;
}