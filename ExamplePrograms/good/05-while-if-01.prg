void main() {
    int i = 1;
    while (i < 10) {
        print(i);
        i = i * 2;
        if (i == 8) {
            print("x == 8");
        }
        if (i == 42) {
            print("x == 42");
        }
    }
}