int incBy8(int x) {
    x = x + 8;
    return x;
}

int incBy2(int x) {
    x = x + 2;
    return x;
}

int incBy10(int x) {
    x = incBy8(x);
    x = incBy2(x);
    return x;
}

int main() {
    int x = 11;
    int y = incBy10(x);
    print(x);               // x == 11
    print(y);               // y == 21
}