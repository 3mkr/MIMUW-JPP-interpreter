int incBy10 (int x) {
    x = x + 10;
    return x;
}

void main() {
    int x = 9;
    printf([x], [ ], [ ], "x equals: %d");                  // "x equals: 9"
    string y = incBy10(x);
    printf([y], [ ], [ ], "y equals: %d");                  // "y equals: 19"
    printf([x], [ ], [ ], "x equals: %d (did not change)"); // "x equals: 9 (did not change)"
}