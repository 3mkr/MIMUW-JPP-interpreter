int main() {
    int[] x = [10, 20, 30];
    print(x);                   // Will print VArr [VInt 10, VInt 20, VInt 30]
    for (i := 0 to 2) {
        x[i] = x[i] + 1;
    }
    print(x);                   // Will print VArr [VInt 11, VInt 21, VInt 31]
    print(x[0]);                // Will print VInt 11
}