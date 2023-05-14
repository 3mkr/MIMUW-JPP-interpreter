void main() {
    for (i := 0 to 10) {
        i = i * 2;          // Error because i is read-only
        print(i);
    }
}