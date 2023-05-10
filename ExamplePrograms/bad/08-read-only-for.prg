int main() {
    for (i := 0 to 10) {
        print(i);           // Will print (VInt 0) once
        i = i * 2;          // Error because i is read-only
        print(i);
    }
}