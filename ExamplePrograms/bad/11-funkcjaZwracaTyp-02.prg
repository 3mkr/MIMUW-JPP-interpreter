string if10Kot(int q) {
    if (q == 10) {
        return "Kot";
    } else {
        return "Niekot";
    }
}

void main() {
    int x = 10;
    boolean s = if10Kot(x);
    print(s);
}
