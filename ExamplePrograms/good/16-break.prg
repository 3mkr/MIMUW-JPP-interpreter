int main() {
    for (i := 1 to 3) {
        if (i == 2) {
            print("Earlier end of loop");   
            break;                          // Will end for instantly
            print("Never printed");         // Won't ever print because of break
        }
        print("Entire loop");
    }
}
/*
Expected output:
    VString "Entire loop"
    VString "Earlier end of loop"
*/