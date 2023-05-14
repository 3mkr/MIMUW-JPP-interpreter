void main() {
    for (i := 1 to 3) {
        if (i == 2) {
            print("Earlier end of loop");   
            continue;                       // Will end if's and for's block instantly
            print("Never printed");         // Won't ever print because of continue
        }
        print("Entire loop");
    }
}
/*
Expected output:
    VString "Entire loop"
    VString "Earlier end of loop"
    VString "Entire loop"
*/