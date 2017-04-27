import java.util.*;

class vex {
    static int gather_negs(ArrayList<Integer> input, ArrayList<Integer> output) {
        int count = 0;
        for (int val: input) {
            if (val < 0) { output.add(-val); count++; }
        }
        return count;
    }

    public static void main(String[] args) {
        Integer values[] = { -1, 2, 3, -4, -5, -6, -7, -8, -9, 10};
        ArrayList<Integer> input = new ArrayList<Integer>();
        ArrayList<Integer> myvector = new ArrayList<Integer>(input);
        myvector.addAll(Arrays.asList(values));

        int count = gather_negs(myvector, myvector);

        System.out.print("myvector contains:");
        for (Integer i: myvector) {
            System.out.print(" " + i);
        }
        System.out.println("");
    }
}
