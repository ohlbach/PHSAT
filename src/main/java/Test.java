import java.util.function.Function;

/**
 * Created by ohlbach on 23.10.2018.
 */
public class Test {

    Test(int n) {this.n = n;}

    int n;

    Function<Integer,Integer> plus  = m -> m + n;

    public static void main(String[] args) {
        Test a = new Test(5);
        System.out.println(a.plus.apply(7));
    }

}
