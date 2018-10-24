import com.sun.org.glassfish.gmbal.Description;

import java.lang.reflect.Field;
import java.util.function.Function;

/**
 * Created by ohlbach on 23.10.2018.
 */
public class Test {
    Test(){}

    Test(int n) {this.n = n;}

    @Description("test description")
    public int n;
    public int m;

    public Function<Integer,Integer> plus  = m -> m + n;

    public static void main(String[] args) throws Exception {
        Test b = new Test();
        System.out.println(b.getClass().getField("m").getAnnotation(Description.class).value());
    }

}
