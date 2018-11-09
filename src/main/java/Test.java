import com.sun.org.glassfish.gmbal.Description;

import javax.swing.plaf.basic.BasicIconFactory;
import java.lang.reflect.Field;
import java.util.BitSet;
import java.util.TreeSet;
import java.util.function.Function;

/**
 * Created by ohlbach on 23.10.2018.
 */
public class Test {

    public static void main(String[] args) throws Exception {
        TreeSet<Integer> a = new TreeSet<>();
        a.add(1);
        System.out.println(a.remove(0));
    }

}
