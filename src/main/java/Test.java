import com.sun.org.glassfish.gmbal.Description;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import javafx.util.Pair;

import javax.swing.plaf.basic.BasicIconFactory;
import java.lang.reflect.Field;
import java.util.BitSet;
import java.util.TreeSet;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.function.Function;

/**
 * Created by ohlbach on 23.10.2018.
 */
public class Test {

    public static void main(String[] args) throws Exception {
        final PriorityBlockingQueue<Integer> queue =
                new PriorityBlockingQueue<Integer>(10,
                        (Integer i, Integer j) -> {
                        int s1 = 0;
                        if(i < 0) return -1;
                        else return 1;});
        queue.add(6); queue.add(5);
        System.out.println(queue.toString());
        System.out.println(queue.poll());}

}
