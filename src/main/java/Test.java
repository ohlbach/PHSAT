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
        for(int i = 0; i < 10; ++i) {
            switch(i) {
                case 1: continue;
                case 8: return;}
            System.out.println(i);
        }

}}
