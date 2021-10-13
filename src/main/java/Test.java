import java.util.Comparator;
import java.util.concurrent.PriorityBlockingQueue;

/**
 * Created by ohlbach on 23.10.2018.
 */
public class Test {
    public static class Tes {
        int i;
        public Tes(int i) { this.i =i;}
        public int getP() {return i;}}

    public static void main(String[] args) throws InterruptedException{
        PriorityBlockingQueue<Tes> q = new PriorityBlockingQueue<Tes>(5, Comparator.comparingInt(Tes::getP));
        q.add(new Tes(10));
        q.add(new Tes(2));

        Tes a = q.take();
        System.out.println(a.i);

    }

}
