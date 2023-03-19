import org.junit.Test;
import Utilities.*;
import QUSat.QUSat;

/**
 * Created by ohlbach on 04.06.2019.
 */
public class QUSatTest {
    @Test
    public void main() throws Exception {
        String[] args = new String[]{Utilities.resourceFile("Resolution.phs")};
        //args = new String[]{"help"};
        QUSat.main(args);

    }

}