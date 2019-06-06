import org.junit.Test;
import Utilities.*;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 04.06.2019.
 */
public class PHSatTest {
    @Test
    public void main() throws Exception {
        String[] args = new String[]{Utilities.resourceFile("Walker.phs")};
        //args = new String[]{"help"};
        PHSat.main(args);

    }

}