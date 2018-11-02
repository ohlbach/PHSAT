package Solvers.RandomWalker;

import Generators.StringClauseSetGenerator;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Objects;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 02.11.2018.
 */
public class WalkerTest {
    @Test
    public void parseParameters() throws Exception {
        System.out.println("parseParameters");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        parameters.put("seed","3,4");
        parameters.put("flips","10000");
        parameters.put("jumps", "10,20");
        ArrayList<HashMap<String,Object>> pars = Walker.parseParameters(parameters,errors,warnings);
        assertEquals("[{seed=3, flips=10000, name=W1, jumps=10}, {seed=4, flips=10000, name=W2, jumps=10}, {seed=3, flips=10000, name=W3, jumps=20}, {seed=4, flips=10000, name=W4, jumps=20}]",pars.toString());
    }

    @Test
    public void help() throws Exception {
        System.out.println(Walker.help());
    }

    @Test
    public void solve() throws Exception {

    }

    @Test
    public void addObservers() throws Exception {

    }

    @Test
    public void removeObservers() throws Exception {

    }

    @Test
    public void updateClause() throws Exception {

    }

    @Test
    public void getOccurrences() throws Exception {

    }

    @Test
    public void combinedScore() throws Exception {

    }

    @Test
    public void flip() throws Exception {

    }

    @Test
    public void selectFlipPredicate() throws Exception {

    }

    @Test
    public void isFalse() throws Exception {

    }

    @Test
    public void findOtherTrueLiteral() throws Exception {

    }

    @Test
    public void changeScore() throws Exception {

    }

    @Test
    public void updateScores() throws Exception {

    }

}