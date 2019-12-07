package Datastructures.Literals;

import Datastructures.Clauses.Clause;
import Utilities.BucketSortedIndex;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 06.12.2019.
 */
public class LitAlgorithmsTest {


    private Clause make(int id,BucketSortedIndex<CLiteral<Clause>> literalIndex,int... literals) {
        Clause cl = new Clause(Integer.toString(id),literals.length);
        int i = -1;
        for(int l:literals) {
            CLiteral lit = new CLiteral(l,cl,++i);
            literalIndex.add(lit);
            cl.add(lit);}
        return cl;}

        private BucketSortedIndex<CLiteral<Clause>> makeIndex(int predicates) {
            return
                new BucketSortedIndex<CLiteral<Clause>>(predicates,
                    (cLiteral->cLiteral.literal),
                    (cLiteral->cLiteral.clause.size()));}

    @Test
    public void isSubsumed() throws Exception {
        System.out.println("isSubsumed");
        BucketSortedIndex<CLiteral<Clause>> index = makeIndex(10);
        Clause c1 = make(1,index, 1,2,3);
        Clause c2 = make(2,index,3,2,1);
        assertEquals(c2,LitAlgorithms.isSubsumed(c1,index,1));
    }

    @Test
    public void subsumes() throws Exception {

    }

    @Test
    public void replacementResolutionBackwards() throws Exception {

    }

    @Test
    public void replacementResolutionForward() throws Exception {

    }

    @Test
    public void contains() throws Exception {

    }

    @Test
    public void resolve() throws Exception {

    }

}