package Datastructures;

import Datastructures.Clauses.Quantifier;
import Datastructures.Theory.Model;
import junit.framework.TestCase;

import java.util.function.Consumer;
import java.util.function.Function;

public class ClauseListTest extends TestCase {

    static Consumer<String> monitor = (string -> System.out.println(string));
    static Symboltable symboltable = new Symboltable(10);
    static {symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"s");}

    static Function<Integer,Literal> litCreator = (literal) -> new Literal(literal,1);
    static int or = Quantifier.OR.ordinal();
    static int atl = Quantifier.ATLEAST.ordinal();
    static int atm = Quantifier.ATMOST.ordinal();
    static int intv = Quantifier.INTERVAL.ordinal();
    static int and = Quantifier.AND.ordinal();
    static int eqv = Quantifier.EQUIV.ordinal();

    public void testAddClause() {
        System.out.println("addClause");
        ClauseList cl = new ClauseList(true,monitor,true);
        Model model = new Model(10);
        cl.initialize("Test",model,symboltable);
        Clause c1 = new Clause(new int[]{1,or,1,-2,3},true,litCreator,symboltable);
        cl.addClause(c1);
        Clause c2 = new Clause(new int[]{2,intv,2,3,1,-2,1,3,2,2,4},true,litCreator,symboltable);
        cl.addClause(c2);
        System.out.println(cl.toString("",symboltable));
    }

    public void testRemoveClause() {
    }

    public void testAddClauseToIndex() {
    }

    public void testRemoveClauseFromIndex() {
    }

    public void testRemoveLiteralFromIndex() {
    }

    public void testApplyTrueLiteral() {
    }
}