package Datastructures.TwoLiteral;

import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.DisjointnessClasses;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import Management.Monitor;
import org.junit.Test;

import static org.junit.Assert.*;

public class TwoLitClausesTest {

    StringBuffer errors = new StringBuffer();
    StringBuffer warnings = new StringBuffer();
    boolean monitoring = true;

    int type = ClauseType.OR.ordinal();
    int typeEQ = ClauseType.EQUIV.ordinal();

    @Test
    public void addBasicClause() {
        System.out.println("addBasicClause");
        Monitor monitor = !monitoring ? null : new Monitor(null,"mixed",errors,warnings);
        Symboltable symboltable = new Symboltable(10);
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        Model model = new Model(10,symboltable);
        EquivalenceClasses eqClasses = new EquivalenceClasses(model,"test",monitor);
        DisjointnessClasses dClasses = new DisjointnessClasses(model,eqClasses,"test",monitor);

        TwoLitClauses clauses = new TwoLitClauses("test",model,eqClasses,dClasses,monitor);
        int[] clause1 = new int[]{1,type,2,3};
        int[] clause2 = new int[]{1,type,3,5};
        try{clauses.integrateBasicClause(clause1,null);
            clauses.integrateBasicClause(clause2,null);}
        catch(Unsatisfiable uns) {}
        assertEquals("Two-Literal Clauses of problem test\n" +
                "  q,r\n" +
                "  r,5",clauses.toString());
        //System.out.println(clauses.infoString(symboltable));

    }
}