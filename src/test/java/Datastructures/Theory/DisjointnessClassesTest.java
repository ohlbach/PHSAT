package Datastructures.Theory;

import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Management.Monitor;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

import java.util.ArrayList;

import static org.junit.Assert.*;

public class DisjointnessClassesTest {

    StringBuffer errors = new StringBuffer();
    StringBuffer warnings = new StringBuffer();
    boolean monitoring = true;

    int type = ClauseType.DISJOINT.ordinal();
    int typeEQ = ClauseType.EQUIV.ordinal();

    @Test
    public void addDisjointnessObserver() {
    }

    @Test
    public void run() {
    }

    @Test
    public void addDisjointnessClause() {
    }

    @Test
    public void addTrueLiteral() {
    }

    @Test
    public void addDerivedDisjoints() {
    }

    @Test
    public void addEquivalenceClass() {
    }

    @Test
    public void integrateDisjointnessClass() {
    }

    @Test
    public void integrateDisjointnessClause1() {
        System.out.println("integrateDisjointnessClause1");
        Monitor monitor = !monitoring ? null : new Monitor(null,"mixed",errors,warnings);
        Symboltable symboltable = new Symboltable(10);
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        Model model = new Model(10,symboltable);
        EquivalenceClasses eqClasses = new EquivalenceClasses(model,"test",monitor);
        DisjointnessClasses dClasses = new DisjointnessClasses(model,eqClasses,"test",monitor);
        int[] clause = new int[]{1,type,1,2,3};
        try{
        dClasses.integrateDisjointnessClause(clause,null);}
        catch(Unsatisfiable uns) {}
        assertEquals("Disjointness Classes:\n" +
                "p != q != r",dClasses.toString("",symboltable));
        //System.out.println(dClasses.infoString(symboltable));
        assertTrue(dClasses.areDisjoint(1,3));
        assertFalse(dClasses.areDisjoint(1,-3));
        assertFalse(dClasses.areDisjoint(1,4));
        assertEquals("[1]",dClasses.getOrigins(1,2).toString());

        clause = new int[]{2,type,4,2,-4};
        try{
            dClasses.integrateDisjointnessClause(clause,null);}
        catch(Unsatisfiable uns) {}
        assertEquals("Disjointness Classes:\n" +
                "p != q != r\n" +
                "4 != q",dClasses.toString("",symboltable));
        System.out.println(dClasses.infoString(null));
        clause = new int[]{2,type,5,3,5};
        try{
            dClasses.integrateDisjointnessClause(clause,null);}
        catch(Unsatisfiable uns) {
            System.out.println(uns.toString());
        }
    }

    @Test
    public void integrateDisjointnessClause2() {
        System.out.println("integrateDisjointnessClause with model");
        Monitor monitor = !monitoring ? null : new Monitor(null, "mixed", errors, warnings);
        Symboltable symboltable = new Symboltable(10);
        symboltable.setName(1, "p");
        symboltable.setName(2, "q");
        symboltable.setName(3, "r");
        Model model = new Model(10, symboltable);
        ArrayList<Object> observed = new ArrayList<>();
        model.addObserver(null,
                ((literal, originals) -> {
                    observed.add(literal);
                    observed.add(originals);
                }));
        IntArrayList origins = new IntArrayList();
        origins.add(20);
        origins.add(30);
        model.addImmediately(2, origins);
        EquivalenceClasses eqClasses = new EquivalenceClasses(model, "test", monitor);
        DisjointnessClasses dClasses = new DisjointnessClasses(model, eqClasses, "test", monitor);
        int[] clause = new int[]{1, type, 1, 2, 3};
        try {dClasses.integrateDisjointnessClause(clause, null);
        } catch (Unsatisfiable uns) {}
        assertEquals("[-1, [1], -3, [1]]",observed.toString());
    }

    @Test
    public void integrateDisjointnessClause3() {
        System.out.println("integrateDisjointnessClause with equivalences");
        Monitor monitor = !monitoring ? null : new Monitor(null, "mixed", errors, warnings);
        Symboltable symboltable = new Symboltable(10);
        symboltable.setName(1, "p");
        symboltable.setName(2, "q");
        symboltable.setName(3, "r");
        Model model = new Model(10, symboltable);
        EquivalenceClasses eqClasses = new EquivalenceClasses(model, "test", monitor);
        int[] clause = new int[]{1,typeEQ,3,2,1};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        clause = new int[]{2,type,4,2,5};
        DisjointnessClasses dClasses = new DisjointnessClasses(model, eqClasses, "test", monitor);
        try {dClasses.integrateDisjointnessClause(clause, null);
        } catch (Unsatisfiable uns) {}
        assertEquals("Disjointness Classes:\n" +
                "4 != p != 5",dClasses.toString("",symboltable));
        //System.out.println(dClasses.infoString(symboltable));
        }

    @Test
    public void forwardSubsumption() {
        System.out.println("forwardSubsumption");
        Monitor monitor = !monitoring ? null : new Monitor(null, "mixed", errors, warnings);
        Symboltable symboltable =  null;
        Model model = new Model(10, symboltable);
        EquivalenceClasses eqClasses = new EquivalenceClasses(model, "test", monitor);
        DisjointnessClasses dClasses = new DisjointnessClasses(model, eqClasses, "test", monitor);
        int[] clause1 = new int[]{1, type, 1, 2, 3, 4};
        int[] clause2 = new int[]{2, type, 3,2,1};
        try {
            dClasses.integrateDisjointnessClause(clause1, null);
            dClasses.integrateDisjointnessClause(clause2, null);
        } catch (Unsatisfiable uns) {}
        assertEquals("Disjointness Classes:\n" +
                "1 != 2 != 3 != 4",dClasses.toString());
        //System.out.println(dClasses.infoString(symboltable));

        int[] clause = new int[]{3,typeEQ,1,5};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        clause2 = new int[]{4, type, 3,2,5};
        try {
            dClasses.integrateDisjointnessClause(clause2, null);
        } catch (Unsatisfiable uns) {}
        assertEquals("Disjointness Classes:\n" +
                "1 != 2 != 3 != 4",dClasses.toString());
    }
    @Test
    public void backwardSubsumption() {
        System.out.println("backwardSubsumption");
        Monitor monitor = !monitoring ? null : new Monitor(null, "mixed", errors, warnings);
        Symboltable symboltable =  null;
        Model model = new Model(10, symboltable);
        EquivalenceClasses eqClasses = new EquivalenceClasses(model, "test", monitor);
        DisjointnessClasses dClasses = new DisjointnessClasses(model, eqClasses, "test", monitor);
        int[] clause1 = new int[]{1, type, 1, 2, 3, 4};
        int[] clause2 = new int[]{2, type, 3,2,1};
        try {
            dClasses.integrateDisjointnessClause(clause2, null);
            dClasses.integrateDisjointnessClause(clause1, null);
        } catch (Unsatisfiable uns) {}
        assertEquals("Disjointness Classes:\n" +
                "1 != 2 != 3 != 4",dClasses.toString());
        //System.out.println(dClasses.infoString(symboltable));

        int[] clause = new int[]{3,typeEQ,1,5};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        clause2 = new int[]{4, type, 3,2,5,4,6};
        try {
            dClasses.integrateDisjointnessClause(clause2, null);
        } catch (Unsatisfiable uns) {}
        assertEquals("Disjointness Classes:\n" +
                "3 != 2 != 1 != 4 != 6",dClasses.toString());
    }


        @Test
    public void integrateTrueLiteral() {
    }

    @Test
    public void integrateEquivalence() {
    }
}