package Datastructures.Theory;

import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Inconsistency;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Management.Monitor;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

import java.util.ArrayList;

import static org.junit.Assert.*;

public class EquivalenceClassesTest {
    int type = ClauseType.EQUIV.ordinal();
    @Test
    public void addBasicEquivalenceClause1() {
        System.out.println("Add Basic Equivalence Clause no overlaps");
        Symboltable symboltable = new Symboltable(10);
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"a");
        symboltable.setName(5,"b");
        symboltable.setName(6,"c");
        Model model = new Model(10,symboltable);
        EquivalenceClasses eqClasses = new EquivalenceClasses(model,"test",null);
        int[] clause = new int[]{1,type,1,2,3};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("1 = 2 = 3",eqClasses.toString());
        assertEquals("1 = 2 = 3 [1]",eqClasses.infoString(null));
        assertEquals("p = q = r",eqClasses.toString(symboltable));
        assertEquals("p = q = r [1]",eqClasses.infoString(symboltable));

        clause = new int[]{2,type,4,-5,-6};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}

        assertEquals("1 = 2 = 3\n" +
                "4 = -5 = -6",eqClasses.toString());
        assertEquals("1 = 2 = 3 [1]\n" +
                "4 = -5 = -6 [2]",eqClasses.infoString(null));
        assertEquals("p = q = r\n" +
                "a = -b = -c",eqClasses.toString(symboltable));
        assertEquals("p = q = r [1]\n" +
                "a = -b = -c [2]",eqClasses.infoString(symboltable));

        assertEquals(1,eqClasses.getRepresentative(2));
        assertEquals(-1,eqClasses.getRepresentative(-2));
        assertEquals(-4,eqClasses.getRepresentative(5));
        assertEquals(4,eqClasses.getRepresentative(-5));

        assertEquals("[1]",eqClasses.getOrigins(3).toString());
        assertEquals("[2]",eqClasses.getOrigins(5).toString());
    }

    @Test
    public void addBasicEquivalenceClause2() {
        System.out.println("Add Basic Equivalence Clause with overlaps");
        Symboltable symboltable = new Symboltable(10);
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"a");
        symboltable.setName(5,"b");
        symboltable.setName(6,"c");
        Model model = new Model(10,symboltable);
        EquivalenceClasses eqClasses = new EquivalenceClasses(model,"test",null);
        int[] clause = new int[]{1,type,2,3,4};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}

        clause = new int[]{2,type,-5,6,2}; // 2 overlaps
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("2 = 3 = 4 = -5 = 6",eqClasses.toString());
        assertEquals("2 = 3 = 4 = -5 = 6 [1, 2]",eqClasses.infoString(null));

        clause = new int[]{3,type,6,7,1}; // 2 overlaps
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("1 = 2 = 7 = 3 = 4 = -5 = 6",eqClasses.toString());
        assertEquals("1 = 2 = 7 = 3 = 4 = -5 = 6 [3, 1, 2]",eqClasses.infoString(null));
    }

    @Test
    public void addBasicEquivalenceClause3() {
        System.out.println("Add Basic Equivalence Clause with model ");
        Symboltable symboltable = new Symboltable(10);
        symboltable.setName(1, "p");
        symboltable.setName(2, "q");
        symboltable.setName(3, "r");
        symboltable.setName(4, "a");
        symboltable.setName(5, "b");
        symboltable.setName(6, "c");
        Model model = new Model(10, symboltable);
        IntArrayList orig1 = new IntArrayList();
        orig1.add(20);
        IntArrayList orig2 = new IntArrayList();
        orig2.add(30);
        model.addImmediately(2, orig1);
        model.addImmediately(-3, orig2);
        ArrayList<Object> observed = new ArrayList<>();
        model.addObserver(null,
                ((literal, originals) -> {
                    observed.add(literal);
                    observed.add(originals);}));
        EquivalenceClasses eqClasses = new EquivalenceClasses(model, "test", null);

        int[] clause = new int[]{1,type,5,2,4};
        try{
            eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("",eqClasses.toString());
        assertEquals("[5, [1], 4, [1]]",observed.toString());
        observed.clear();
        clause = new int[]{2,type,6,7,-4};
        try{
            eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("[-6, [2], -7, [2]]",observed.toString());
        observed.clear();
        clause = new int[]{3,type,8,-6,-2};
        try{
            eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {
            System.out.println(uns.toString());}}

    @Test
    public void addBasicEquivalenceClause4() {
        System.out.println("Add Basic Equivalence Clause internal error ");
        Model model = new Model(10, null);
        EquivalenceClasses eqClasses = new EquivalenceClasses(model, "test", null);
        int[] clause = new int[]{1,type,2,3,4,3};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("2 = 3 = 4",eqClasses.toString());
        clause = new int[]{2,type,2,3,4};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("2 = 3 = 4",eqClasses.toString());

        clause = new int[]{3,type,5,3,4};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("2 = 3 = 4 = 5 [1, 3]",eqClasses.infoString(null));
        clause = new int[]{4,type,6,3,-4};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {
            System.out.println(uns.toString());}}

    @Test
    public void integrateTrueLiteral() {
        System.out.println("Integrate True Literal ");
        Model model = new Model(10, null);
        ArrayList<Object> observed = new ArrayList<>();
        model.addObserver(null,
                ((literal, originals) -> {
                    observed.add(literal);
                    observed.add(originals);}));
        EquivalenceClasses eqClasses = new EquivalenceClasses(model, "test", null);
        int[] clause = new int[]{1,type,2,3,4};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        clause = new int[]{2,type,5,-6};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        IntArrayList origins = new IntArrayList(); origins.add(20);
        try{eqClasses.integrateTrueLiteral(6,origins);}
        catch(Unsatisfiable uns) {}
        assertEquals("[-5, [2, 20], 6, [2, 20]]",observed.toString());
        assertEquals("2 = 3 = 4 [1]",eqClasses.infoString(null));
        assertEquals("-5 @ [2, 20]\n" +
                "6 @ [2, 20]",model.infoString(false));


    }

    @Test
    public void threadtest() {
        System.out.println("Thread ");
        Model model = new Model(10, null);
        EquivalenceClasses eqClasses = new EquivalenceClasses(model, "test", null);
        int[] clause1 = new int[]{1,type,2,3,4};
        int[] clause2 = new int[]{2,type,5,6,7};
        try{eqClasses.addBasicEquivalenceClause(clause1);
            eqClasses.addBasicEquivalenceClause(clause2);}
            catch(Unsatisfiable uns) {}

        Result[] result = new Result[]{null};
        Thread thread1 = new Thread(()->result[0] = eqClasses.run());
        thread1.start();

        Thread thread2 = new Thread(() -> {
            IntArrayList origins = new IntArrayList(); origins.add(10);
            eqClasses.addDerivedEquivalence(2,-5,origins);
            System.out.println("DONE");
            try{Thread.sleep(1000);}catch(Exception ex) {}});
        thread2.start();

        try{thread1.join(); thread2.join();} catch(Exception ex) {}
        System.out.println(result[0].toString());
    }

    }