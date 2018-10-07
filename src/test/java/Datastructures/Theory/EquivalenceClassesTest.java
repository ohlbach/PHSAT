package Datastructures.Theory;

import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.ImplicationGraph;
import Datastructures.Theory.Model;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by Ohlbach on 21.09.2018.
 */
public class EquivalenceClassesTest {

/*
    @Test
    public void addEquivalence1() throws Exception {
        System.out.println("addEquivalence no observers");
        Model model = new Model(10);
        ImplicationGraph ig = new ImplicationGraph(10);
        EquivalenceClasses ec = new EquivalenceClasses(model,ig);
        ec.addEquivalence("ec1",1,2);
        assertEquals(1,ec.mapToRepresentative(2));
        assertEquals(1,ec.mapToRepresentative(1));
        assertEquals(3,ec.mapToRepresentative(3));
        ec.addEquivalence("ec2",2,3);
        assertEquals(1,ec.mapToRepresentative(3));
        ec.addEquivalence("ec3",5,6);
        assertEquals(5,ec.mapToRepresentative(6));
    }

    @Test
    public void addEquivalence2() throws Exception {
        System.out.println("addEquivalence with observers");
        Model model = new Model(10);
        ImplicationGraph ig = new ImplicationGraph(10);
        EquivalenceClasses ec = new EquivalenceClasses(model,ig);
        StringBuffer st = new StringBuffer();
        ec.equivalenceObservers.add((lit1,lit2)->st.append(lit1+"="+lit2+"  "));
        ec.addEquivalence("ec1",1,2);
        assertEquals(1,ec.mapToRepresentative(2));
        assertEquals(1,ec.mapToRepresentative(1));
        assertEquals(3,ec.mapToRepresentative(3));
        ec.addEquivalence("ec2",2,3);
        assertEquals(1,ec.mapToRepresentative(3));
        ec.addEquivalence("ec3",5,6);
        assertEquals(5,ec.mapToRepresentative(6));
        assertEquals("1=2  1=3  5=6  ",st.toString());
    }

    @Test
    public void addEquivalenceClass1() throws Exception {
        System.out.println("addEquivalenceClass no observers");
        Model model = new Model(10);
        ImplicationGraph ig = new ImplicationGraph(10);
        EquivalenceClasses ec = new EquivalenceClasses(model,ig);
        ec.addEquivalenceClass(new int[]{11,0,1,2,3});
        assertEquals(1,ec.mapToRepresentative(2));
        assertEquals(1,ec.mapToRepresentative(3));
        model.add(5);
        ec.addEquivalenceClass(new int[]{11,0,4,5,6});
        assertEquals(5,ec.mapToRepresentative(5));
        ec.addEquivalenceClass(new int[]{12,0,1,6,7});
        assertEquals(1,ec.mapToRepresentative(7));
    }

    @Test
    public void addEquivalenceClass2() throws Exception {
        System.out.println("addEquivalenceClass with observers");
        Model model = new Model(20);
        ImplicationGraph ig = new ImplicationGraph(20);
        EquivalenceClasses ec = new EquivalenceClasses(model,ig);
        StringBuffer st = new StringBuffer();
        ec.trueLiteralObservers.add(lit->st.append("T "+lit + "  "));
        ec.unsatisfiabilityObservers.add(us -> st.append(us.getClass().getName()+"  "));
        ec.addEquivalenceClass(new int[]{11,0,1,2,3});
        assertEquals(1,ec.mapToRepresentative(2));
        assertEquals(1,ec.mapToRepresentative(3));
        model.add(5);
        ec.addEquivalenceClass(new int[]{11,0,4,5,6});
        assertEquals(5,ec.mapToRepresentative(5));

        ec.addEquivalenceClass(new int[]{12,0,1,6,-3});

        model.add(-10);
        ec.addEquivalenceClass(new int[]{12,0,10,11});

        assertEquals("T 4  T 6  Datastructures.Results.Unsatisfiable  T -11  ",st.toString());

    }

    @Test
    public void implicationGraph() throws Exception {
        System.out.println("implicationDAG");
        Model model = new Model(10);
        ImplicationGraph ig = new ImplicationGraph(10);
        EquivalenceClasses ec = new EquivalenceClasses(model, ig);
        ig.addImplication(1,2);
        ig.addImplication(2,1);
        assertEquals(2,ec.mapToRepresentative(1));
    }
    */
}