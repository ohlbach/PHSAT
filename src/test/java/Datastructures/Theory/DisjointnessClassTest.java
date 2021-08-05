package Datastructures.Theory;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

import static org.junit.Assert.*;

public class DisjointnessClassTest {

    @Test
    public void constructor() {
        System.out.println("constructor");
        IntArrayList literals = new IntArrayList();
        literals.add(2); literals.add(3);
        IntArrayList origins = new IntArrayList();
        origins.add(20);
        DisjointnessClass dClass = new DisjointnessClass(literals,origins);
        assertEquals("2,3",dClass.toString());
        assertEquals("2,3 [20]",dClass.infoString(null));
        Symboltable st = new Symboltable(10);
        st.setName(1,"p");
        st.setName(2,"q");
        st.setName(3,"r");
        assertEquals("q,r",dClass.toString(st));
        assertEquals("q,r [20]",dClass.infoString(st));
        assertEquals(1,dClass.contains(2));
        assertEquals(-1,dClass.contains(-3));
        assertEquals(0,dClass.contains(-4));
    }


    @Test
    public void addLiteral() {
    }

    @Test
    public void replaceEquivalence() {
        System.out.println("replaceEquivalence");
        IntArrayList literals = new IntArrayList();
        literals.add(2); literals.add(3); literals.add(-4);
        IntArrayList origins = new IntArrayList();
        origins.add(20);
        DisjointnessClass dClass = new DisjointnessClass(literals,origins);
        origins = new IntArrayList();
        origins.add(30);
        try{dClass.replaceEquivalence(5,3,origins);}
        catch(Unsatisfiable uns) {}
        assertEquals("2,5,-4 [20, 30]",dClass.infoString(null));
        origins = new IntArrayList();
        origins.add(40);
        try{assertFalse(dClass.replaceEquivalence(-2,5,origins));}
        catch(Unsatisfiable uns) {}
        assertEquals("2,5,-4 [20, 30]",dClass.infoString(null));
        try{assertFalse(dClass.replaceEquivalence(2,5,origins));}
        catch(Unsatisfiable uns) {
            System.out.println(uns.toString());
        }



    }
}