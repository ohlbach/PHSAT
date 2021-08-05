package Datastructures.Theory;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import com.sun.deploy.security.WSeedGenerator;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import javafx.util.Pair;
import org.junit.Test;

import java.util.ArrayList;

import static org.junit.Assert.*;

public class EquivalenceClassTest {

    @Test
    public void equivalenceClass1() {
        System.out.println("EquivalenceClass positives");
        Symboltable symboltable = new Symboltable(5);
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        IntArrayList literals = new IntArrayList();
        IntArrayList orig = new IntArrayList(); orig.add(10); orig.add(20);
        literals.add(2);
        literals.add(1);
        literals.add(3);
        EquivalenceClass eqClass = new EquivalenceClass(literals,orig);
        assertEquals("1 = 2 = 3", eqClass.toNumbers());
        assertEquals("1 = 2 = 3", eqClass.toString(null));
        assertEquals("1 = 2 = 3 [10, 20]", eqClass.infoString(null));
        assertEquals("p = q = r", eqClass.toString(symboltable));
        assertEquals("p = q = r [10, 20]", eqClass.infoString(symboltable));
        assertEquals(1 , eqClass.representative);
        assertEquals("[2, 3]",eqClass.literals.toString());
        assertEquals(1,eqClass.contains(1));
        assertEquals(-1,eqClass.contains(-1));
    }

    @Test
    public void equivalenceClass2() {
        System.out.println("EquivalenceClass negatives");
        Symboltable symboltable = new Symboltable(5);
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        IntArrayList literals = new IntArrayList();
        literals.add(2); literals.add(3); literals.add(-1);
        IntArrayList orig1 = new IntArrayList(); orig1.add(10); orig1.add(20);
        EquivalenceClass eqClass = new EquivalenceClass(literals,orig1);
        assertEquals("1 = -2 = -3", eqClass.toNumbers());
        assertEquals("1 = -2 = -3", eqClass.toString(null));
        assertEquals("1 = -2 = -3 [10, 20]", eqClass.infoString(null));
        assertEquals("p = -q = -r", eqClass.toString(symboltable));
        assertEquals("p = -q = -r [10, 20]", eqClass.infoString(symboltable));
        assertEquals(1 , eqClass.representative);
        assertEquals("[-2, -3]",eqClass.literals.toString());
        assertEquals("[10, 20]",eqClass.origins.toString());

        assertEquals(-1,eqClass.contains(2));
        assertEquals(+1,eqClass.contains(-2));
        assertEquals(0,eqClass.contains(4));
    }

    @Test
    public void addLiteral() {
        System.out.println("addLiteral");
        IntArrayList literals = new IntArrayList();
        literals.add(20); literals.add(10); literals.add(30);
        IntArrayList orig1 = new IntArrayList();
        orig1.add(10); orig1.add(20);
        EquivalenceClass eqClass = new EquivalenceClass(literals, orig1);
        IntArrayList orig3 = new IntArrayList();
        orig3.add(40); orig3.add(41);
        try {eqClass.addLiteral(50, orig3);} catch (Unsatisfiable uns) {}
        assertEquals("10 = 20 = 30 = 50 [10, 20, 40, 41]", eqClass.infoString(null));
        IntArrayList orig4 = new IntArrayList();
        orig4.add(50);orig4.add(51);
        try {eqClass.addLiteral(5, orig4);} catch (Unsatisfiable uns) {}
        assertEquals("5 = 20 = 30 = 50 = 10 [10, 20, 40, 41, 50, 51]",
                eqClass.infoString(null));
        try {eqClass.addLiteral(-10, orig4);}
        catch (Unsatisfiable uns) {System.out.println(uns.toString());}}

    @Test
    public void addEquivalenceClass1() {
        System.out.println("addEquivalenceClass");
        IntArrayList literals = new IntArrayList();
        literals.add(2); literals.add(3); literals.add(4);
        IntArrayList orig = new IntArrayList();
        orig.add(10); orig.add(20);
        EquivalenceClass eqClass1 = new EquivalenceClass(literals, orig);

        literals = new IntArrayList(); orig = new IntArrayList();
        literals.add(5); literals.add(6); literals.add(7);
        orig.add(30); orig.add(40);
        EquivalenceClass eqClass2 = new EquivalenceClass(literals, orig);
        orig = new IntArrayList(); orig.add(50);
        try{
            EquivalenceClass eqClass = eqClass1.addEquivalenceClass(eqClass2, -1,orig);}
        catch(Unsatisfiable uns) {}
        assertEquals("2 = 3 = 4 = -5 = -6 = -7 [10, 20, 50, 30, 40]",
                eqClass1.infoString(null));}

    @Test
    public void addEquivalenceClass2() {
        System.out.println("addEquivalenceClass 2");
        IntArrayList literals = new IntArrayList();
        literals.add(2); literals.add(3); literals.add(4);
        IntArrayList orig = new IntArrayList();
        orig.add(10); orig.add(20);
        EquivalenceClass eqClass1 = new EquivalenceClass(literals, orig);

        literals = new IntArrayList(); orig = new IntArrayList();
        literals.add(5); literals.add(6); literals.add(1);
        orig.add(30); orig.add(40);
        EquivalenceClass eqClass2 = new EquivalenceClass(literals, orig);
        orig = new IntArrayList(); orig.add(50);
        try{
            EquivalenceClass eqClass = eqClass1.addEquivalenceClass(eqClass2, 1,orig);}
        catch(Unsatisfiable uns) {}
        assertEquals("1 = 3 = 4 = 2 = 5 = 6 [10, 20, 50, 30, 40]",
                eqClass1.infoString(null));}

    @Test
    public void addEquivalenceClass3() {
        System.out.println("addEquivalenceClass unsat");
        IntArrayList literals = new IntArrayList();
        literals.add(2); literals.add(3); literals.add(4);
        IntArrayList orig = new IntArrayList();
        orig.add(10); orig.add(20);
        EquivalenceClass eqClass1 = new EquivalenceClass(literals, orig);

        literals = new IntArrayList(); orig = new IntArrayList();
        literals.add(5); literals.add(3); literals.add(-4);
        orig.add(30); orig.add(40);
        EquivalenceClass eqClass2 = new EquivalenceClass(literals, orig);
        orig = new IntArrayList(); orig.add(50);
        try{
            EquivalenceClass eqClass = eqClass1.addEquivalenceClass(eqClass2,1, orig);}
        catch(Unsatisfiable uns) {
            System.out.println(uns.toString());
        }}

}