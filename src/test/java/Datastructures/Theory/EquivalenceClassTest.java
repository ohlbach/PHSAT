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
        ArrayList<Pair<Integer, IntArrayList>> clause = new ArrayList<>();
        IntArrayList orig1 = new IntArrayList(); orig1.add(10); orig1.add(20);
        clause.add(new Pair(2,orig1));
        clause.add(new Pair(1,null));
        IntArrayList orig2 = new IntArrayList(); orig2.add(30); orig2.add(31);
        clause.add(new Pair(3,orig2));
        EquivalenceClass eqClass = new EquivalenceClass(clause,9);
        assertEquals("1 = 2 = 3", eqClass.toNumbers());
        assertEquals("1 = 2 = 3", eqClass.toString(null));
        assertEquals("1 = 2[9, 10, 20] = 3[9, 30, 31]", eqClass.infoString(null));
        assertEquals("p = q = r", eqClass.toString(symboltable));
        assertEquals("p = q[9, 10, 20] = r[9, 30, 31]", eqClass.infoString(symboltable));
        assertEquals(1 , eqClass.representative);
        assertEquals("[2, 3]",eqClass.literals.toString());
        assertEquals("[9, 10, 20]",eqClass.getOrigins(2).toString());
        assertEquals("[9, 30, 31]",eqClass.getOrigins(3).toString());
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
        ArrayList<Pair<Integer, IntArrayList>> clause = new ArrayList<>();
        IntArrayList orig1 = new IntArrayList(); orig1.add(10); orig1.add(20);
        clause.add(new Pair(2,orig1));
        clause.add(new Pair(3,null));
        IntArrayList orig2 = new IntArrayList(); orig2.add(30); orig2.add(31);
        clause.add(new Pair(-1,orig2));
        EquivalenceClass eqClass = new EquivalenceClass(clause,9);
        assertEquals("1 = -2 = -3", eqClass.toNumbers());
        assertEquals("1 = -2 = -3", eqClass.toString(null));
        assertEquals("1 = -2[30, 31, 9, 10, 20] = -3[30, 31, 9]", eqClass.infoString(null));
        assertEquals("p = -q = -r", eqClass.toString(symboltable));
        assertEquals("p = -q[30, 31, 9, 10, 20] = -r[30, 31, 9]", eqClass.infoString(symboltable));
        assertEquals(1 , eqClass.representative);
        assertEquals("[-2, -3]",eqClass.literals.toString());
        assertEquals("[30, 31, 9, 10, 20]",eqClass.getOrigins(2).toString());
        assertEquals("[30, 31, 9]",eqClass.getOrigins(3).toString());

        assertEquals(-1,eqClass.contains(2));
        assertEquals(+1,eqClass.contains(-2));
        assertEquals(0,eqClass.contains(4));
    }


    @Test
    public void equivalenceClassTwoLits() {
        System.out.println("constructor with two literals");
        IntArrayList origins = new IntArrayList(); origins.add(10);
        EquivalenceClass eqClass = new EquivalenceClass(2,-1,origins);
        assertEquals("1 = -2", eqClass.toNumbers());
        assertEquals("1 = -2[10]", eqClass.infoString(null));
    }
    @Test
    public void addEquivalence() {
        System.out.println("addEquivalence");
        ArrayList<Pair<Integer, IntArrayList>> clause = new ArrayList<>();
        IntArrayList orig1 = new IntArrayList();
        orig1.add(10);
        orig1.add(20);
        clause.add(new Pair(20, orig1));
        clause.add(new Pair(10, null));
        IntArrayList orig2 = new IntArrayList();
        orig2.add(30);
        orig2.add(31);
        clause.add(new Pair(30, orig2));
        EquivalenceClass eqClass = new EquivalenceClass(clause, 9);
        IntArrayList orig3 = new IntArrayList();
        orig3.add(40);
        orig3.add(41);
        try {eqClass.addEquivalence(50, orig3);} catch (Unsatisfiable uns) {}
        assertEquals("10 = 20[9, 10, 20] = 30[9, 30, 31] = 50[40, 41]", eqClass.infoString(null));
        IntArrayList orig4 = new IntArrayList();
        orig4.add(50);
        orig4.add(51);
        try {eqClass.addEquivalence(5, orig4);} catch (Unsatisfiable uns) {}
        assertEquals("5 = 20[9, 10, 20, 50, 51] = 30[9, 30, 31, 50, 51] = 50[40, 41, 50, 51] = 10[50, 51]",
                eqClass.infoString(null));
        try {eqClass.addEquivalence(-10, orig4);}
        catch (Unsatisfiable uns) {System.out.println(uns.toString());}}

    }