package Datastructures;

import Datastructures.Clauses.Quantifier;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import junit.framework.TestCase;

import java.util.ArrayList;
import java.util.Arrays;

public class ValueTypeTest extends TestCase {


    public void testStrings() {
        System.out.println("Strings");
        StringBuilder errors = new StringBuilder();
        ValueType type = new ValueType.Strings();
        assertTrue(type instanceof ValueType.Strings);
        assertEquals("test",type.parseValue("test",errors).toString());

        assertEquals("test", type.toString("test"));

        type = new ValueType.Strings(0);
        String[] parts = (String[])type.parseValue("test1 test2,test3",errors);
        assertEquals(3,parts.length);
        assertEquals("test1", parts[0]);
        assertEquals("test2", parts[1]);
        assertEquals("test3", parts[2]);
        System.out.println(type.toString(parts));
        assertEquals(Arrays.toString(parts),  Arrays.toString((String[])type.parseValue(type.toString(parts),errors)));

        type = new ValueType.Strings(2);
        type.parseValue("test1 test2,test3",errors);
        System.out.println(errors.toString());

    }


    public void testBooleans() {
        System.out.println("Booleans");
        StringBuilder errors = new StringBuilder();
        ValueType type = new ValueType.Booleans();
        boolean b = (Boolean) type.parseValue(" true ", errors);
        assertTrue(b);
        b = (Boolean) type.parseValue(" False ", errors);
        assertFalse(b);
        assertEquals("true", type.toString(true));
        assertEquals("false", type.toString(false));
        System.out.println(type.toString(b));
        type.parseValue(" Flase ", errors);
        System.out.println(errors.toString());

        type = new ValueType.Booleans(true);
        boolean[] bool =  (boolean[])type.parseValue(" false,  true ", errors);
        assertEquals(2, bool.length);
        assertTrue(bool[1]);
        assertFalse(bool[0]);
        assertEquals(Arrays.toString(bool),  Arrays.toString((boolean[])type.parseValue(type.toString(bool),errors)));
        errors = new StringBuilder();
        type.parseValue(" false,  true, false ", errors);
        System.out.println(errors.toString());
    }

    public void testIntegers() {
        System.out.println("Integers");
        StringBuilder errors = new StringBuilder();

        ValueType type = new ValueType.Integers();
        int i = (Integer)type.parseValue(" 3 ",errors);
        assertEquals(3,i);
        assertEquals("3",type.toString(i));
        type.parseValue(" 3a ",errors);
        System.out.println(errors.toString());

        type = new ValueType.Integers(true);
        IntArrayList nums = (IntArrayList) type.parseValue("1, 2, 3,4, 5", errors);
        assertEquals(nums.toString(),  ((IntArrayList)type.parseValue(type.toString(nums),errors)).toString());
        assertEquals("1, 2, 3, 4, 5", type.toString(nums));

        errors = new StringBuilder();
        type.parseValue("1 2, 3,4c 5", errors);
        System.out.println(errors.toString());

        type = new ValueType.Integers(-1,+1,true);
        nums = (IntArrayList) type.parseValue("1,-1", errors);
        assertEquals("[1, -1]", nums.toString());


        errors = new StringBuilder();
        nums = (IntArrayList) type.parseValue("1,-1,3", errors);
        System.out.println(errors.toString());

        type = new ValueType.Integers(-1,+5,true);
        nums = (IntArrayList) type.parseValue("1,-1, 3", errors);
        assertEquals("[1, -1, 3]", nums.toString());


        errors = new StringBuilder();
        type.parseValue("1,-1, 30", errors);
        System.out.println(errors);

        type = new ValueType.Integers(-1,+5,false);
        int n = (Integer)type.parseValue("0", errors);
        assertEquals(0, n);

        errors = new StringBuilder();
        type.parseValue("1,-1, 30", errors);
        System.out.println(errors);

        errors = new StringBuilder();
        type = new ValueType.Integers(0,true);
        nums = (IntArrayList) type.parseValue("1, 5 to 10,22, 30 to 35 step 2", errors);
        assertEquals("[1, 5, 6, 7, 8, 9, 10, 22, 30, 32, 34]", nums.toString());
        assertEquals("1, 5, 6, 7, 8, 9, 10, 22, 30, 32, 34",type.toString(nums));

        type = new ValueType.Integers(1,33,false);
        nums = (IntArrayList) type.parseValue("1, 5 to 10,22, 30 to 35 step 2", errors);
        System.out.println(errors);

    }

    public void testEnums() {
        System.out.println("Enums");
        StringBuilder errors = new StringBuilder();
        ValueType type = new ValueType.Enums(Quantifier.class);
        Quantifier quantifier = (Quantifier) type.parseValue("OR", errors);
        assertEquals(Quantifier.OR, quantifier);

        type = new ValueType.Enums(true,Quantifier.class);
        Enum[] quantifiers = (Enum[]) type.parseValue("OR, AND, EQUIV", errors);
        assertEquals(3, quantifiers.length);
        assertEquals(Quantifier.OR,quantifiers[0]);
        assertEquals(Quantifier.AND, quantifiers[1]);
        assertEquals(Quantifier.EQUIV, quantifiers[2]);
        System.out.println(type.toString(quantifiers));
    }
    public void testQuantifications() {
        System.out.println("Quantifications");
        StringBuilder errors = new StringBuilder();
        ValueType type = new ValueType.Quantifications();
        Object[] quantification = (Object[]) type.parseValue("1", errors);
        assertEquals("1",type.toString(quantification));
        quantification = (Object[]) type.parseValue("< 2", errors);
        assertEquals("<= 1",type.toString(quantification));

        type = new ValueType.Quantifications(true);
        ArrayList <Object[]> quantifications = (ArrayList<Object[]>) type.parseValue("1,2,[3,4],< 5, <= 6, > 7, >= 8, 9 10", errors);
        System.out.println(errors);
        assertEquals("1,2,[3,4],<= 4,<= 6,>= 8,>= 8,9 10",type.toString(quantifications));
        assertEquals(type.toString(quantifications), type.toString(type.parseValue(type.toString(quantifications),errors)));

        type = new ValueType.Quantifications(true,1,3);
        quantifications = (ArrayList<Object[]>) type.parseValue("1,2,[3,4],", errors);
        System.out.println(errors);

    }
    }