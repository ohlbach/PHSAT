package Management;

import Datastructures.Clauses.Quantifier;
import junit.framework.TestCase;

public class ValueTypeTest extends TestCase {


    public void testStrings() {
        System.out.println("Strings");
        StringBuilder errors = new StringBuilder();
        ValueType type = new ValueType.Strings();
        assertTrue(type instanceof ValueType.Strings);
        assertEquals("test",type.parseValue("test",errors).toString());

        type = new ValueType.Strings(0);
        String[] parts = (String[])type.parseValue("test1 test2,test3",errors);
        assertEquals(3,parts.length);
        assertEquals("test1", parts[0]);
        assertEquals("test2", parts[1]);
        assertEquals("test3", parts[2]);

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
        type.parseValue(" Flase ", errors);
        System.out.println(errors.toString());

        type = new ValueType.Booleans(true);
        boolean[] bool =  (boolean[])type.parseValue(" false,  true ", errors);
        assertEquals(2, bool.length);
        assertTrue(bool[1]);
        assertFalse(bool[0]);

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
        type.parseValue(" 3a ",errors);
        System.out.println(errors.toString());

        type = new ValueType.Integers(true);
        int[] nums = (int[]) type.parseValue("1 2, 3,4 5", errors);
        assertEquals(5, nums.length);
        assertEquals(1, nums[0]);
        assertEquals(2, nums[1]);
        assertEquals(3, nums[2]);
        assertEquals(4, nums[3]);
        assertEquals(5, nums[4]);

        errors = new StringBuilder();
        type.parseValue("1 2, 3,4c 5", errors);
        System.out.println(errors.toString());

        type = new ValueType.Integers(-1,+1,true);
        nums = (int[]) type.parseValue("1,-1", errors);
        assertEquals(2, nums.length);
        assertEquals(1, nums[0]);
        assertEquals(-1, nums[1]);

        errors = new StringBuilder();
        nums = (int[]) type.parseValue("1,-1,3", errors);
        System.out.println(errors.toString());

        type = new ValueType.Integers(-1,+5,true);
        nums = (int[]) type.parseValue("1,-1, 3", errors);
        assertEquals(3, nums.length);
        assertEquals(1, nums[0]);
        assertEquals(-1, nums[1]);
        assertEquals(3, nums[2]);

        errors = new StringBuilder();
        type.parseValue("1,-1, 30", errors);
        System.out.println(errors.toString());

        type = new ValueType.Integers(-1,+5,false);
        int n = (Integer)type.parseValue("0", errors);
        assertEquals(0, n);

        errors = new StringBuilder();
        type.parseValue("1,-1, 30", errors);
        System.out.println(errors.toString());
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


    }
    }