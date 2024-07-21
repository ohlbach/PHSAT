package Management;

import Datastructures.Clauses.Quantifier;

import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;

import static Utilities.Utilities.contains;

/**
 * This class with its subclasses describe the potential values of some the parameters for the QSat system.
 */
public abstract class ValueType {

    /** parses a string. The value depends on the ValueType and its parameters.
     *
     * @param input  a string to be parsed.
     * @param errors for appending error messages.
     * @return the parsed object.
     */
    public abstract Object parseValue(String input, StringBuilder errors);

    /** This class represents string parameters, a single string or a list of strings.*/
    public static class Strings extends ValueType {

        /** the maximum number of strings*/
        private int size;

        /** the allowed values for the strings */
        private String[] allowed;

        /** constructs a Strings type for a single string. */
        public Strings() {
            size = 1;}

        /** constructs a Strings type for a maximum number of strings.
         *
         * @param size the maximum number of strings (size &lt;0 means arbitrary many strings)
         */
        public Strings(int size) {
            if (size <= 0) this.size = Integer.MAX_VALUE;
            else this.size = size;
        }

        /** specifies the allowed strings
         *
         * @param allowed the allowed strings
         */
        public Strings(String... allowed) {
            this.allowed = allowed;}


        /** parses the input and returns either a single String (size = 1) or a String[].
         *
         * @param input  a string to be parsed.
         * @param errors for appending error messages.
         * @return a single String (size = 1) or a String[].
         */
        @Override
        public Object parseValue(String input, StringBuilder errors) {
            input = input.trim();
            String[] parts = input.trim().split("\\s*[, ]\\s*");
            if (allowed != null) {
                for (String part : parts) {
                    if (!Arrays.asList(allowed).contains(part)) {
                        errors.append("Type Strings: parsing " + input + ": string " + part + " is not allowed.\n" +
                                "  Allowed values are: " + Arrays.toString(allowed) + "\n");
                        return null;}}
                return parts;}
            if (parts.length > size) {
                errors.append("Type Strings: too many strings in " + input + ": allowed are " + size + " strings\n");
                return null;}
            return (size == 1) ? parts[0] : parts;}}

    /** This class represents either a single boolean value, or the two boolean values */
    public static class Booleans extends ValueType {

        /** one or two boolean values*/
        private boolean list;

        /** a single boolean value*/
        public Booleans() {
            list = false;}

        /** two boolean values*/
        public Booleans(boolean list) {
            this.list = list;}

        /** parses the input and returns either a single boolean value (list = false) or a boolean[].
         *
         * @param input  a string to be parsed.
         * @param errors for appending error messages.
         * @return a single boolean value (list = false) or a boolean[].
         */
        @Override
        public Object parseValue(String input, StringBuilder errors) {
            input = input.trim();
            String[] parts = input.trim().split("\\s*[, ]\\s*", 2);
            if (list) {
                boolean[] booleans = new boolean[parts.length];
                for (int i = 0; i < parts.length; i++) {
                    if (parts[i].equalsIgnoreCase("true")) {
                        booleans[i] = true; continue;}
                    if (parts[i].equalsIgnoreCase("false")) {
                        booleans[i] = false; continue;}
                    errors.append("Type Booleans: unknown boolean value: " + parts[i] + "\n");
                    return null;}
                return booleans;}
            if (parts[0].equalsIgnoreCase("true")) {return true;}
            if (parts[0].equalsIgnoreCase("false")) {return false;}
            errors.append("Type Booleans: unknown boolean value: " + parts[0] + "\n");
            return null;}}

    /** This class represents either a single integer value or a list of int-values. The values may be constrained in various ways.
     */
    public static class Integers extends ValueType {
        /** if true then a list of int-values is allowed*/
        public boolean list;

        /** if true then the values are constrained*/
        public boolean constrained;

        /** smallest int*/
        public int min;

        /** largest int*/
        public int max;

        /** list of allowed values*/
        public int[] allowed;

        /** constructs a type where a single, but arbitrary, int-value is allowed*/
        public Integers() {
            this.list = false;
            constrained = false;
            allowed = null;}

        /** constructs a type where a list if unconstrained int-values is allowed.
         *
         * @param list if true then a list is allowed, if false then a single value is allowed.
         */
        public Integers(boolean list) {
            this.list = list;
            constrained = false;
            allowed = null;}

        /** constructs a type where a list of constrained int-values is allowed.
         *
         * @param min  the smallest allowed int-value.
         * @param max  the largest allowed int-value.
         * @param list if true then a list is allowed, if false then a single value is allowed.
         */
        public Integers(int min, int max, boolean list) {
            this.list = list;
            this.constrained = true;
            this.min = min;
            this.max = max;}

        /** constructs a type where a list of constrained int-values is allowed.
         *
         * @param allowed the list of allowed values
         * @param list    if true then a list is allowed, if false then a single value is allowed.
         */
        public Integers(boolean list, int... allowed) {
            this.list = list;
            constrained = true;
            this.allowed = allowed;}

        /** parses the input and returns either a single int value or an array int[]
         *
         * @param input  a string to be parsed.
         * @param errors for appending error messages.
         * @return a single boolean value (list = false) or a boolean[].
         */
        @Override
        public Object parseValue(String input, StringBuilder errors) {
            input = input.trim();
            String[] values = input.split("\\s*[, ]\\s*");
            int[] numbers = new int[values.length];
            try {
                for (int i = 0; i < values.length; i++) {
                    numbers[i] = Integer.parseInt(values[i]);}}
            catch (NumberFormatException ex) {
                errors.append("Type Integers: parsing failed: " + ex.getMessage() + "\n");
                return null;}

            if (allowed != null) {
                for (int number : numbers) {
                    if (contains(allowed, number) == -1) {
                        errors.append("Type Integers: parsing " + input + ": number " + number + " is not allowed.\n" +
                                "  Allowed values are: " + Arrays.toString(allowed) + "\n");
                        return null;}}

                if (list) return numbers;

                if (numbers.length > 1) {
                    errors.append("Type Integers: parsing " + input + ": only one number is allowed.\n" +
                            "  Allowed values are: " + Arrays.toString(allowed) + "\n");
                    return null;}}

            if (constrained) {
                for (int number : numbers) {
                    if (!(min <= number && number <= max)) {
                        errors.append("Type Integers: parsing " + input + ": number " + number + " is not allowed.\n" +
                                "  Allowed values are between " + min + " and " + max + "\n");
                        return null;}}
                if (list) return numbers;

                if (numbers.length != 1) {
                    errors.append("Type Integers: parsing " + input + ": exactly one integer must be provided\n");
                    return null;}}

            if (list) return numbers;

            if (numbers.length != 1) {
                errors.append("Type Integers: parsing " + input + ": exactly one integer must be provided\n");
                return null;}

            return numbers[0];}}

    /* This class allows one to parse values from an arbitrary Enum-class. */
    public static class Enums extends ValueType {

        /** The Enum class*/
        private Class enumeration;

        /** if true then a list of Enum-values are allowed*/
        private boolean list;

        /** This class allows one to parse values from an arbitrary Enum-class.
         *
         * @param enumeration the Enum-class
         */
        public <E extends Enum<E>> Enums(Class<E> enumeration) {
            this.list = false;
            this.enumeration = enumeration;}

        /** constructs a type for an arbitrary Enum-class.
         *
         * @param list        if true then a list of enumerations are allowed.
         * @param enumeration the Enum-Class
         * @param <E>         the Enum-Class
         */
        public <E extends Enum<E>> Enums(boolean list, Class<E> enumeration) {
            this.list = list;
            this.enumeration = enumeration;}


        /** This class allows one to parse values from an arbitrary Enum-class.
         *
         * @param input  a string to be parsed.
         * @param errors for appending error messages.
         * @return either a single Enum-object, or an Enum[] array.
         */
        @Override
        public Object parseValue(String input, StringBuilder errors) {
            input = input.trim();
            String[] values = input.split("\\s*[, ]\\s*");
            try {
                if (list) {
                    Enum[] enums = new Enum[values.length];
                    for (int i = 0; i < values.length; i++) {
                        enums[i] = Enum.valueOf(enumeration, values[i].toUpperCase());}
                    return enums;}
                if (values.length > 1) {
                    errors.append("Type Enums: parsing " + input + ": only one enum value is allowed.\n");
                    return null;}
                return Enum.valueOf(enumeration, values[0].toUpperCase());}
            catch (IllegalArgumentException ex) {
                errors.append("Type Enums: unknown enum value: " + input + "\n");
                return null;}}}

    /** This class represents Path parameters, a single path or a list of paths.*/
    public static class Paths extends ValueType {

        /** if true then a list of paths is allowed */
        private boolean list;

        /** for a single path */
        public Paths() {
            this.list = false;}

        /** for a list of paths */
        public Paths(boolean list) {
            this.list = list;}

        /** This class allows one to parse Path values
         *
         * @param input  a string to be parsed.
         * @param errors for appending error messages.
         * @return either a single Path object or an Array Path[].
         */
        @Override
        public Object parseValue(String input, StringBuilder errors) {
            input = input.trim();
            String[] parts = input.split("\\s*[, ]\\s*");
            Path[] paths = new Path[parts.length];
            try {
                for (int i = 0; i < parts.length; i++) {
                    String part = parts[i];
                    paths[i] = Path.of(part);}}
            catch (InvalidPathException ex) {
                errors.append("Type Paths: invalid path: " + input + "\n" + ex.toString() + "\n");
                return null;}

            if (list) return paths;

            if (paths.length > 1) {
                errors.append("Type Paths: parsing " + input + ": only one path is allowed.\n");}
            return null;}}

    /** This class represents Ranges of Integers, like 2,[1,2],&gt;3,&lt;= 5.*/
    public static class IntRanges extends ValueType {

        private boolean list;
        private int min;
        private int max;

        public IntRanges() {
            list = false;
            min = Integer.MIN_VALUE;
            max = Integer.MAX_VALUE;}

        public IntRanges(boolean list) {
            this.list = list;
            min = Integer.MIN_VALUE;
            max = Integer.MAX_VALUE;}



        public IntRanges(int min, int max) {
            list = false;
            this.min = min;
            this.max = max;}

        public IntRanges(boolean list, int min, int max) {
            this.list = list;
            this.min = min;
            this.max = max;}

        /** This class allows one to parse Integer-ranges
         *
         * @param input  a string to be parsed.
         * @param errors for appending error messages.
         * @return an ArrayList<Object[]> where Object[] starts with a quantifier (OR,EXACTLY,INTERVAL) followed by numbers
         */
        @Override
        public Object parseValue(String input, StringBuilder errors) {
                input = input.trim();
                boolean erraneous = false;
                ArrayList<Object[]> capacities = new ArrayList<>();
                String[] parts = input.split("\\s*,\\s*(?![^\\[]*\\])");

                for(String part : parts) {
                    if(part.startsWith("[")) {
                        if(part.endsWith("]")) {
                            String[] prt = part.substring(1, part.length() - 1).split("\\s*,\\s*");
                            Object[] cap = new Object[prt.length+1];
                            cap[0] = Quantifier.OR;
                            for(int i = 0; i < prt.length; i++) {
                                Integer n = parseInt(prt[i],errors);
                                if(n == null) erraneous = true;
                                else cap[i+1] = n;}
                            capacities.add(cap);}
                        else {errors.append("Type IntRange: " + part + " starts with [, but does not end with ].\n");
                            erraneous = true;}
                        continue;}
                    try{
                        Integer n = Integer.parseInt(part);
                        capacities.add(new Object[]{Quantifier.EXACTLY, n});
                        continue;}                         // just a single number
                    catch(NumberFormatException ignore) {} // more complex

                    Integer limit;
                    switch(part.charAt(0)) {
                        case '<':
                            if(part.charAt(1) == '=') {
                                 limit = parseInt(part.substring(2).trim(),errors);}
                            else {limit = parseInt(part.substring(1).trim(),errors);
                                  if(limit != null) limit -= 1;}
                            if(limit == null) {erraneous = true; continue;}
                            capacities.add(new Object[]{Quantifier.INTERVAL,min,limit});
                            break;

                            case '>':
                                if(part.charAt(1) == '=') {
                                    limit = parseInt(part.substring(2).trim(),errors);}
                                else {limit = parseInt(part.substring(1).trim(),errors);
                                    if(limit != null) limit += 1;}
                                if(limit == null) {erraneous = true; continue;}
                                capacities.add(new Object[]{Quantifier.INTERVAL,limit,max});
                                break;
                            default: errors.append("Type IntRange: malformed interval in " + part); erraneous = true;}}
                return erraneous ? null : capacities;}

        /** parses an int and checks the limits min, max
         *
         * @param number the string to be parsed
         * @param errors for appending error messages
         * @return null or the parsed integer
         */
        private Integer parseInt(String number, StringBuilder errors) {
            try{int n = Integer.parseInt(number);
                if(!(min <= n && n <= max)) {
                    errors.append("Type IntRange: number " + number + " is not between " + min + " and " + max + "\n");
                    return null;}
                return n;}
            catch(NumberFormatException ex) {
                errors.append("Type IntRange: number " + number + " is well formed.\n");}
            return null;}}
    }
