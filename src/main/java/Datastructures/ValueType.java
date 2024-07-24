package Datastructures;

import Datastructures.Clauses.Quantifier;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;

/**
 * This class with its subclasses describe the potential values of some the parameters for the QSat system.
 */
public abstract class ValueType {

    /** can be used to check the parsed value string.*/
    public Function<Object,Boolean> check = null;

    /** parses a string. The value depends on the ValueType and its parameters.
     *
     * @param input  a string to be parsed.
     * @param errors for appending error messages.
     * @return the parsed object.
     */
    public abstract Object parseValue(String input, StringBuilder errors);

    /** sets a final check for the parsers
     *
     * @param check a function (object, errors) -> true (okay) / false (error)
     */
    public void setCheck(Function<Object,Boolean> check) { this.check = check; }

    /** turns the parameter value into a string
     *
     * @param object a parameter value
     * @return the value as a string.
     */
    public String toString(Object object) {
        if(object == null) return "";
        if(object instanceof IntArrayList) {
            String st = object.toString();
            return st.substring(1,st.length()-1);}
        if(object.getClass().isArray()) {
            String st = Arrays.toString((Object[])object);
            return st.substring(1,st.length()-1);}
        return object.toString();}

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
            if(check != null) {
                for (String part : parts) {if(!check.apply(part)) return null;}}
            return (size == 1) ? parts[0] : parts;}}

    /** This class represents either a single boolean value, or the two boolean values */
    public static class Booleans extends ValueType {

        /** one or two boolean values*/
        private final boolean list;

        /** a single boolean value*/
        public Booleans() {
            list = false;}

        /** two boolean values
         * @param list if true then both booleans are allowed.*/
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
                return booleans.length == 1 ? booleans[0] : booleans;}
            if (parts[0].equalsIgnoreCase("true")) {return true;}
            if (parts[0].equalsIgnoreCase("false")) {return false;}
            errors.append("Type Booleans: unknown boolean value: " + parts[0] + "\n");
            return null;}

        /** turns the parameter value into a string
         *
         * @param object a parameter value
         * @return the value as a string.
         */
        @Override
        public String toString(Object object) {
            if(object == null) return "";
            if(object instanceof Boolean) {return object.toString();}
            if(object.getClass().isArray()) {
                String st = Arrays.toString((boolean[])object);
                return st.substring(1,st.length()-1);}
            return object.toString();}
    }

    /** This class represents either a single integer value or a list of int-values.
     * The values may be constrained by minimum and maximum values, or by checkong the allowed values.
     * <br>
     * There are the following possibilities to specify lists like in the examples: <br>
     * 3,6,7 <br>
     * 3 to 10 <br>
     * 3 to 10 step 2 <br>
     * The different possibilities can be mixed comma separated.
     */
    public static class Integers extends ValueType {
        /** if true then a list of int-values is allowed*/
        public boolean list;

        /** smallest int*/
        public int min = Integer.MIN_VALUE;

        /** largest int*/
        public int max = Integer.MAX_VALUE;

        /** list of allowed values*/
        public int[] allowed;

        /** constructs a type where a single, but arbitrary, int-value is allowed*/
        public Integers() {
            this.list = false;
            allowed = null;}

        /** constructs a type where a list if unconstrained int-values is allowed.
         *
         * @param list if true then a list is allowed, if false then a single value is allowed.
         */
        public Integers(boolean list) {
            this.list = list;
            allowed = null;}

        /** constructs a type where a list of constrained int-values is allowed.
         *
         * @param min  the smallest allowed int-value.
         * @param max  the largest allowed int-value.
         * @param list if true then a list is allowed, if false then a single value is allowed.
         */
        public Integers(int min, int max, boolean list) {
            if(max < min) {
                System.err.println("Error ValueType.Inegers: max < min " + max + " < " + min );
                new Exception().printStackTrace();
                System.exit(1);}
            this.list = list;
            this.min = min;
            this.max = max;}

        /** constructs a type where a list of constrained int-values is allowed.
         * <br>
         * max is Integer.MAX_Value
         *
         * @param min  the smallest allowed int-value.
         * @param list if true then a list is allowed, if false then a single value is allowed.
         */
        public Integers(int min, boolean list) {
            this.list = list;
            this.min = min;}


        /** constructs a type where a list of constrained int-values is allowed.
         *
         * @param list    if true then a list is allowed, if false then a single value is allowed.
         * @param allowed the list of allowed values
         */
        public Integers(boolean list, int... allowed) {
            this.list = list;
            this.allowed = allowed;}

        /** parses the input and returns either a single int value or an IntArrayList
         *  There are the following possibilities to specify lists like in the examples: <br>
         *      * 3,6,7 <br>
         *      * 3 to 10 <br>
         *      * 3 to 10 step 2 <br>
         *      * The different possibilities can be mixed comma separated.
         *
         * @param input  a string to be parsed.
         * @param errors for appending error messages.
         * @return a single int value (list = false) or an IntArrayList
         */
        @Override
        public Object parseValue(String input, StringBuilder errors) {
            String[] parts = input.trim().split("\\s*,\\s*");
            IntArrayList values = new IntArrayList(parts.length);
            try{
                for(String part : parts) {
                    if(part.contains("to")) {
                        if(part.contains("step")) {
                            String[] pparts = part.split("\\s*(to|step)\\s*",3);
                            int start = Integer.parseInt(pparts[0]);
                            int end   = Integer.parseInt(pparts[1]);
                            int step  = Integer.parseInt(pparts[2]);
                            for(int i = start; i <= end; i += step) {values.add(i);}
                            continue;}
                        else {String[] pparts = part.split("\\s*to\\s*",2);
                            int start = Integer.parseInt(pparts[0]);
                            int end   = Integer.parseInt(pparts[1]);
                            for(int i = start; i <= end; ++i) {values.add(i);}
                            continue;}}
                    values.add(Integer.parseInt(part));}}

            catch(NumberFormatException e) {
                errors.append("Type Integers: Error when parsing " + input +"\n  "+ e.getMessage()+"\n");
                return null;}
            for (int number : values) {
                if (!(min <= number && number <= max)) {
                    errors.append("Type Integers: Error when parsing " + input + ": number " + number + " is not allowed.\n" +
                            "  Allowed values are between " + min + " and " + max + "\n");
                    return null;}}
            if(allowed != null) {
                for (int number : values) {
                    if (!List.of(allowed).contains(number)) {
                        errors.append("Type Integers:  Error when parsing " + input + ": number " + number + " is not allowed.\n" +
                                "  Allowed values are: " + Arrays.toString(allowed) + "\n");
                        return null;}}}

            if (list) return values;

            if (values.size() != 1) {
                errors.append("Type Integers: Error when parsing " + input + ": exactly one integer must be provided\n");
                return null;}
            return values.getInt(0);}

    }

    /** This class allows one to parse values from an arbitrary Enum-class. */
    public static class Enums extends ValueType {

        /** The Enum class*/
        private final Class enumeration;

        /** if true then a list of Enum-values are allowed*/
        private final boolean list;

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
        private final boolean list;

        /** for a single path */
        public Paths() {
            this.list = false;}

        /** for a list of paths
         *
         * @param list if true then a list of Paths are allowed.*/
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

    /** This class allows one to parse quantified ranges
     * <p>
     * The specifications are allowed as in the following examples: (comma separated)<br>
     *  5    -&gt; [exactly, 5] <br>
     *  &lt; 6  -&gt; [atmost, 5] <br>
     *  &lt;= 6 -&gt; [atmost, 6] <br>
     *  &gt; 6  -&gt; [atleast, 7] <br>
     *  &gt;= 6 -&gt; [atleast, 6] <br>
     *  6 7  -&gt; [interval,6,7] (numbers are blank-separated)*/
    public static class Quantifications extends ValueType {

        /** if true then a list of quantifications is allowed */
        private final boolean list;
        /** optional a smallest number */
        private final int min;
        /** optional a largest number */
        private int max = Integer.MAX_VALUE;
        /** if true then min and max are tested */
        private boolean constrained = false;

        /** constructs the type with list = false and no constraints */
        public Quantifications() {
            list = false;
            min = Integer.MIN_VALUE;}

        /** constructs the type with given list value and no constraints
         *
         * @param list if true then both booleans are allowed*/
        public Quantifications(boolean list) {
            this.list = list;
            min = Integer.MIN_VALUE;}

        /** constructs the type with list = false and constraints
         *
         * @param min smallest value
         * @param max largest value */
        public Quantifications(int min, int max) {
            list = false;
            constrained = true;
            this.min = min;
            this.max = max;}

        /** constructs the type with list = false and min-constraints
         *
         * @param min smallest value */
        public Quantifications(int min) {
            list = false;
            constrained = true;
            this.min = min;}

        /** constructs the type with given list and constraints
         *
         * @param list if true then more quantifications are allowed
         * @param min smallest value
         * @param max largest value */
        public Quantifications(boolean list, int min, int max) {
            this.list = list;
            constrained = true;
            this.min = min;
            this.max = max;}

        /** constructs the type with given list and min-constraints
         *
         * @param list if true then more quantifications are allowed
         * @param min smallest value*/
        public Quantifications(boolean list, int min) {
            this.list = list;
            constrained = true;
            this.min = min;}

        /** This method parses quantified ranges
         * <p>
         * The specifications are allowed as in the following examples: (comma separated)<br>
         *  5    -&gt; [exactly, 5] <br>
         *  &lt; 6  -&gt; [atmost, 5] <br>
         *  &lt;= 6 -&gt; [atmost, 6] <br>
         *  &gt; 6  -&gt; [atleast, 7] <br>
         *  &gt;= 6 -&gt; [atleast, 6] <br>
         *  6 7  -&gt; [interval,6,7] (numbers are blank-separated)
         *
         * @param input  a string to be parsed.
         * @param errors for appending error messages.
         * @return an ArrayList&lt;Object[]&gt; where Object[] starts with a quantifier (OR,EXACTLY, ATLEAST, ATMOST, INTERVAL) followed by numbers
         */
        @Override
        public Object parseValue(String input, StringBuilder errors) {
                input = input.trim();
                ArrayList<Object[]> quantifications = new ArrayList<>();
                String[] parts = input.split("\\s*,\\s*(?![^\\[]*\\])");

                try{
                    for(String part : parts) {
                        if(part.startsWith("[")) {
                            if(part.endsWith("]")) {
                                String[] prt = part.substring(1, part.length() - 1).split("\\s*,\\s*");
                                Object[] cap = new Object[prt.length+1];
                                cap[0] = Quantifier.OR;
                                for(int i = 0; i < prt.length; i++) cap[i+1] = Integer.parseInt(prt[i]);
                                quantifications.add(cap);}
                            else {errors.append("Type Quantification Error: " + part + " starts with [, but does not end with ].\n");
                                return null;}
                        continue;}
                   try{ int n = Integer.parseInt(part);
                        quantifications.add(new Object[]{Quantifier.EXACTLY, n});
                        continue;}                         // just a single number
                    catch(NumberFormatException ignore) {} // more complex
                    int limit;
                    switch(part.charAt(0)) {
                        case '<':
                            if(part.charAt(1) == '=') {
                                 limit = Integer.parseInt(part.substring(2).trim());
                                quantifications.add(new Object[]{Quantifier.ATMOST,limit});
                                break;}
                            else {limit = Integer.parseInt(part.substring(1).trim());
                                quantifications.add(new Object[]{Quantifier.ATMOST,limit-1});}
                            break;

                            case '>':
                                if(part.charAt(1) == '=') {
                                    limit = Integer.parseInt(part.substring(2).trim());
                                    quantifications.add(new Object[]{Quantifier.ATLEAST,limit});
                                    break;
                                }
                                else {limit = Integer.parseInt(part.substring(1).trim());
                                    quantifications.add(new Object[]{Quantifier.ATLEAST,limit+1});}
                                break;
                            default:
                                String[] pparts = part.split("\\s* \\s*",2);
                                int mi = Integer.parseInt(pparts[0]);
                                int ma = Integer.parseInt(pparts[1]);
                                quantifications.add(new Object[]{Quantifier.INTERVAL,mi,ma});}}}
                    catch(NumberFormatException ex) {
                        errors.append("Type IntRange: Error in " + input + "\n " + ex.getMessage() + "\n" );
                        return null;}

                    if(constrained) {
                        for(Object[] quantification : quantifications) {
                            for(int i = 1; i < quantification.length; i++) {
                                int n = (Integer)quantification[i];
                                if(!(min <= n && n <= max)) {
                                    errors.append("Type IntRange: Error in " + input +": number " + n + " is not between "
                                    + min + " and " + max + ".\n");
                                    return null;}}}}

                    if(list) return quantifications;
                    if(quantifications.size() > 1) {
                        errors.append("Type IntRange: Error in " + input + " only one quantification allowed \n");
                        return null;}
                    return quantifications.get(0);}

        /** returns a String representation of the Quantification
         *
         * @param object a quantification
         * @return a String representation of the Quantification. The result can be parsed again.
         */
        @Override
        public String toString(Object object) {
            if(object == null) return "";
            if(object.getClass().isArray()) {
                return toStringQuantification((Object[])object);}
            if(object instanceof ArrayList) {
                ArrayList<Object[]> obj = (ArrayList<Object[]>)object;
                StringBuilder st = new StringBuilder();
                st.append(toStringQuantification(obj.get(0)));
                for(int i = 1; i < obj.size(); i++) {
                    st.append(",").append(toStringQuantification(obj.get(i)));}
                return st.toString();}
            return object.toString();}

        /** turns a single quantification to a string
         *
         * @param quantification a single quantification
         * @return the quantification as a string.
         */
        private String toStringQuantification(Object[] quantification) {
            Quantifier quantifier = (Quantifier) quantification[0];
            StringBuilder st = new StringBuilder();
            switch(quantifier) {
                case OR:
                    st.append("[").append(quantification[1]);
                    for(int i = 2; i < quantification.length; i++) st.append(",").append(quantification[i]);
                    st.append("]");
                    return st.toString();
                case EXACTLY: return ""+quantification[1];
                case ATMOST: return "<= "+quantification[1];
                case ATLEAST: return ">= "+quantification[1];
                case INTERVAL: return ""+quantification[1] + " " + quantification[2];}
        return "";}
    }}
