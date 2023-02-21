package NormalFormTransformers;

import Datastructures.Clauses.Connective;
import junit.framework.TestCase;

import java.util.Arrays;

public class CNFTransformerTest extends TestCase {
    private int cOr = Connective.OR.ordinal();
    private final static int cAtleast = Connective.ATLEAST.ordinal();
    private final static int cAtmost = Connective.ATMOST.ordinal();
    private final static int cExactly = Connective.EXACTLY.ordinal();
    private final static int cInterval = Connective.INTERVAL.ordinal();

    public void testAtleast() {
        System.out.println("atleast to CNF");
        int[] ids = new int[]{9};
        CNFTransformer cnf = new CNFTransformer(() -> ++ids[0]);
        int[] clause = new int[]{10, cAtleast, 2, 1, 2, 3, 4, 5};
        cnf.reset(clause);
        StringBuilder disjunctions = new StringBuilder();
        while (cnf.hasNext()) {
            disjunctions.append(Arrays.toString(cnf.next())).append("\n");
        }
        assertEquals("[10, 0, 1, 2, 3, 4]\n" +
                "[11, 0, 1, 2, 3, 5]\n" +
                "[12, 0, 1, 2, 4, 5]\n" +
                "[13, 0, 1, 3, 4, 5]\n" +
                "[14, 0, 2, 3, 4, 5]\n", disjunctions.toString());

        clause = new int[]{20, cAtleast, 3, 1, 2, 3, 4};
        cnf.reset(clause);
        disjunctions = new StringBuilder();
        while (cnf.hasNext()) {
            disjunctions.append(Arrays.toString(cnf.next())).append("\n");
        }
        assertEquals("[15, 0, 1, 2]\n" +
                "[16, 0, 1, 3]\n" +
                "[17, 0, 1, 4]\n" +
                "[18, 0, 2, 3]\n" +
                "[19, 0, 2, 4]\n" +
                "[20, 0, 3, 4]\n", disjunctions.toString());
    }

    public void testAtmost() {
        System.out.println("atmost to CNF");
        int[] ids = new int[]{9};
        CNFTransformer cnf = new CNFTransformer(() -> ++ids[0]);
        int[] clause = new int[]{10, cAtleast, 2, 1, 2, 3, 4, 5};
        cnf.reset(clause);
        StringBuilder disjunctions = new StringBuilder();
        while (cnf.hasNext()) {
            disjunctions.append(Arrays.toString(cnf.next())).append("\n");
        }
        assertEquals("[10, 0, 1, 2, 3, 4]\n" +
                "[11, 0, 1, 2, 3, 5]\n" +
                "[12, 0, 1, 2, 4, 5]\n" +
                "[13, 0, 1, 3, 4, 5]\n" +
                "[14, 0, 2, 3, 4, 5]\n", disjunctions.toString());

        clause = new int[]{10, cAtmost, 3, -1, -2, -3, -4, -5};
        cnf.reset(clause);
        disjunctions = new StringBuilder();
        while (cnf.hasNext()) {
            disjunctions.append(Arrays.toString(cnf.next())).append("\n");
        }
        assertEquals("[15, 0, 1, 2, 3, 4]\n" +
                "[16, 0, 1, 2, 3, 5]\n" +
                "[17, 0, 1, 2, 4, 5]\n" +
                "[18, 0, 1, 3, 4, 5]\n" +
                "[19, 0, 2, 3, 4, 5]\n", disjunctions.toString());
    }

    public void testExactly() {
        System.out.println("exactly to CNF");
        int[] ids = new int[]{9};
        CNFTransformer cnf = new CNFTransformer(() -> ++ids[0]);
        int[] clause = new int[]{10, cAtleast, 2, 1, 2, 3, 4, 5};
        cnf.reset(clause);
        StringBuilder disjunctions = new StringBuilder();
        while (cnf.hasNext()) {
            disjunctions.append(Arrays.toString(cnf.next())).append("\n");}
        assertEquals("[10, 0, 1, 2, 3, 4]\n" +
                "[11, 0, 1, 2, 3, 5]\n" +
                "[12, 0, 1, 2, 4, 5]\n" +
                "[13, 0, 1, 3, 4, 5]\n" +
                "[14, 0, 2, 3, 4, 5]\n", disjunctions.toString());

        clause = new int[]{10, cAtmost, 2, 1, 2, 3, 4, 5};
        cnf.reset(clause);
        disjunctions = new StringBuilder();
        while (cnf.hasNext()) {
            disjunctions.append(Arrays.toString(cnf.next())).append("\n");}
        assertEquals("[15, 0, -1, -2, -3]\n" +
                "[16, 0, -1, -2, -4]\n" +
                "[17, 0, -1, -2, -5]\n" +
                "[18, 0, -1, -3, -4]\n" +
                "[19, 0, -1, -3, -5]\n" +
                "[20, 0, -1, -4, -5]\n" +
                "[21, 0, -2, -3, -4]\n" +
                "[22, 0, -2, -3, -5]\n" +
                "[23, 0, -2, -4, -5]\n" +
                "[24, 0, -3, -4, -5]\n", disjunctions.toString());

        clause = new int[]{10, cExactly, 2, 1, 2, 3, 4, 5};
        cnf.reset(clause);
        disjunctions = new StringBuilder();
        while (cnf.hasNext()) {
            disjunctions.append(Arrays.toString(cnf.next())).append("\n");}
        assertEquals("[25, 0, 1, 2, 3, 4]\n" +
                "[26, 0, 1, 2, 3, 5]\n" +
                "[27, 0, 1, 2, 4, 5]\n" +
                "[28, 0, 1, 3, 4, 5]\n" +
                "[29, 0, 2, 3, 4, 5]\n" +
                "[30, 0, -1, -2, -3]\n" +
                "[31, 0, -1, -2, -4]\n" +
                "[32, 0, -1, -2, -5]\n" +
                "[33, 0, -1, -3, -4]\n" +
                "[34, 0, -1, -3, -5]\n" +
                "[35, 0, -1, -4, -5]\n" +
                "[36, 0, -2, -3, -4]\n" +
                "[37, 0, -2, -3, -5]\n" +
                "[38, 0, -2, -4, -5]\n" +
                "[39, 0, -3, -4, -5]\n", disjunctions.toString());
    }

    public void testInterval() {
        System.out.println("interval to CNF");
        int[] ids = new int[]{9};
        CNFTransformer cnf = new CNFTransformer(() -> ++ids[0]);
        int[] clause = new int[]{10, cAtleast, 2, 1, 2, 3, 4, 5};
        cnf.reset(clause);
        StringBuilder disjunctions = new StringBuilder();
        while (cnf.hasNext()) {
            disjunctions.append(Arrays.toString(cnf.next())).append("\n");
        }
        assertEquals("[10, 0, 1, 2, 3, 4]\n" +
                "[11, 0, 1, 2, 3, 5]\n" +
                "[12, 0, 1, 2, 4, 5]\n" +
                "[13, 0, 1, 3, 4, 5]\n" +
                "[14, 0, 2, 3, 4, 5]\n", disjunctions.toString());

        clause = new int[]{10, cAtmost, 3, 1, 2, 3, 4, 5};
        cnf.reset(clause);
        disjunctions = new StringBuilder();
        while (cnf.hasNext()) {
            disjunctions.append(Arrays.toString(cnf.next())).append("\n");
        }
        assertEquals("[15, 0, -1, -2, -3, -4]\n" +
                "[16, 0, -1, -2, -3, -5]\n" +
                "[17, 0, -1, -2, -4, -5]\n" +
                "[18, 0, -1, -3, -4, -5]\n" +
                "[19, 0, -2, -3, -4, -5]\n", disjunctions.toString());

        clause = new int[]{10, cInterval, 2, 3, 1, 2, 3, 4, 5};
        cnf.reset(clause);
        disjunctions = new StringBuilder();
        while (cnf.hasNext()) {
            disjunctions.append(Arrays.toString(cnf.next())).append("\n");
        }
        assertEquals("[20, 0, 1, 2, 3, 4]\n" +
                "[21, 0, 1, 2, 3, 5]\n" +
                "[22, 0, 1, 2, 4, 5]\n" +
                "[23, 0, 1, 3, 4, 5]\n" +
                "[24, 0, 2, 3, 4, 5]\n" +
                "[25, 0, -1, -2, -3, -4]\n" +
                "[26, 0, -1, -2, -3, -5]\n" +
                "[27, 0, -1, -2, -4, -5]\n" +
                "[28, 0, -1, -3, -4, -5]\n" +
                "[29, 0, -2, -3, -4, -5]\n", disjunctions.toString());

    }
    public void testRedundancies() {
        System.out.println("multiple literals");
        int[] ids = new int[]{9};
        CNFTransformer cnf = new CNFTransformer(() -> ++ids[0]);
        int[] clause = new int[]{10, cInterval, 2, 3, 1, 2, 3,4,5};
        cnf.reset(clause);
        StringBuilder disjunctions = new StringBuilder();
        while (cnf.hasNext()) {
            disjunctions.append(Arrays.toString(cnf.next())).append("\n");
        }
        assertEquals("[10, 0, 1, 2, 3, 4]\n" +
                "[11, 0, 1, 2, 3, 5]\n" +
                "[12, 0, 1, 2, 4, 5]\n" +
                "[13, 0, 1, 3, 4, 5]\n" +
                "[14, 0, 2, 3, 4, 5]\n" +
                "[15, 0, -1, -2, -3, -4]\n" +
                "[16, 0, -1, -2, -3, -5]\n" +
                "[17, 0, -1, -2, -4, -5]\n" +
                "[18, 0, -1, -3, -4, -5]\n" +
                "[19, 0, -2, -3, -4, -5]\n", disjunctions.toString());

        clause = new int[]{10, cInterval, 2, 3, 1, 1, 3,4,5};
        cnf.reset(clause);
        disjunctions = new StringBuilder();
        while (cnf.hasNext()) {
            disjunctions.append(Arrays.toString(cnf.next())).append("\n");
        }
        assertEquals("[20, 0, 1, 3, 4]\n" +
                "[21, 0, 1, 3, 5]\n" +
                "[22, 0, 1, 4, 5]\n" +
                "[23, 0, 1, 3, 4, 5]\n" +
                "[24, 0, 1, 3, 4, 5]\n" +
                "[25, 0, -1, -3, -4]\n" +
                "[26, 0, -1, -3, -5]\n" +
                "[27, 0, -1, -4, -5]\n" +
                "[28, 0, -1, -3, -4, -5]\n" +
                "[29, 0, -1, -3, -4, -5]\n", disjunctions.toString());

        clause = new int[]{10, cInterval, 2, 3, 1, 2, 1,4,2};
        cnf.reset(clause);
        disjunctions = new StringBuilder();
        while (cnf.hasNext()) {
            disjunctions.append(Arrays.toString(cnf.next())).append("\n");
        }
        assertEquals("[30, 0, 1, 2, 4]\n" +
                "[31, 0, 1, 2]\n" +
                "[32, 0, 1, 2, 4]\n" +
                "[33, 0, 1, 4, 2]\n" +
                "[34, 0, 2, 1, 4]\n" +
                "[35, 0, -1, -2, -4]\n" +
                "[36, 0, -1, -2]\n" +
                "[37, 0, -1, -2, -4]\n" +
                "[38, 0, -1, -4, -2]\n" +
                "[39, 0, -2, -1, -4]\n", disjunctions.toString());

    }
    }