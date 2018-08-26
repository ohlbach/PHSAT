package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LiteralGenerator;

import java.util.ArrayList;

/**
 * Created by ohlbach on 26.08.2018.
 *
 * The generator creates clauses by using the given LiteralGenerator.
 * The class can be subclassed to generate more specific clause types.
 */
public class ClauseGenerator {

    private LiteralGenerator literalGenerator;

    /** constructs a ClauseGenerator
     *
     * @param literalGenerator for generating literals
     */
    public ClauseGenerator(LiteralGenerator literalGenerator) {
        this.literalGenerator = literalGenerator;}

    /** generates a new clause
     *
     * @param number the clause number (for enumerating clauses)
     * @param literals the literals for the clause
     * @return a new clause or null, of the clause is a tautology.
     */
    public Clause newClause(int number, ArrayList<Integer> literals) {
        int size = literals.size();
        Clause clause = new Clause(number, size);
        for(int literal : literals) {
            CLiteral cliteral = literalGenerator.newLiteral(literal);
            int status = clause.addLiteral(cliteral);
            if(status < 0) {return null;}} // tautology
        return clause;}

    /** generates an info string
     *
     * @return an info string.
     */
    public String toString() {
        return "The ClauseGenerator creates a Clause by using the LiteralGeberator:\n" + literalGenerator.toString();}

}
