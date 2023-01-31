package ProblemGenerators;

import Datastructures.Clauses.InputClauses;
import Management.GlobalParameters;
import Management.Monitor.Monitor;
import Utilities.StringIterator;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by ohlbach on 27.08.2018.
 * This problemGenerator creates InputClauses from a string representation of the clauses.
 * This is basically for test purposes.
 */
public final class StringClauseSetGenerator extends ProblemGenerator {

    /** the name of the problem. */
    private final String name;

    /** a String of clauses in the extended cnf-normal form */
    private final String clauses;

    /** constructs a new StringClauseSetGenerator with parameters name and clauses .
     *
     * @param name    the name of the problem.
     * @param clauses the clauses in extended cnf-normal form.
     */
    public StringClauseSetGenerator(String name, String clauses) {
        this.name = name;
        this.clauses = clauses;}



    /** generates a StringClauseSetGenerator with the parameters 'name' and 'clauses'.
     * The parameters are taken unchanged from 'parameters'.
     *
     * @param parameters   contains the keys "name" (optional) and "clauses" (required).
     * @param globalParameters not used.
     * @param generators for adding the new StringClauseSetGenerator.
     * @param errors    for adding error messages.
     * @param warnings  no effect.
     */
    public static void makeProblemGenerator(HashMap<String,String> parameters, GlobalParameters globalParameters,
                                       ArrayList<ProblemGenerator> generators,
                                       StringBuilder errors, StringBuilder warnings){
        assert parameters != null;
        String clauses = parameters.get("clauses");
        if(clauses == null) {errors.append("StringClauseSetGenerator.makeProblemGenerator: no clauses provided\n");}
        else generators.add(new StringClauseSetGenerator(parameters.get("name"),clauses.trim()));}




    /** generates a help string
     *
     * @return a help string
     */
    public static String help() {
        return "StringClauseSetGenerator just parses a string of clauses in extended cnf-normal form.\n" +
                "It may have an arbitrary number of header lines. \n"+
                "The main section has to start with\n" +
                "p cnf 'number of predicates'\n" +
                "A clause is just a single line of literals.\n" +
                "The literals in the clause may by any strings, possibly preceded by -.\n" +
                "A clause starting with a special symbol indicates a special meaning of the clause:\n" +
                "& ...     conjunctions (all literals must be true).\n" +
                "e ...     equivalence  (all literals are equivalent).\n" +
                "<= n ...  at most n literals are true\n"+
                ">= n ...  at least n literals are true\n"+
                "= n ...   exaclty n literals are true\n"+
                "[n,m] ... interval (between n and m literal are true).\n"+
                "l1,...    or-clause\n"+
                "The literals may be names or integers != 0\n"+
                "Each clause may have a '0' at the end.";}

    /** parses the clause string and generates a BasicClauseList object.
     *
     * @param errorMonitor   for error messages
     * @return  true if there was no error.
     */
    @Override
    public InputClauses generateProblem(Monitor errorMonitor) {
        StringBuilder errors   = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        InputClauses inputClauses = parseClauses(name,new StringIterator(clauses,"\n"),errors,warnings);
        if(errorMonitor != null) {
            if(warnings.length() > 0) {
                errorMonitor.println("Warnings when parsing clauses "+name + "\n"+errors.toString());}
            if(errors.length() > 0) {
                errorMonitor.println("Errors when parsing clauses "+name + "\n"+errors.toString()); return null;}}
        return inputClauses;
    }


    /** returns the name and the clauses.
     *
     * @return the name and the clauses
     */
    public String toString() {
        return "StringClauseSetGenerator for " + name + "\nClauses:\n"+clauses;}







}
