package ProblemGenerators;

import Datastructures.Clauses.InputClauses;
import Management.Parameter;
import Management.Parameters;
import Utilities.FileIterator;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;

/**
 * This class is for reading cnf-files.
 * 
 * A standard cnf-file has the following structure:<br>
 * # comment<br>
 * # comment<br>
 * % ignored lines
 * p cnf predicates clauses [symbolic]<br>
 * clause1 0 <br>
 * clause2 0 <br>
 * ...<br>
 * A clause is a blank or comma-separated list of predicates (positive or negative numbers /= 0, or names).
 * <br>
 * An extension of this format may contain clauses beginning with special characters:.<br>
 *
 * '&#38;':  means and:          '&#38; 3 4 5' stands for 3 and 4 and 5.<br>
 * 'e':  means equivalences: 'e 4 5 -6' means that these three predicates are equivalent.<br>
 * '&lt;=': means atleast:      '&lt;= 2 p q r' means atleast two of p,q,r are true.<br>
 * '&gt;=': means atleast:      '&gt;= 2 p q r' means atmost two of p,q,r are true.<br>
 * '=':  means exactly:      '= 2 p q r' means exactly two of p,q,r are true.<br>
 * '[min,max]':  means interval: '[2,4] p q r s t' means between 2 and 4 of p,q,r,s,t are true.<br>
 * No special symbol means 'or' 3 4 5' stands for 3 or 4 or 5.
 */
public final class CNFReader extends ProblemGenerator {


    /** the cnf file */
    private final File file;

    /**
     * Creates a Parameters object for opening a file.
     *
     * @return the created Parameters object
     */
    public static Parameters makeParameter() {
        Parameter file = new Parameter("CNF-File", Parameter.Type.File, null,null,
                """
                CNFReader for reading CNF-Files.
                The parameters are:
                  files:       A single filename or a comma-separated list of filenames.
                  directories: A single directory name or a comma-separated list of directory names.
                               All files in the directory ending with .cnf are loaded, unless regExpr is defined.
                  regExprs:    A regular expression to select files in the directory.

                A standard cnf-file has the following structure:
                 # comment
                 # comment
                 % ignored lines
                 p cnf predicates ...
                 clause1 0
                 clause2 0
                 ...
                 A clause is a blank or comma-separated list of literals (positive or negative numbers /= 0, or symbols).
                \s
                 An extension of this format may contain clauses beginning with special characters:.
                 '&':  means and:          '& 3 4 5'     stands for 3 and 4 and 5.
                 'e':  means equivalence:  'e 4 5 -6'    means that these three literals are equivalent.
                 '>=': means atleast:      '>= 2 p q r'  means at least two of p,q,r are true.
                 '<=': means atmost:       '<= 2 p q r'  means at most two of p,q,r are true.
                 [min,max]: means interval '[2,3] p,q,r' means between 2 and 3 of p,q,r are true.
                 '=':  means exactly:      '= 2 p q r'   means exactly two of p,q,r are true.
                No special symbol means 'or': 'p,q,r' means p or q or r""");
        Parameters parameters = new Parameters("CNF-Reader");
        Parameter selected = new Parameter("Select",Parameter.Type.Button,"false",false,
                "Select the Clause File Generator");
        parameters.add(selected);
        parameters.add(file);
        parameters.setOperation((Parameters params, StringBuilder errors) -> {
            ArrayList<ProblemGenerator> generators = new ArrayList<>();
            makeProblemGenerators(params, generators);
            ArrayList<InputClauses> clauses = new ArrayList<>();
            for(ProblemGenerator generator : generators) {
                clauses.add(generator.generateProblem(errors));}
            return clauses;});
        parameters.setDescription("Loads a single file or all files in a directory.");

        return parameters;
    }
        /** creates a CNFReader for the given file
         *
         * @param file a cnf file.
         */
    public CNFReader(File file) {
        this.file = file;}

    /**
     * Generates and adds new problem generators based on the provided parameters.
     *
     * @param parameters The parameters containing the values necessary to create the problem generators.
     * @param generators The list of problem generators to add the newly created generators to.
     */
    public static void makeProblemGenerators(Parameters parameters,ArrayList<ProblemGenerator> generators) {
        Object files = parameters.parameters.get(1).value;
        if(files != null) {
            for(File file :(ArrayList<File>)files)
                generators.add(new CNFReader(file));}}


    /** reads the cnf-file and generates a InputClauses
     *
     * @param errors    for error massages
     * @return null or the new InputClauses.
     */
    public InputClauses generateProblem(StringBuilder errors) {
        InputClauses inputClauses;
        String problemId = file.getName();
        try{
            inputClauses = parseClauses(problemId,new FileIterator(file.getAbsolutePath()),errors);}
        catch (FileNotFoundException ex){
            errors.append("CNFReader: File ").append(file).append(" was not found");
            return null;}
        if(inputClauses == null) return null;
        inputClauses.problemId = problemId;
        return inputClauses;}

    /** turns the file and, if already filled, the InputClauses into a string.
     *
     * @return the filename and, if already filled, the InputClauses
     */
    public String toString() {
        String st = "CNFReader for file " + file.getAbsolutePath();
        if(inputClauses != null) st += "\n"+ inputClauses;
        return st;}


}
