package Solvers;

import Coordinator.CentralProcessor;
import Coordinator.Processor;
import Coordinator.Task;
import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Result;
import Datastructures.Statistics.CentralProcessorStatistics;
import Datastructures.Theory.ImplicationNode;
import Solvers.RandomWalker.WalkerCommunicative;
import Solvers.RandomWalker.WalkerIsolated;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/** This is the superclass of all solver classes.
 * Created by ohlbach on 09.10.2018.
 */
public abstract class Solver extends Processor {

    /* Static data and methods
       *********************** */

    /** the list of all solver types */
    public static String[] solvers = new String[]{"walker isolated", "walker communicative","resolution"};

    /** checks if the name is a solver name
     *
     * @param name  a string
     * @return true if the name is the name of a solver.
     */
    public static boolean isSolver(String name) {
        for(String solver : solvers) {if(name.equals(solver)) {return true;}}
        return false;}


    /** maps the solver names to the solver classes.
     * This method must be extended when a new solver class is added.
     *
     * @param name a solver name
     * @return the solver class, or null
     */
    public static Class solverClass(String name) {
        switch (name) {
            case "walker isolated":      return WalkerIsolated.class;
            case "walker communicative": return WalkerCommunicative.class;
            case "resolution":           return Solvers.Resolution.Resolution.class;
            default: return null;}}

    /** collects all the help-strings for all solver classes
     *
     * @return the collected help string for all solver classes
     */
    public static String help() {
        StringBuilder st = new StringBuilder();
        st.append("The following solver types are available:\n");
        for(String solver : solvers) {
            st.append(solver).append(":\n");
            st.append(help(solver)).append("\n");}
        return st.toString();}

    /** returns the help-string for the generator with the given name
     *
     * @param name a generator name
     * @return its help-string
     */
    public static String help(String name) {
        Class clazz = solverClass(name);
        if(clazz == null) {return "Unknown Solver Class: " +name;}
        try{
            Method helper = clazz.getMethod("help");
            return (String)helper.invoke(null);}
        catch(Exception ex) {ex.printStackTrace();System.exit(1);}
        return null;}


    /** parses the string-type parameters into sequences of objects
     *
     * @param name       the generator name
     * @param parameters a key-value map with parameters as strings
     * @param errors     for collecting error messages
     * @param warnings   for collecting warning messages
     * @return           a list of key-value maps where the values are objects.
     */
    public static ArrayList<HashMap<String,Object>> parseParameters(String name, HashMap<String,String> parameters,
                                                                    StringBuffer errors, StringBuffer warnings) {
        Class clazz = solverClass(name);
        if(clazz == null) {errors.append("Unknown solver class: " + name+"\n"); return null;}
        try{
            Method parser = clazz.getMethod("parseParameters",HashMap.class,StringBuffer.class, StringBuffer.class);
            return (ArrayList<HashMap<String,Object>>)parser.invoke(null,parameters,errors,warnings);}
        catch(Exception ex) {ex.printStackTrace();System.exit(1);}
        return null;}

    /** constructs a new solver of the given type
     *
     * @param type             the solver type
     * @param solverParameters a key-value map with parameters as strings
     * @param centralProcessor the central processor
     * @return           a new solver
     */
    public static Solver construct(String type,HashMap<String,Object> solverParameters, CentralProcessor centralProcessor) {
        Class clazz = solverClass(type);
        try{
            Constructor constructor = clazz.getConstructor(HashMap.class,CentralProcessor.class);
            return (Solver)constructor.newInstance(solverParameters,centralProcessor);}
        catch(Exception ex) {ex.printStackTrace();System.exit(1);}
        return null;}


    /* Instance data and methods
       *************************  */

    /** all solvers can report intermediate results to the centralProcess, and observe new results there.*/
    protected CentralProcessor centralProcessor;

    protected boolean debug1 = false;
    protected boolean debug2 = false;

    /** constructs a solver as an instance of the Processor class.
     *
     * @param solverParameters    the control parameters for the solver
     * @param centralProcessor the central processor.
     */
    public Solver(HashMap<String,Object> solverParameters, CentralProcessor centralProcessor) {
        super((String)solverParameters.get("name"),centralProcessor.supervisor,solverParameters,centralProcessor.basicClauseList);
        this.centralProcessor = centralProcessor;}

    public Solver() {
        super();}


    /** This observer is called when a literal has become true.
     * It inserts a TrueLiteral task into centralProcessor's task queue.
     */
    protected Consumer<Integer> trueLiteralObserverToCentral =
            literal -> {centralProcessor.addTask(new Task.TrueLiteral(literal,centralProcessor));
                ((CentralProcessorStatistics)centralProcessor.statistics).CP_UnitClausesReceived++;};

    /** This observer is called when a new implication is inserted into the implicationDAG
     *  It inserts BinaryClause tasks locally and in the centralProcessor's task queue.
     */
    protected BiConsumer<ImplicationNode,ImplicationNode> implicationObserverBoth =
            (from, to) -> {addTask(new Task.BinaryClause(-from.literal,to.literal,this));
                centralProcessor.addTask(new Task.BinaryClause(-from.literal,to.literal,centralProcessor));
                ((CentralProcessorStatistics)centralProcessor.statistics).CP_ImplicationsReceived++;};

    /** This observer is called when a new equivalence class is derived in the implicationDAG
     *  It inserts Equivalence tasks locally and in the centralProcessor's task queue.
     */
    protected Consumer<int[]> equivalenceObserverBoth =
            equivalence -> {addTask(new Task.Equivalence(equivalence,this));
                centralProcessor.addTask(new Task.Equivalence(equivalence,centralProcessor));
                ((CentralProcessorStatistics)centralProcessor.statistics).CP_EquivalencesReceived++;};

    /** This observer is called when a literal is removed from a clause.
     *  It inserts a task locally and a LiteralRemoval task in the centralProcessor's task queue.
     */
    protected Consumer<CLiteral> literalRemovalObserverBoth =
            cLiteral    -> {
                Clause clause = cLiteral.clause;
                addTask(makeShortenedClauseTask(clause,this));
                if(clause.input)
                    centralProcessor.addTask(new Task.LiteralRemoval(clause.id,cLiteral.literal,centralProcessor));
                    ((CentralProcessorStatistics)centralProcessor.statistics).CP_ShortenedClausesReceived++;};

    /** This method adds observers to the local process and the centralProcessor.
     * The centralProcessor is observed and may insert tasks in the local task queue
     */
    protected void addObservers() {
        // these observers cause local tasks to be generated
        centralProcessor.model.addTrueLiteralObserver(trueLiteralObserver);
        centralProcessor.implicationDAG.addImplicationObserver(implicationObserver);
        centralProcessor.implicationDAG.addEquivalenceObserver(equivalenceObserver);

        model.addTrueLiteralObserver(trueLiteralObserverToCentral);
        clauses.addLiteralRemovalObserver(literalRemovalObserverBoth);
        clauses.addPurityObserver(purityObserver);
        implicationDAG.addTrueLiteralObserver(trueLiteralObserver);
        implicationDAG.addImplicationObserver(implicationObserverBoth);
        implicationDAG.addEquivalenceObserver(equivalenceObserverBoth);}

    /** This method removes the observers from the centralProcessor.
     *  The local observer automatically become garbage at the end.
     */
    protected void removeObservers() {
        centralProcessor.model.removeTrueLiteralObserver(trueLiteralObserver);
        centralProcessor.implicationDAG.removeImplicationObserver(implicationObserver);
        centralProcessor.implicationDAG.removeEquivalenceObserver(equivalenceObserver);
    }


    /** The key method, which has to be implemented by the solvers.
     * It is supposed to find a model or a contradiction in the clauses.
     *
     * @return Un/Satisfiable or null
     */
    public abstract Result solve();











}
