package Solvers.RandomWalker;

import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LiteralIndex;
import Datastructures.Theory.ImplicationNode;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Created by Ohlbach on 01.09.2018.
 */
public class WalkerCommunicative extends Walker {


    LiteralIndex index;

    /** parses a HashMap with key-value pairs<br>
     *
     * @param parameters  the parameters with the keys "seed", "flips", "jumpFrequency"
     * @param errors      for error messages
     * @param warnings    for warnings
     * @return            a list of HashMaps with these keys.
     */
    public static ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters, StringBuffer errors, StringBuffer warnings){
        return Walker.parseParameters(parameters,errors,warnings);}

    public static String help() {
        return Walker.help();}


    /** constructs a new solver of type RandomWalker.
     * The constructor is called serially. Therefore there are no race conditions.
     * globalParameters and centralProcessor are shared between different threads.
     * The walker is passive in the sense that it does not send data to the CentralDataHolder.
     *
     * @param applicationParameters     contains the parameters for controlling the solver
     * @param centralProcessor       contains the result of parsing and initializing the problem data.
     */
    public WalkerCommunicative(HashMap<String,Object> applicationParameters, CentralProcessor centralProcessor) {
        super(applicationParameters,centralProcessor);
        addObservers();}

    /** for optimizing some algorithms */
    protected int timestamp = 0;

    private ArrayList<Integer> trueLiterals = new ArrayList<>();
    private synchronized void addTrueLiteral(int literal) {trueLiterals.add(literal);}
    private Consumer<Integer> trueLiteralObserver = literal-> addTrueLiteral(literal);

    private ArrayList<Integer> implications = new ArrayList<>();
    private synchronized void addImplication(int from, int to) {implications.add(from); implications.add(to);}
    private BiConsumer<ImplicationNode,ImplicationNode> implicationObserver = (from, to) -> addImplication(from.literal,to.literal);


    protected void addObservers() {
        centralProcessor.model.addTrueLiteralObserver(trueLiteralObserver);
        centralProcessor.implicationDAG.addImplicationObserver(implicationObserver);
        implicationDAG.addTrueLiteralObserver(trueLiteralObserver);
    }

    protected void removeObservers() {
        centralProcessor.model.removeTrueLiteralObserver(trueLiteralObserver);
        centralProcessor.implicationDAG.removeImplicationObserver(implicationObserver);}

    public synchronized void integrateNewFacts() {
        for(int i = 0; i < implications.size(); i += 2) {
            int from = implications.get(i);
            int to   = implications.get(i+1);
            if(rwModel.isTrue(from) && rwModel.isFalse(to)) {flip(to);}
            implicationDAG.addClause(-from,to);} // this may generate new true literals
        implications.clear();

        for(int literal : trueLiterals) {
            int predicate = Math.abs(literal);
            if(rwModel.isFalse(literal)) {flip(predicate);}
            predicateQueue.remove(predicate);}
        trueLiterals.clear();}


    /** generates a candidate rwModel for the clauses.
     * A predicate becomes true if it occurs in more clauses than its negation.
     * The implications in the implicationDAG are taken into account.
     */
    public void initializeModel() {
        implicationDAG.applyToRoots(literal -> {
            if(rwModel.status[Math.abs(literal)] == 0) {
                int sizep = getOccurrences(literal);
                int sizen =  getOccurrences(-literal);
                if(sizep == 0 && sizen == 0) {return;}
                literal = sizep > sizen ? literal : -literal; // literal must become true
                implicationDAG.apply(literal,true,(lit-> { // all implied literals must become true
                    rwModel.status[ Math.abs(lit)] = (byte)(lit > 0 ? 1 : -1);}));}});
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(rwModel.status[predicate] == 0) {
                int sizep = getOccurrences(predicate);
                int sizen =  getOccurrences(-predicate);
                if(sizep == 0 && sizen == 0) {continue;}
                rwModel.status[predicate] = (byte)(sizep >= sizen ? 1 : -1);}}}


    /** flips the truth value of the given predicate together with implied predicates, if necessary.
     *
     * @param predicate to be flipped
     */
    public void flip(int predicate) {
        ++((WalkerStatistics)statistics).RW_flips;
        if(rwModel.isTrue(predicate)) {predicate *=-1;}
        implicationDAG.apply(predicate,true,(literal -> {
            if(rwModel.isFalse(literal)) {flipPredicate(Math.abs(literal));}}));}



    /** adds (change = 1) or removes (change = -1) a clause.
     * Updates flipScores and falseClauses
     *
     * @param clause a clause
     * @param change +1 for adding the clause, -1 for removing the clause.
     */
    public void updateClauseScore(Clause clause, int change) {
        int trueLiteral = 0;
        for(CLiteral lit : clause.cliterals) {
            int literal = lit.literal;
            if(rwModel.isTrue(literal)) {
                if(trueLiteral != 0) {return;} // at least two true literals: flipping changes nothing.
                trueLiteral = literal;}}
        if(trueLiteral == 0) { // clause is false
            if(change > 0) {falseClauses.add(clause);}
            else           {falseClauses.remove(clause);}
            for(CLiteral lit : clause.cliterals) {
                implicationDAG.apply(lit.literal,false,
                        (literal -> {if(rwModel.isFalse(literal)) {changeScore(Math.abs(literal),change);}}));}}
        else {  // clause is true and becomes false
            changeScore(Math.abs(trueLiteral),-change);}}

    private int[] counter = new int[]{0};

    /** counts the clauses containing the literal and its implied literals
     *
     * @param literal a literal
     * @return the number of clauses containing the literal and its implied literals.
     */
    int getOccurrences(int literal) {
        ++timestamp;
        counter[0] = 0;
        implicationDAG.apply(literal,true,(lit-> {
            for(CLiteral cLiteral : clauses.getLiterals(lit)){
                Clause clause = cLiteral.clause;
                if(clause.timestamp != timestamp) {
                    clause.timestamp = timestamp;
                    ++counter[0];}}}));
        return counter[0];}




}







