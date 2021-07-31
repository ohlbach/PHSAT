package Datastructures.Theory;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import com.sun.istack.internal.Nullable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

/** This class represents a propositional model, i.e. a set of literals which are supposed to be true.<br>
 * Each literal in the model is accompanied by the origins, i.e. the list of basic clause ids which
 * caused the derivation of the truth of the literal.
 * <br>
 * When a literal is added to the model, and its negation is already in the model then
 * an Unsatisfiable exception is thrown.
 * This may terminate the search process.
 * Adding a non-contradictory literal to the model causes all transferers to be called.
 * They distribute the information about the literal to the other reasoners.
 *
 * Created by ohlbach on 12.09.2018.
 */
public class Model {
    /** the maximum number of predicates */
    public int predicates = 0;

    /** the symboltable for the literals */
    public Symboltable symboltable = null;

    /** the current model */
    private IntArrayList model = null;

    /** lists the origins, i.e. the ids of the basic clauses causing the truth of the literal */
    private ArrayList<IntArrayList> origins = null;

    /** maps predicates in the model to +1 (true), -1 (false) or 0 (undefined) */
    private byte[] status = null;

    /** observers to be called when a new true literal is inserted */
    private ArrayList<OneLiteralTransfer> transferers = new ArrayList<>();

    /** creates a model with a maximum number of predicates, together with a means of tracking the origins
     *
     * @param symboltable the symboltable for the literals.
     * @param trackReasoning if true then the origins are initialized
     * @param predicates the maximum number of predicates
     */
    public Model(int predicates, Symboltable symboltable, boolean trackReasoning) {
        assert predicates > 0;
        this.predicates = predicates;
        this.symboltable = symboltable;
        model = new IntArrayList(predicates);
        if(trackReasoning) origins = new ArrayList<>(predicates);
        status  = new byte[predicates+1];}

    /** adds a new observer which gets called when a new true literal is inserted
     *
     * @param transferer to be called when a new true literal is inserted
     */
    public void addTransferer(OneLiteralTransfer transferer) {
        transferers.add(transferer);}


    /** pushes a literal onto the model and checks if the literal is already in the model.
     * If the literal is new to the model then all transferers are called.
     *
     * @param literal the literal for the model.
     * @param origin the ids of the basic clauses causing this truth
     * @param thread the thread which generated the true literal.
     * @throws Unsatisfiable if a contradiction with an earlier entry in the model occurs.
     */
    public synchronized void add(int literal, IntArrayList origin, Thread thread) throws Unsatisfiable {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        if(isTrue(literal)) {return;}
        if(isFalse(literal))
            throw new Unsatisfiable(this,literal,symboltable,
                    origins != null ? joinIntArrays(origin,getOrigin(literal)) : null);

        model.add(literal);
        if(origins != null) origins.add(origin);
        status[predicate] = literal > 0 ? (byte)1: (byte)-1;
        if(transferers != null) {
            for(OneLiteralTransfer transferer : transferers) {
                if(thread != transferer.thread) {transferer.transferer.accept(literal,origin);}}}}

    /** adds a literal immediately without any checks and transfers
     *
     * @param literal a literal
     * @param origin he ids of the basic clauses causing this truth
     */
    public void addImmediately(int literal, IntArrayList origin) {
        model.add(literal);
        if(origins != null) origins.add(origin);
        status[Math.abs(literal)] = literal > 0 ? (byte)1: (byte)-1;}

        /** returns the entire model, i.e. the list of true literals.
         * Access to the list, however is not synchronized.
         *
         * @return the model
         */
    public IntArrayList getModel() {return model;}

    /** checks if the literal is true in the model.
     *
     * @param literal the literal to be checked.
     * @return true if the literal is true in the model.
     */
    public synchronized boolean isTrue(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        short status = this.status[predicate];
        if(status == 0) {return false;}
        return literal > 0 ? status == 1: status == -1;}

    /** checks if the literal is false in the model.
     *
     * @param literal the literal to be checked.
     * @return true if the literal is false in the model.
     */
    public synchronized boolean isFalse(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        short status = this.status[predicate];
        if(status == 0) {return false;}
        return literal > 0 ? status == -1: status == 1;}

    /** returns the status of the literal in the model.
     *
     * @param literal the literal to be checked
     * @return +1 if the literal is true in the model, -1 if it is false in the model, otherwise 0.
     */
    public synchronized int status(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        short status = this.status[predicate];
        return literal > 0 ? status : -status;}

    /** returns the origins of a model entry
     *
     * @param literal a literal
     * @return null or the origins of the literal entry
     */
    public synchronized IntArrayList getOrigin(int literal) {
        if(origins == null) {return null;}
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        byte status = this.status[predicate];
        if(status == 0) {return null;}
        return origins.get(model.indexOf((status > 0) ? predicate : -predicate));}

    /** turns the status value into a string
     *
     * @param status -1,0,+1
     * @return the corresponding name
     */
    public synchronized static String toString(int status) {
        switch(status) {
            case -1 : return "false";
            case  0:  return "unknown";
            case +1 : return "true";}
        return "error";}


    /** sets the logical status of the literal.
     *
     * @param literal a literal
     * @param status +1 (for true) and -1 (for false)
     */
    public synchronized void setStatus(int literal, int status, IntArrayList origin) {
        if(literal < 0) {literal = -literal; status = (byte)-status;}
        assert this.status[literal] == 0 || this.status[literal] == status;
        if(this.status[literal] == 0) {
            this.status[literal] = (byte)status;
            model.add(status > 0 ? literal : -literal);
            if(origins != null) {origins.add(origin);}}}



    /** clones the model (without observers)
     *
     * @return a clone of the model
     */
    public Model clone() {
        Model newModel = new Model(predicates, symboltable,origins != null);
        if(origins != null) {
            newModel.origins = new ArrayList<>();
            for(IntArrayList origin : origins) {newModel.origins.add(origin.clone());}}
        newModel.status = status.clone();
        newModel.model = model.clone();
        return newModel;}

    /** returns a clone of the current status of the model.
     *
     * @return a clone of the current status of the model
     */
    public byte[] cloneStatus() {return status.clone();}

    /** checks if the literal is in the model.
     *
     * @param literal a literal
     * @return true if the model contains the literal.
     */
    public synchronized boolean contains(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        return status[predicate] != 0;}

    /** returns the current size of the model.
     *
     * @return the current size of the model
     */
    public synchronized int size() {return model.size();}

    /** checks if the model is empty.
     *
     * @return true if the model is empty
     */
    public synchronized boolean isEmpty() {return model.isEmpty();}

    /** checks if the model is full.
     *
     * @return true if the model is empty
     */
    public synchronized boolean isFull() {return model.size() == predicates;}


    /** returns the model as a comma separated string of names
     *
     * @param symboltable null or a symboltable
     * @return the model as a comma separated string of names
     */
    public String toString(@Nullable Symboltable symboltable) {
        return Symboltable.getLiteralNames(model,symboltable);}

    /** turnes the model and the origins into a string
     *
     * @param symboltable null or a symboltable
     * @return the model together with the origins as string.
     */
    public String infoString(@Nullable Symboltable symboltable) {
        if(origins == null)  return toString(symboltable);
        StringBuilder st = new StringBuilder();
        int size = model.size()-1;
        for(int i = 0; i <= size; ++i) {
            st.append(Symboltable.getLiteralName(model.getInt(i),symboltable)).append(" @ ");
            st.append(Symboltable.getLiteralNames(origins.get(i),null));
            if(i < size-1) st.append("\n");}
        return st.toString();}


}
