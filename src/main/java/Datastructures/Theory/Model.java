package Datastructures.Theory;

import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/** This class represents a propositional model, i.e. a set of literals which are supposed to be true.<br>
 *
 * Created by ohlbach on 12.09.2018.
 */
public class Model {
    /** the maximum number of predicates */
    public int predicates = 0;
    /** the current model */
    private IntArrayList model = null;

    /** lists the origins, i.e. the ids of the basic clauses causing the truth of the literal */
    private ArrayList<IntArrayList> origins = null;

    /** maps predicates in the model to +1 (true), -1 (false) or 0 (undefined) */
    private byte[] status = null;

    /** observers to be called when a new true literal is inserted */
    private ArrayList<BiConsumer<Integer,IntArrayList>> observers = null;

    /** creates a model with a maximum number of predicates, together with a means of tracking the origins
     *
     * @param trackReasoning if true then the origins are initialized
     * @param predicates the maximum number of predicates
     */
    public Model(int predicates, boolean trackReasoning) {
        assert predicates > 0;
        this.predicates = predicates;
        model = new IntArrayList(predicates);
        if(trackReasoning) origins = new ArrayList<>(predicates);
        status  = new byte[predicates+1];}

    /** adds a new observer which gets called when a new true literal is inserted
     *
     * @param observer to be called when a new true literal is inserted
     */
    public void addObserver(BiConsumer<Integer,IntArrayList> observer) {
        if(observers == null) {observers = new ArrayList<>();}
        observers.add(observer);}


    /** pushes a literal onto the model and checks if the literal is already in the model.
     * If the literal is new to the model then all observers are called.
     *
     * @param literal the literal for the model.
     * @param origin the ids of the basic clauses causing this truth
     * @return +1 if the literal was already in the model, -1 if the negated literal was in the model, otherwise 0.
     */
    public synchronized int add(int literal, IntArrayList origin) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        short tr = status[predicate];
        if(tr == 0){
            model.add(literal);
            if(origins != null) origins.add(origin);
            status[predicate] = literal > 0 ? (byte)1: (byte)-1;
            if(observers != null) {for(BiConsumer<Integer,IntArrayList> observer : observers) {observer.accept(literal,origin);}}
            return 0;}
        else {return (Integer.signum(literal) == (int)tr) ? 1 : -1;}}


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
    public synchronized  boolean isTrue(int literal) {
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
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        byte status = this.status[predicate];
        if(origins == null ||  status == 0) {return null;}
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
        Model newModel = new Model(predicates,origins != null);
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


    /**
     * @return the model as a comma separated string.
     */
    public String toString() {return model.toString();}

    /** returns the model as a comma separated string of names
     *
     * @param symboltable a symboltable
     * @return the model as a comma separated string of names
     */
    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        int size = model.size()-1;
        for(int i = 0; i <= size; ++i) {
            int literal = model.getInt(i);
            st.append((symboltable == null) ? Integer.toString(literal) : symboltable.getLiteralName(literal));
            if(origins != null) {st.append("@").append(origins.get(i).toString());}
            if(i == size) st.append(",");}
        return st.toString();}


}
