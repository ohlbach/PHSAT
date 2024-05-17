package InferenceSteps;

import Datastructures.Clause;
import Datastructures.Literal;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.function.Consumer;

public class InfClauseSimplification extends InferenceStep{
    @Override
    public String title() {
        return "Clause Simplification";}

    @Override
    public String rule() {
        return "Various simplifications of clauses themselves";
    }

    private final int[] clauseBefore;
    private final Clause clauseAfter;
    private final IntArrayList derivedLiterals;

    public InfClauseSimplification(int[] clauseBefore, Clause clauseAfter, IntArrayList trueLiterals) {
        super();
        this.clauseBefore = clauseBefore;
        this.clauseAfter = clauseAfter;
        this.derivedLiterals = trueLiterals;}

    @Override
    public String toString(Symboltable symboltable) {
        return Clause.toString(clauseBefore, symboltable) + " -> " + clauseAfter.toString(symboltable,0);
    }

    @Override
    public void verify(Consumer<String> monitor, Symboltable symboltable) {
        IntArrayList literals = new IntArrayList();
        for(int i = 5; i < clauseBefore.length; i += 2) literals.add(clauseBefore[i]);
        int min = clauseBefore[3]; int max = clauseBefore[4];
        int nModels = 1 << literals.size();
        for(int model = 0; model < nModels; model++) {
             int trueLiterals = 0;
             for(int i = 5; i < clauseBefore.length; i += 2) {
                 if ((model & (i-5) & 1) == 1) trueLiterals += clauseBefore[i+1];}
             if(min <= trueLiterals && trueLiterals <= max) { // model satisfies clauseBefore
                 if(clauseAfter == null) {
                     monitor.accept("Clause " + Clause.toString(clauseBefore,symboltable) +
                             " is supposed to be unsatsifiable, but satisfied by " + toString(model,literals,symboltable));
                     continue;}
                 int trueLits = 0;
                 for(Object litObject : clauseAfter.literals) {
                     Literal literalObject = (Literal) litObject;
                     int literal = literalObject.literal;
                     int position = literals.indexOf(literal);
                     if((model & position) != 0) trueLits += literalObject.multiplicity;}
                 if(!(clauseAfter.min <= trueLits && trueLits <= clauseAfter.max)) {
                     monitor.accept("Model "+ toString(model,literals,symboltable) + " of clause " +
                             Clause.toString(clauseBefore,symboltable) + " does not satisfy simplified clause " +
                             clauseAfter.toString(symboltable,0)+"\n");}
                 if(derivedLiterals != null) {
                     for(int literal : derivedLiterals) {
                         int position = literals.indexOf(literal);
                         if(position >= 0) {
                             if((model & (1 << position)) == 0) {
                                 monitor.accept("Model "+ toString(model,literals,symboltable) + " of clause " +
                                         Clause.toString(clauseBefore,symboltable) + " does not satisfy derived literal " +
                                         Symboltable.toString(literal,symboltable));}}
                         else {
                             position = literals.indexOf(-literal);
                             if((model << (1 << position)) != 0) {
                                 monitor.accept("Model "+ toString(model,literals,symboltable) + " of clause " +
                                         Clause.toString(clauseBefore,symboltable) + " does not satisfy derived literal " +
                                         Symboltable.toString(literal,symboltable));}}}}}}}

    public static String toString(int model, IntArrayList literals, Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        for(int i = 0; i < literals.size(); i++) {
            int literal = literals.getInt(i);
            st.append(symboltable.toString(literal));
            if ((model & (1 << i)) != 0) {st.append(symboltable.toString(literal));}
            else {st.append(symboltable.toString(-literal));}
            if(i < literals.size() - 1) st.append(",");}
        return st.toString();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {

    }
}
