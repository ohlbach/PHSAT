package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;

import java.util.ArrayList;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/** This class maintains observers which are called when clauses or literals are
 * added, removed or exchanged.
 *
 * Created by ohlbach on 16.06.2019.
 */
public class ClausesObservers<Clause> {

    /** These are observers which are called when a clause is added */
    private ArrayList<Consumer<Clause>> clauseAddedObservers = null;

    /** adds a clauseAddedObserver
     *
     * @param observer a function to be called when a clause is added to the clause list
     */
    public void addClauseAddedObserver(Consumer<Clause> observer) {
        clauseAddedObservers.add(observer);}

    /** removes a clauseAddedObserver
     *
     * @param observer the observer to be removed
     */
    public void removeClauseAddedObserver(Consumer<Clause> observer) {
        clauseAddedObservers.remove(observer);}

    /** removes all clauseAddedObservers
     */
    public void clearClauseAddedObservers() {
        clauseAddedObservers.clear();}

    /** calls all clauseAddedObservers
     *
     * @param clause a clause
     */
    public void addedClause(Clause clause) {
        if(clauseAddedObservers != null) {
            for(Consumer<Clause> observer : clauseAddedObservers) {observer.accept(clause);}}}




    /** These are observers which are called when a clause is removed */
    private ArrayList<Consumer<Clause>> clauseRemovedObservers = null;

    /** adds a clauseAddedObserver
     *
     * @param observer a function to be called when a clause is removed from the clause list
     */
    public void addClauseRemovedObserver(Consumer<Clause> observer) {
        clauseRemovedObservers.add(observer);}

    /** removes a clauseRemovedObserver
     *
     * @param observer the observer to be removed
     */
    public void removeClauseRemovedObserver(Consumer<Clause> observer) {
        clauseRemovedObservers.remove(observer);}

    /** removes all clauseRemovedObservers
     */
    public void clearClauseRemovedObservers() {
        clauseRemovedObservers.clear();}

    /** calls all clauseRemovedObserver
     *
     * @param clause a clause
     */
    public void removedClause(Clause clause) {
        if(clauseRemovedObservers != null) {
            for(Consumer<Clause> observer : clauseRemovedObservers) {observer.accept(clause);}}}





    /** These are observers which are called when a literal is removed from a clause*/
    private ArrayList<Consumer<CLiteral<Clause>>> literalRemovedObservers = null;

    /** adds a literalRemovedObserver
     *
     * @param observer a function to be called when a literal is removed froma clause
     */
    public void addLiteralRemovedObserver(Consumer<CLiteral<Clause>> observer) {
        literalRemovedObservers.add(observer);}

    /** removes a literalRemovedObserver
     *
     * @param observer the observer to be removed
     */
    public void removeLiteralRemovedObserver(Consumer<CLiteral<Clause>> observer) {
        literalRemovedObservers.remove(observer);}

    /** removes all literalRemovedObservers
     */
    public void clearLiteralRemovedObservers() {
        literalRemovedObservers.clear();}

    /** calls all literalRemovedObservers
     *
     * @param cLiteral a cLiteral
     */
    public void removedLiteral(CLiteral<Clause> cLiteral) {
        if(literalRemovedObservers != null) {
            for(Consumer<CLiteral<Clause>> observer : literalRemovedObservers) {observer.accept(cLiteral);}}}





    /** These are observers which are called when a literal is removed from a clause*/
    private ArrayList<BiConsumer<CLiteral<Clause>,CLiteral<Clause>>> literalExchangedObservers = null;

    /** adds a literalRemovedObserver
     *
     * @param observer a function to be called when a literal is removed froma clause
     */
    public void addLiteralExchangedObserver(BiConsumer<CLiteral<Clause>,CLiteral<Clause>> observer) {
        literalExchangedObservers.add(observer);}

    /** removes a literalRemovedObserver
     *
     * @param observer the observer to be removed
     */
    public void removeLiteralExchangedObserver(BiConsumer<CLiteral<Clause>,CLiteral<Clause>> observer) {
        literalExchangedObservers.remove(observer);}

    /** removes all literalRemovedObservers
     */
    public void clearLiteralExchangedObservers() {
        literalExchangedObservers.clear();}

    /** calls all literalRemovedObservers
     *
     * @param oldCLiteral the old cLiteral
     * @param newCLiteral the new cLiteral
     */
    public void exchangedLiteral(CLiteral<Clause> oldCLiteral, CLiteral<Clause>  newCLiteral) {
        if(literalRemovedObservers != null) {
            for(BiConsumer<CLiteral<Clause>,CLiteral<Clause>> observer : literalExchangedObservers) {
                observer.accept(oldCLiteral, newCLiteral);}}}
}
