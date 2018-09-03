package Management;

import java.lang.reflect.Method;
import java.util.HashMap;

/**
 * Created by Ohlbach on 03.09.2018.
 */
public class SolverControl {
    private KVAnalyser kvAnalyser;
    private StringBuffer errors;
    private StringBuffer warnings;

    public SolverControl(StringBuffer errors, StringBuffer warnings) {
        this.errors = errors;
        this.warnings = warnings;}


    public void solve(KVAnalyser kvAnalyser) {
        this.kvAnalyser = kvAnalyser;
        int parallel = (Integer)kvAnalyser.globalParameters.get("parallel");
        if(parallel == 0) {
            for(HashMap<String,Object> control : kvAnalyser.problemParameters) {solveProblem(control);}
            return;}
        int nproblems = kvAnalyser.problemParameters.size();
        int nthreads = nproblems / parallel;
        if(nproblems % parallel != 0) {++nthreads;}
        Thread[] threads = new Thread[nthreads];
        int threadCounter = -1;
        for(int n = 0; n < nproblems; ++n) {
            if((n % parallel) == 0) {
                ++threadCounter;
                int m = n;
                threads[threadCounter] = new Thread(()->solveProblems(m,parallel));}}
        for(int n = 0; n < nthreads; ++n) {threads[n].start();}}


    void solveProblems(int n, int parallel) {
        for(int i = n; i < n+parallel; ++i) {
            solveProblem(kvAnalyser.problemParameters.get(i));}}

    void solveProblem(HashMap<String,Object> problemControl) {
        int nthreads = kvAnalyser.solverParameters.size();
        Thread[] threads = new Thread[nthreads];
        for(int n = 0; n < nthreads; ++n) {
            HashMap<String,Object> solverCtr = kvAnalyser.solverParameters.get(n);
            threads[n] = new Thread(()->solve(solverCtr, problemControl));}
        for(int n = 0; n < nthreads; ++n) {
            threads[n].start();}}


    void solve(HashMap<String,Object> solverControl,HashMap<String,Object> problemControl) {
        try{
            ((Method)solverControl.get("solver")).invoke("solve",solverControl,problemControl,errors,warnings);}
        catch(Exception ex) {
            ex.printStackTrace();
            System.exit(1);}}
}
