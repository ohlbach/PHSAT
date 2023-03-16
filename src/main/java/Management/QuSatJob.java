package Management;

import ProblemGenerators.ProblemGenerator;
import Solvers.Solver;
import Utilities.KVParser;

import java.util.ArrayList;
import java.util.Date;

public class QuSatJob {

    public Date jobDate = new Date();

    public KVParser kvParser;

    public long startTime;

    public long endTime;

    public GlobalParameters globalParameters;

    public ArrayList<ProblemSupervisor> problemSupervisors = new ArrayList<>();

    public ProblemDistributor problemDistributor;

    public QuSatJob(KVParser kvParser) {
        this.kvParser = kvParser;}

    public void solveProblems() {
        startTime = System.nanoTime();
        StringBuilder errors   = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        globalParameters= new GlobalParameters(kvParser.get("global"),errors,warnings);
        ArrayList<ProblemGenerator> generators = ProblemGenerator.makeProblemGenerators(kvParser.get("generator"),errors,warnings);
        ArrayList<Solver> solvers = Solver.makeSolvers(kvParser.get("solver"),errors,warnings);
        if(warnings.length() > 0)  System.out.println(warnings);
        if(errors.length() > 0) {
            System.out.println(errors);
            System.out.println("QuSat solver stops!");
            return;}

        for(ProblemGenerator problemGenerator : generators) {
            problemSupervisors.add(new ProblemSupervisor(globalParameters,problemGenerator,solvers));}
        problemDistributor = new ProblemDistributor(this,globalParameters,problemSupervisors);
        problemDistributor.solveProblems();
        endTime = System.nanoTime();
        analyseResults();
    }

    protected void analyseResults() {

    }
}
