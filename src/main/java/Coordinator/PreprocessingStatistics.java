package Coordinator;

/**
 * Created by ohlbach on 10.10.2018.
 */
public class PreprocessingStatistics {
    private int disjunctions = 0;
    private int conjunctions = 0;
    private int xors         = 0;
    private int disjoints    = 0;
    private int tautologies  = 0;
    private int literalDeletions = 0;
    private int forwardSubsumptions  = 0;
    private int backwardSubsumptions  = 0;
    private int purities         = 0;
    private int implicationResolutions = 0;
    private int doubleLiterals = 0;
    private int equivalenceReplacements = 0;
    private int falseLiterals = 0;
    private int trueClauses = 0;
    private int replacementResolutions = 0;

    public void addDisjunctions(int n) {disjunctions += n;}
    public void addConjunctions(int n) {conjunctions += n;}
    public void addXors(int n) {xors += n;}
    public void addDisjoints(int n) {disjoints += n;}
    public void addtautologies(int n) {tautologies += n;}
    public void addLiteralDeletions(int n) {literalDeletions += n;}
    public void addClauseDeletions(int n) {literalDeletions += n;}
    public void addPurities(int n) {purities += n;}
    public void addImplicationResolutions(int n) {implicationResolutions += n;}
    public void addDoubleLiterals(int n) {doubleLiterals += n;}
    public void addEquivalenceReplacements(int n) {equivalenceReplacements += n;}
    public void addFalseLiterals(int n) {falseLiterals += n;}
    public void addTrueClauses(int n) {trueClauses += n;}
    public void addBackwardSubsumptions(int n) {backwardSubsumptions += n;}
    public void addForwardSubsumptions(int n) {forwardSubsumptions += n;}
    public void addReplacementResolutions(int n) {replacementResolutions += n;}




}
