package Datastructures.Clauses.SimplifiersOld;

public class ClauseSimplifierTest {
/*
    static StringBuilder errors=new StringBuilder();
    static StringBuilder warnings=new StringBuilder();

    boolean monitoring = false;

    int etype= Connective.EQUIV.ordinal();

    ProblemSupervisor prepare(boolean monitoring, boolean withSymboltable) {
        GlobalParameters globalParameters=new GlobalParameters();
        globalParameters.monitor=!monitoring ? null : new MonitorLife(null,"mixed",errors,warnings);
        HashMap<String,Object> problemParameters=new HashMap<>();
        problemParameters.put("name","test");

        Controller controller=new Controller(null,null,null);
        ProblemSupervisor problemSupervisor=new ProblemSupervisor(controller,globalParameters,problemParameters,null);
        Symboltable symboltable= null;
        if(withSymboltable) {
            symboltable = new Symboltable(10);
            symboltable.setName(1,"p");
            symboltable.setName(2,"q");
            symboltable.setName(3,"r");
            symboltable.setName(4,"a");
            symboltable.setName(5,"b");
            symboltable.setName(6,"c");}
        problemSupervisor.model=new Model(20,symboltable);
        problemSupervisor.equivalenceClasses = new EquivalenceClasses(problemSupervisor);
        InitializerSimplifier clauses = new InitializerSimplifier(problemSupervisor);
        problemSupervisor.clauseSimplifier = new ClauseSimplifier(clauses,null);
        problemSupervisor.clauseCounter = 9;
        return problemSupervisor;}

    @Test
    public void replaceEquivalences() throws Unsatisfiable {
        System.out.println("replaceEquivalences");
        ProblemSupervisor ps = prepare(true,false);
        EquivalenceClasses eqc = ps.equivalenceClasses;
        ClauseSimplifier cs = ps.clauseSimplifier;
        eqc.addBasicEquivalenceClause(new int[]{1,etype,2,3,4});
        Clause c1 = new Clause(2, Connective.INTERVAL, 2, 3, 4,3,2,5);
        Clause c2 = cs.replaceEquivalences(c1);
        assertEquals("I-10: [2,3]: 2,2,2,5",c2.toNumbers());

        ps = prepare(false,true);
        eqc = ps.equivalenceClasses;
        cs = ps.clauseSimplifier;
        eqc.addBasicEquivalenceClause(new int[]{1,etype,2,3,4});
        cs.nextId = null;
        Clause c3 = cs.replaceEquivalences(c1);
        assertSame(c3,c1);
        assertEquals("I-2: [2,3]: 2,2,2,5",c3.toNumbers());
    }

    @Test
    public void MultipleAndComplementaryLiterals() throws Unsatisfiable {
        System.out.println("MultipleAndComplementaryLiterals");
        ProblemSupervisor ps = prepare(true, false);
        ClauseSimplifier cs = ps.clauseSimplifier;
        Clause c1 = new Clause(1, Connective.OR, 2, 3, 4, 3, 2, 4, 2, 5);
        Clause c2 = cs.removeMultipleAndComplementaryLiterals(c1);
        assertEquals("10: 2,3,4,5", c2.toNumbers());

        c1 = new Clause(2, Connective.OR, 2, 3, 4, 3, 2, -4, 2, 5);
        c2 = cs.removeMultipleAndComplementaryLiterals(c1);
        assertNull(c2);

        c1 = new Clause(1, Connective.OR, 2, 3, 4, 5);
        c2 = cs.removeMultipleAndComplementaryLiterals(c1);
        assertEquals("1: 2,3,4,5", c2.toNumbers());

        c1 = new Clause(1, Connective.INTERVAL, 2,3, 2, 3, 4, -2,5);
        c2 = cs.removeMultipleAndComplementaryLiterals(c1);
        assertEquals("I-12: [1,2]: 3,4,5", c2.toNumbers());

        c1 = new Clause(1, Connective.INTERVAL, 2,3, 2, 3, 4, -2);
        c2 = cs.removeMultipleAndComplementaryLiterals(c1);
        assertEquals("13: 3,4", c2.toNumbers());

        c1 = new Clause(1, Connective.INTERVAL, 2,2, 2, 3, -2);
        c2 = cs.removeMultipleAndComplementaryLiterals(c1);
        assertNull(c2);
        assertEquals("Model:\n" +
                "3",ps.model.toNumbers());

        c1 = new Clause(1, Connective.INTERVAL, 1,1, 2, 4, -2);
        c2 = cs.removeMultipleAndComplementaryLiterals(c1);
        assertNull(c2);
        assertEquals("Model:\n" +"3,-4",ps.model.toNumbers());}

    @Test
    public void removeTrueFalsePredicates() throws Unsatisfiable {
        System.out.println("removeTrueFalsePredicates");
        ProblemSupervisor ps = prepare(true, false);
        ClauseSimplifier cs = ps.clauseSimplifier;
        Clause c1 = new Clause(1, Connective.OR, 2, 3, 4, 3, 2, 4, 2, 5);
        Clause c2 = cs.removeTrueFalsePredicates(c1);
        assertEquals("1: 2,3,4,3,2,4,2,5", c2.toNumbers());
        ps.model.add(-3,new InferenceTest("My Test"));
        c2 = cs.removeTrueFalsePredicates(c1);
        assertEquals("10: 2,4,2,4,2,5", c2.toNumbers());

        ps.model.add(5,new InferenceTest("My Test"));
        c1 = new Clause(1, Connective.INTERVAL, 2, 3, 4, -3, 2, 4, 2, -5,6,7);
        c2 = cs.removeTrueFalsePredicates(c1);
        assertEquals("I-11: [1,2]: 4,2,4,2,6,7", c2.toNumbers());

        c1 = new Clause(1, Connective.INTERVAL, 1,1, -3,5,6);
        try{c2 = cs.removeTrueFalsePredicates(c1);}
        catch(Unsatisfiable uns) {
            System.out.println(uns.toString());}}
            */

    }