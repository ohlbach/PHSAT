package Datastructures.Clauses.Simplifiers;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.Connective;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import Management.Controller;
import Management.GlobalParameters;
import Management.Monitor;
import Management.ProblemSupervisor;
import org.junit.Test;

import java.util.HashMap;

import static org.junit.Assert.*;

public class ClauseSimplifierTest {

    static StringBuilder errors=new StringBuilder();
    static StringBuilder warnings=new StringBuilder();

    boolean monitoring = false;

    int etype= Connective.EQUIV.ordinal();

    ProblemSupervisor prepare(boolean monitoring, boolean withSymboltable) {
        GlobalParameters globalParameters=new GlobalParameters();
        globalParameters.monitor=!monitoring ? null : new Monitor(null,"mixed",errors,warnings);
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
        problemSupervisor.clauseSimplifier = new ClauseSimplifier(problemSupervisor,globalParameters.monitor,"test",null);
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
}