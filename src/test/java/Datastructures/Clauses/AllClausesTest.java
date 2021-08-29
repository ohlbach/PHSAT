package Datastructures.Clauses;

import Datastructures.Results.Result;
import Datastructures.Symboltable;
import Datastructures.Theory.DisjointnessClasses;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import Management.Controller;
import Management.GlobalParameters;
import Management.Monitor;
import Management.ProblemSupervisor;
import org.junit.Test;

import java.util.HashMap;

import static org.junit.Assert.*;

public class AllClausesTest {
    StringBuffer errors = new StringBuffer();
    StringBuffer warnings = new StringBuffer();
    boolean monitoring = true;

    int typeOR = ClauseType.OR.ordinal();
    int typeXOR = ClauseType.XOR.ordinal();
    int typeDISJ = ClauseType.DISJOINT.ordinal();
    int typeEQ = ClauseType.EQUIV.ordinal();

    GlobalParameters globalParameters = new GlobalParameters();
    Controller controller = new Controller(null,null,null);
    ProblemSupervisor problemSupervisor;
    Symboltable symboltable;
    EquivalenceClasses eqClasses;
    DisjointnessClasses dClasses;
    Model model = new Model(10,symboltable);;
    BasicClauseList basicClauseList;

    private void prepare() {
        globalParameters.monitor = !monitoring ? null : new Monitor(null,"mixed",errors,warnings);
        globalParameters.trackReasoning = true;
        HashMap<String,Object> problemParameters = new HashMap<>();
        problemParameters.put("name","test");
        problemSupervisor = new ProblemSupervisor(controller,globalParameters,problemParameters,null);
        symboltable = new Symboltable(10);
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"a");
        symboltable.setName(5,"b");
        symboltable.setName(6,"c");
        problemSupervisor.model = new Model(10,symboltable);
        eqClasses = new EquivalenceClasses(problemSupervisor);
        dClasses = new DisjointnessClasses(problemSupervisor);
        problemSupervisor.equivalenceClasses = eqClasses;
        problemSupervisor.disjointnessClasses = dClasses;
        model.symboltable = symboltable;
        problemSupervisor.model = model;
        basicClauseList = new BasicClauseList();
        problemSupervisor.basicClauseList = basicClauseList;
    }

    @Test
    public void insertOr() throws Result {
        System.out.println("insert OR");
        prepare();
        basicClauseList.addClause(new int[]{1,typeOR,1,2,3});
        basicClauseList.addClause(new int[]{2,typeOR,3,4,-5});
        AllClauses allClauses = new AllClauses(problemSupervisor);
        assertEquals("All Clauses of Problem test:\n" +
                "  1: p,q,r\n" +
                "  2: r,a,-b\n",allClauses.toString());
        assertEquals("All Clauses of Problem test:\n" +
                "  1: 1,2,3\n" +
                "  2: 3,4,-5\n",allClauses.toNumbers());
        //System.out.println(allClauses.infoString(null));
    }

    @Test
    public void insertEQV() throws Result {
        System.out.println("insert EQV");
        prepare();
        eqClasses.addBasicEquivalenceClause(new int[]{1,typeEQ,1,2,3});
        eqClasses.addBasicEquivalenceClause(new int[]{2,typeEQ,5,-4,6});
        basicClauseList.addClause(new int[]{3,typeOR,1,2,3});
        AllClauses allClauses = new AllClauses(problemSupervisor);

        assertEquals("All Clauses of Problem test:\n",allClauses.toString());
        //System.out.println(eqClasses.infoString(null));
        //System.out.println(model.infoString(false));
        assertEquals("1",model.toNumbers());
    }

    @Test
    public void insertDisj() throws Result {
        System.out.println("insert Disjoints");
        prepare();
        basicClauseList.addClause(new int[]{1, typeDISJ, 1, 2, 3});
        AllClauses allClauses = new AllClauses(problemSupervisor);
        System.out.println(allClauses.infoString(null));
        System.out.println(dClasses.infoString(null));
        assertEquals("",dClasses.toString());
        assertEquals("All Clauses of Problem test:\n" +
                "  1: -p,-q\n" +
                "  2: -p,-r\n" +
                "  3: -q,-r\n",allClauses.toString());

    }
    }