package Solvers.Reolution;

import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import com.sun.xml.internal.ws.message.ByteArrayAttachment;

/**
 * Created by ohlbach on 22.10.2018.
 */
public class RClause extends Clause {
    int priority;

    public RClause(Clause clause, int priority) {
        super(clause.id,clause.size());
        this.priority = priority;
        for(CLiteral cLiteral : clause.cliterals) {clause.addCLiteralDirectly(cLiteral.clone());}
    }


}
