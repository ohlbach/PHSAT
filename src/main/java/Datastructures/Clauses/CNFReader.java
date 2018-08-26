package Datastructures.Clauses;

import Datastructures.Model;
import Datastructures.Status;

import java.io.*;
import java.util.ArrayList;

/**
 * Created by ohlbach on 26.08.2018.
 */
public class CNFReader extends ClauseSetGenerator {
    String filename;
    BufferedReader reader;

    public CNFReader(String filename,ClauseGenerator clauseGenerator) throws FileNotFoundException {
        this.filename = filename;
        this.clauseGenerator = clauseGenerator;
        reader = new BufferedReader(new FileReader(filename));}


    public Status generate() throws Exception {
        StringBuffer info = new StringBuffer();
        int predicates = 0;
        int clauses = 0;
        Model model = null;
        int clauseCounter = 0;
        String line;
        ArrayList<Integer> literals = new ArrayList();
        Status stat = new Status();
        stat.filename = filename;
        while((line = reader.readLine()) != null) {
            if(line.startsWith("c")) {info.append(line).append("\n"); continue;}
            if(line.startsWith("p")) {
                String[] parts = line.split("\\s+");
                if(parts.length < 4) {throw new Exception("Illegal format of line " + line + " in " + filename);}
                predicates = Integer.parseInt(parts[2]);
                clauses = Integer.parseInt(parts[3]);
                continue;}
            if(predicates == 0) {throw new Exception("Unknown number of predicates in " + filename);}
            if(clauseList == null) {
                model = new Model(predicates);
                clauseList = new ClauseList(clauses,model,null);
                clauseList.info = info.toString();
                stat.clauseList = clauseList;}
            ++clauseCounter;
            literals.clear();
            for(String literal : line.split("\\s+")) {
                int lit = Integer.parseInt(literal);
                if(lit != 0) {literals.add(lit);}
            int status = simplify(literals,model);
            if(status == -1) {
                stat.unsatisfiable = true;
                stat.falseClause = line;
                return stat;}
            if(status == 0) {
                Clause clause = clauseGenerator.newClause(clauseCounter,literals);
                clauseList.addClause(clause);}}}
            return stat;}

}
