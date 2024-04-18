package Management;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.function.BiFunction;

public class Parameters {
    public String title;
    public String description;
    public ArrayList<Parameter> parameters;
    public BiFunction<Parameters, StringBuilder, Boolean> finalCheck;
    public BiFunction<Parameters,StringBuilder,Object> operation = null;

    public Parameters(String title) {
        this.title = title;
        parameters = new ArrayList<>();}

    public void add(Parameter parameter) {
        parameters.add(parameter);}

    public void setDescription(String description) {this.description = description;}
    public void setFinalCheck(BiFunction<Parameters, StringBuilder, Boolean> finalCheck) {this.finalCheck = finalCheck;}
    public void setOperation(BiFunction<Parameters,StringBuilder,Object> operation) {
        this.operation = operation;}

    public void clearValues() {
        for(Parameter parameter : parameters) {parameter.value = null;}
    }

    public Parameters clone() {
        Parameters cloned = new Parameters(title);
        cloned.description = description;
        cloned.finalCheck = finalCheck;
        cloned.operation = operation;
        cloned.parameters = new ArrayList<>();
        for (Parameter parameter : parameters) cloned.parameters.add(parameter.clone());
        return cloned;}

    public String toString() {
        String result = title + "\n";
        for (Parameter parameter : parameters) {
            result += parameter.toString() + "\n";
        }
        System.out.println("RES\n"+ result + "SER\n" );
        return result;
        }


    public void loadParameters(BufferedReader reader, StringBuilder errors) throws IOException {
        String line = "";
        while((line = reader.readLine()).isEmpty()) {}
        while(!line.isEmpty()) {
            String[] parts = line.split("\\s*;\\s*");
            String name = parts[0];
            for(Parameter parameter : parameters) {
                if(parameter.name.equals(name)) {
                    parameter.valueString = parts[1];
                    parameter.value = (parameter.parser == null) ?
                            parts[1] : parameter.parser.apply(parts[1],errors);
                    if(parameter.updater != null) parameter.updater.accept(parts[1]);}}
            line = reader.readLine();}}

}
