package Management;

import java.util.ArrayList;

public class Parameters {
    public String title;
    public ArrayList<Parameter> parameters;

    public Parameters(String title) {
        this.title = title;
        parameters = new ArrayList<>();}

    public void add(Parameter parameter) {
        parameters.add(parameter);}

    public void clearValues() {
        for(Parameter parameter : parameters) {parameter.value = null;}
    }

}
