package Management;

import java.util.ArrayList;

public class Parameters {
    public String title;
    public String description;
    public ArrayList<Parameter> parameters;

    public Parameters(String title) {
        this.title = title;
        parameters = new ArrayList<>();}

    public void add(Parameter parameter) {
        parameters.add(parameter);}

    public void setDescription(String description) {this.description = description;}

    public void clearValues() {
        for(Parameter parameter : parameters) {parameter.value = null;}
    }

    public String toString() {
        String result = "Title: " + title + "\n";
        for (Parameter parameter : parameters) {
            result += parameter.toString() + "\n";
        }
        return result;
        }

}
