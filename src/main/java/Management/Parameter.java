package Management;

import java.util.function.BiFunction;

public class Parameter {

    public enum Type {String,Integer,OneOf,Label,File,Frame,Boolean};
    public String name;
    public Type type;
    public String defaultValue;
    public String description;
    public Object value;
    public Parameters parameters;
    public BiFunction<String,StringBuilder,Object> parser;

    public Parameter(String name, Type type, String defaultValue, Object value, String description) {
        this.name = name;
        this.type = type;
        this.defaultValue = defaultValue == null ? name : defaultValue;
        this.value = value;
        this.description = description;
    }
    public Parameter(String name, Type type, String defaultValue, String description) {
        this.name = name;
        this.type = type;
        this.defaultValue = defaultValue == null ? name : defaultValue;
        this.value = defaultValue;
        this.description = description;
    }


    public void setParser(BiFunction<String,StringBuilder,Object> parser) {
        this.parser = parser;}

    public String toString() {
        return name + ": " + value;}


}
