package Management;

import java.util.function.BiFunction;

public class Parameter {

    public enum Type {String,Integer,OneOf,Label,File,Frame,Boolean};
    public String name;
    public Type type;
    public String defaultName;
    public String description;
    public Object value;
    public Parameters parameters;
    public BiFunction<String,StringBuilder,Object> transformer;

    public Parameter(String name, Type type, String defaultName, String description) {
        this.name = name;
        this.type = type;
        this.defaultName = defaultName == null ? name : defaultName;
        this.description = description;
    }

    public void setTransformer(BiFunction<String,StringBuilder,Object> transformer) {
        this.transformer = transformer;}

    public String toString() {
        return name + ": " + value;}


}
