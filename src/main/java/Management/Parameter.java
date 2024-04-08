package Management;

public class Parameter {

    public enum Type {String,OneOf,Label,File,Frame,Boolean};
    public String name;
    public Type type;
    public String defaultName;
    public String description;
    public Object value;
    public Parameters parameters;

    public Parameter(String name, Type type, String defaultName, String description) {
        this.name = name;
        this.type = type;
        this.defaultName = defaultName == null ? name : defaultName;
        this.description = description;
    }

    public String toString() {
        return name + ": " + value;}


}
