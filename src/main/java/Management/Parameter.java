package Management;

import java.util.ArrayList;
import java.util.function.BiFunction;
import java.util.function.Consumer;

public class Parameter {

    public enum Type {String,OneOf,Label,File,Directory,Frame,Boolean};
    public String name;
    public Type type;
    public String valueString;
    public Object value;
    public String description;
    public Parameters parameters;
    public BiFunction<String,StringBuilder,Object> parser;
    public Consumer<String> updater;

    public Parameter(String name, Type type, String valueString, Object value, String description) {
        this.name = name;
        this.type = type;
        this.valueString = valueString == null ? name : valueString;
        this.value = value;
        this.description = description;
    }
    public Parameter(String name, Type type, String valueString, String description) {
        this.name = name;
        this.type = type;
        this.valueString = valueString == null ? name : valueString;
        this.value = valueString;
        this.description = description;
    }


    public void setParser(BiFunction<String,StringBuilder,Object> parser) {
        this.parser = parser;}

    public Parameter clone() {
        Parameter cloned = new Parameter(name, type, valueString, value, description);
        cloned.updater = updater;
        cloned.parser = parser;
        if(parameters != null) {cloned.parameters = parameters.clone();}
        return cloned;}


    public String toString() {
        return name + "; " + valueString + "; " +valueString(value);}

    public String valueString(Object value) {
        if (value == null) {return "null";}
        if(value instanceof ArrayList) {
            StringBuilder sb = new StringBuilder();
            for (Object v : (ArrayList) value) {
                sb.append(valueString(v)).append(", ");}
            sb.delete(sb.length() - 2, sb.length());
            return sb.toString();}
        if(value.getClass().isArray()) {
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (Object v : (Object[]) value) {
                sb.append(valueString(v)).append(", ");}
            sb.delete(sb.length() - 2, sb.length());
            sb.append("]");
            return sb.toString();}
        return value.toString();
        }





}
