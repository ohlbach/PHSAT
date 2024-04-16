package Management;

import java.util.ArrayList;
import java.util.function.BiConsumer;
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
    public BiConsumer<String,String> updater;

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
        return name + ": " + defaultValue + ": " +valueString(value);}

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
