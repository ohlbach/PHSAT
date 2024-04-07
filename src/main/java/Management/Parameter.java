package Management;

import java.util.function.Consumer;

public class Parameter {
    public String name;
    public String type;
    public String defaultName;
    public String description;
    public String[] choices;
    public Object value;

    public Parameter(String name, String type, String defaultName, String description, Consumer<Object> valueConsumer, String... choices) {
        this.name = name;
        this.type = type;
        this.defaultName = defaultName == null ? name : defaultName;
        this.description = description;
        this.choices = choices;
    }

}
