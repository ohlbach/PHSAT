package Datastructures;

/**
 * Created by ohlbach on 25.08.2018.
 */
public class Symboltable {
    public int size;
    private String[] posName;
    private String[] negName;

    public Symboltable(int size) {
        this.size = size;
        posName = new String[size+1];
        negName = new String[size+1];
    }

    public String getName(int literal) {
        assert literal > 0;
        return (literal > 0) ? posName[literal] : negName[literal];
    }

    public void setName(int literal, String name) {
        assert literal > 0 && literal <= size;
        if(literal > 0) {posName[literal] = name;}
        else {negName[literal] = name;}
    }
}
