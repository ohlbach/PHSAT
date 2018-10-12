package Datastructures.Statistics;

import java.lang.reflect.Field;
import java.util.ArrayList;

/**
 * Created by ohlbach on 11.10.2018.
 */
public abstract class Statistic {

    public static String toString(int size, Statistic statistic) {
        ArrayList<String[]> pairs = new ArrayList<>();
        try{int nameLength = 0;
            int valueLength = 0;
            String value;
            String name;
            for(Field f : statistic.getClass().getFields()) {
                Object o = f.get(statistic);
                if((o instanceof Number) && ((Number)o).doubleValue() != 0.0) {
                    name = f.getName();
                    value = o.toString();
                    nameLength = Math.max(nameLength,name.length());
                    valueLength = Math.max(valueLength,value.length());
                    pairs.add(new String[]{name,value});}}
            if(size != 0) {nameLength = size;}
            StringBuilder st = new StringBuilder();
            for(Object[] pair : pairs) {
                st.append(String.format("%"+nameLength+"s: %"+valueLength+"s",pair[0],pair[1])).append("\n");}
            return st.toString();}
    catch(Exception e) {e.printStackTrace();System.exit(1);}
    return null;}


    /** combines several statistics of the same type.
     *
     * @param statistics of the same type
     * @return a list of triples [name, accumulated values, average value]
     */
    public static ArrayList<Object[]>  combineStatistics(Statistic[] statistics) {
        int size = statistics.length;
        ArrayList<Object[]> combinedStatistics = new ArrayList<>();
        try{
            for(Field f : statistics[0].getClass().getFields()) {
                String name = f.getName();
                Double accumulator = 0.0;
                for(Statistic statistic : statistics){accumulator += ((Number)f.get(statistic)).doubleValue();}
                if(accumulator != 0.0) {combinedStatistics.add(new Object[]{name,accumulator,accumulator/size});}}
            return combinedStatistics;}
        catch(Exception e) {e.printStackTrace();System.exit(1);}
        return null;}
}
