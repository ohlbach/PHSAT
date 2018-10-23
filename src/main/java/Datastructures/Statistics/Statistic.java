package Datastructures.Statistics;

import Coordinator.Processor;
import Utilities.Utilities;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Objects;

/**
 * Created by ohlbach on 11.10.2018.
 */
public class Statistic {
    public Processor processor;
    public Statistic(Processor processor) {this.processor = processor;}
    public long elapsedTime = 0;


    /** This method extracts the statistics fields from the Statistic object and collects them in an array
     *
     * @param statistic an instance of a Statistic-subclass
     * @param zeros if true then zero-values are also included
     * @return an array with entries: [name,value]
     */
    public static ArrayList<Object[]> extractStatistic(Statistic statistic ,boolean zeros) {
        ArrayList<Object[]> values = new ArrayList<>();
        try{
            for(Field f : statistic.getClass().getFields()) {
                String name = f.getName();
                Object value = f.get(statistic);
                if(value != null && value instanceof Number) {
                    float n = ((Number)value).floatValue();
                    if(n != 0f || zeros) {values.add(new Object[]{name,n});}}}}
        catch(Exception ex) {ex.printStackTrace();System.exit(1);}
        return values;}




    public static String statisticToString(String id, ArrayList<Object[]> values) {
        if(values.isEmpty()) {return "";}
        StringBuilder st = new StringBuilder();
        String format = "%-"+Utilities.maxLength(values,(a -> (String)((Object[])a)[0])) + "s %"+
        Math.max(id.length(),Utilities.maxLength(values,(a -> (a != null) ?
                Utilities.numberString((((Object[])a)[1]).toString()) : ""))) + "s\n";
        st.append(String.format(format," ",id));
        for(Object[] value : values) {st.append(String.format(format,(String)value[0], Utilities.numberString(value[1].toString())));}
        return st.toString();}

    public static String statisticToCSV(String id, String separator, ArrayList<Object[]> values) {
        if(values.isEmpty()) {return "";}
        StringBuilder st = new StringBuilder();
        st.append(separator).append(id).append("\n");
        for(Object[] value : values) {st.append((String)value[0]).append(separator).append(Utilities.numberString(value[1].toString())).append("\n");}
        return st.toString();}



    /** combines several statistics of the same type.
     *
     * @param statistics of the same type
     * @return a list of triples [name, value_1,...,value_n, accumulated values, average value]
     */
    public static ArrayList<Object[]>  combineSameStatistics(Statistic[] statistics, boolean zeros) {
        int size = statistics.length;
        ArrayList<Object[]> combinedStatistics = new ArrayList<>();
        Object[] ids = new Object[size];
        for(int i = 0; i < size; ++i) {ids[i] = statistics[i].processor.id;}
        combinedStatistics.add(ids);
        try{
            for(Field f : statistics[0].getClass().getFields()) {
                String name = f.getName();
                float accumulator = 0.0f;
                Object[] line = new Object[size+3];
                line[0] = name;
                for(int i = 0; i < size; ++i) {
                    Statistic statistic = statistics[i];
                    Object value = f.get(statistic);
                    if(value == null || !(value instanceof Number)) {continue;}
                    float n = ((Number)value).floatValue();
                    line[i+1] = n;
                    accumulator += n;}
                line[size+1] = accumulator;
                line[size+2] = accumulator /  size;
                if(accumulator != 0f || zeros)  combinedStatistics.add(line);}
            return combinedStatistics;}
        catch(Exception e) {e.printStackTrace();System.exit(1);}
        return null;}

    /** combines several statistics of different type.
     *
     * @param statistics of the same type
     * @return a list of triples [name, value_1,...,value_n, accumulated values, average value]
     */
    public static ArrayList<Object[]>  combineDifferentStatistics(Statistic[] statistics, boolean zeros) {
        int size = statistics.length;
        if(size == 1) {return extractStatistic(statistics[0],zeros);}
        ArrayList<String> names = new ArrayList<>();
        for(Statistic statistic: statistics) {
            for(Field f : statistic.getClass().getFields()) {
                String name = f.getName();
                if(!names.contains(name)) {names.add(name);}}}
        ArrayList<Object[]> combinedStatistics = new ArrayList<>();
        Object[] ids = new Object[size];
        for(int i = 0; i < size; ++i) {ids[i] = statistics[i].processor.id;}
        combinedStatistics.add(ids);
        try{
            for(String name : names) {
                Object[] line = new Object[size+3];
                line[0] = name;
                float accumulator = 0.0f;
                int counter = 0;
                for(int i = 0; i < size; ++i) {
                    Statistic statistic =  statistics[i];
                    Field f = null;
                    try{f = statistic.getClass().getField(name);} catch(Exception ex) {}
                    if(f == null)  {line[i+1] = null;}
                    else {
                        Object value = f.get(statistic);
                        if(value == null || !(value instanceof Number)) {continue;}
                        ++counter;
                        float n = ((Number)value).floatValue();
                        line[i+1] = n;
                        accumulator += n;}}
                line[size+1] = accumulator;
                line[size+2] = accumulator /  counter;
                if(accumulator != 0f || zeros) combinedStatistics.add(line);}
            return combinedStatistics;}
        catch(Exception e) {e.printStackTrace();System.exit(1);}
        return null;}

    /** This method turns the statistics information into a printable string.
     *
     * @param statistics A list of entries: [key, nunber,...,number, sum, average]
     * @return           the formatted string
     */
    public static String statisticToString(ArrayList<Object[]> statistics) {
        Object[] ids = statistics.get(0);
        int size = statistics.get(1).length;
        if(size == 2) {return statisticToString((String)ids[0],statistics);}
        StringBuilder st = new StringBuilder();
        int[] formats = new int[size];
        for(int i = 0; i < size; ++i) {
            int length = 0;
            if(i > 0 && i < size-2) {length = ((String)ids[i-1]).length();}
            int j = i;
            length = Math.max(length,Utilities.maxLength(statistics,
                    (line-> (j >= ((Object[])line).length || (((Object[])line)[j] ==null)) ? "" :
                        Utilities.numberString(((Object[])line)[j]))));
            if(i > 0 && i < size - 3) {length = Math.max(length,((String)ids[i]).length());}
            if(i >= size - 2) {length = Math.max(length,4);}
            formats[i] = length;}
        st.append(String.format("%"+formats[0]+"s "," "));
        for(int i = 0; i < ids.length; ++i) {
            st.append(String.format("%"+formats[i+1]+"s ",ids[i]));}
        st.append(String.format("%"+formats[size-2]+"s ", "Sum"));
        st.append(String.format("%"+formats[size-1]+"s\n", "Avg."));
        for(int i = 1; i < statistics.size(); ++i) {
            Object[] line =  statistics.get(i);
            st.append(String.format("%-"+formats[0]+"s ",(String)line[0]));
            for(int j = 1; j < line.length; ++j) {
                st.append((line[j] == null) ? String.format("%"+formats[j]+"s "," ") :
                        String.format("%"+formats[j]+"s ",Utilities.numberString(line[j])));}
            st.append("\n");}
        return st.toString();
        }

    /** This method turns the statistics information into a string which can be printed to a csv-file
     *
     * @param statistics A list of entries: [key, nunber,...,number, sum, average]
     * @param separator  for the csv-entries
     * @return           a csv-printable string.
     */

    public static String statisticToCSV(String separator, ArrayList<Object[]> statistics) {
        Object[] ids = statistics.get(0);
        int size = statistics.size();
        if(size == 2) {return statisticToCSV((String)ids[0],separator,statistics);}
        StringBuilder st = new StringBuilder();
        st.append(separator);
        for(int i = 1; i < ids.length; ++i) {st.append((String)ids[i]).append(separator);}
        st.append("Sum").append(separator).append("Avg.").append("\n");
        for(Object[] line : statistics) {
            st.append(Utilities.join(line,separator,(o -> (o == null) ? "" : Utilities.numberString(o)))).append("\n");}
        return st.toString();
    }

    public String toString(boolean zeros) {
        return Statistic.statisticToString(processor.id,Statistic.extractStatistic(this,zeros));}

    public String toString() {
        return Statistic.statisticToString(processor.id,Statistic.extractStatistic(this,false));}

}
