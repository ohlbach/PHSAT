package Datastructures.Statistics;

import Coordinator.Processor;
import Datastructures.Clauses.Clause;
import Utilities.Utilities;
import com.sun.org.glassfish.gmbal.Description;

import java.lang.reflect.Field;
import java.util.ArrayList;

/** This is the superclass of all Statistics classes.
 * It provides some top-level parameters and some static methods for accumulating and printing statistics.
 * The accumulate methods use reflection to collect statistics values from the subclasses.
 *
 * Created by ohlbach on 11.10.2018.
 */
public class Statistic {
    /** An identifier for the statistics */
    public String id = "";
    /** The processor which generated the statistics */
    public Processor processor;

    public static ArrayList<Class> statisticsClasses = new ArrayList<>();
    static{statisticsClasses.add(Statistic.class);}

    /** constructs a Statistics instance for a particular processor
     *
     * @param processor The processor for which the statistics values are collected
     */
    public Statistic(Processor processor) {
        this.processor = processor;
        if(processor != null)  id = processor.id;}

    /** This constructs a statistics instance which is independent of a processor
     *
     * @param id for identifying the statistics
     */
    public Statistic(String id) {this.id = id;}

    @Description("processor's elapsed time in microseconds")
    public long elapsedTime = 0;

    /** the subclasses may overwrite this method to add some observers */
    public void addStatisticsObservers() {}

    /** the subclasses may overwrite this method to remove some observers */
    public void removeStatisticsObservers() {}


    /** This method extracts the statistics fields from the Statistic object and collects them in an array.
     *
     * @param zeros if true then zero-values are also included
     * @return an array with entries: [name,value]
     */
    public ArrayList<Object[]> extractStatistic(boolean zeros) {
        ArrayList<Object[]> values = new ArrayList<>();
        try{
            for(Field f : getClass().getFields()) {
                String name = f.getName();
                Object value = f.get(this);
                if(value != null && value instanceof Number) {
                    float n = ((Number)value).floatValue();
                    if(n != 0f || zeros) {values.add(new Object[]{name,n});}}}}
        catch(Exception ex) {ex.printStackTrace();System.exit(1);}
        return values;}

    /** This method turns the values extracted by the extract-method into a formatted string.
     *
     * @param id     the statistic's id
     * @param values an array with tuples [name,value]
     * @return a formatted string
     */
    public static String statisticToString(String id, ArrayList<Object[]> values) {
        if(values.isEmpty()) {return "";}
        StringBuilder st = new StringBuilder();
        String format = "%-"+Utilities.maxLength(values,(a -> (String)((Object[])a)[0])) + "s %"+
                Math.max(id.length(),Utilities.maxLength(values,(a -> (a != null) ?
                        Utilities.numberString((((Object[])a)[1]).toString()) : ""))) + "s\n";
        st.append(String.format(format," ",id));
        for(Object[] value : values) {st.append(String.format(format,(String)value[0], Utilities.numberString(value[1].toString())));}
        return st.toString();}

    /** This method turns the values extracted by the extract-method into a csv-printable string.
     *
     * @param id     the statistic's id
     * @param separator to separate the items (e.g. ',')
     * @param values an array with tuples [name,value]
     * @return       a csv-printable string
     */
    public static String statisticToCSV(String id, String separator, ArrayList<Object[]> values) {
        if(values.isEmpty()) {return "";}
        StringBuilder st = new StringBuilder();
        st.append(separator).append(id).append("\n");
        for(Object[] value : values) {st.append((String)value[0]).append(separator).append(Utilities.numberString(value[1].toString())).append("\n");}
        return st.toString();}

    /** This method generates a formatted string with the statistics information
     *
     * @param zeros  if true then all fields with zero-value are also included.
     * @return a formatted string with the statistics values
     */
    public String toString(boolean zeros) {
        return Statistic.statisticToString(id,extractStatistic(zeros));}

    /** This method generates a formatted string with the statistics information.
     * zero-values are not included.
     *
     * @return a formatted string with the statistics values
     */
    public String toString() {
        return Statistic.statisticToString(id,extractStatistic(false));}


    /** combines several statistics of the same type.
     *
     * @param statistics an array of statistics of the same type
     * @param zeros if true then the 0-values are also included.
     * @return a list of tuples [name, value_1,...,value_n, accumulated values, average value]
     */
    public static ArrayList<Object[]>  combineSameStatistics(Statistic[] statistics, boolean zeros) {
        int size = statistics.length;
        ArrayList<Object[]> combinedStatistics = new ArrayList<>();
        Object[] ids = new Object[size];
        for(int i = 0; i < size; ++i) {ids[i] = statistics[i].id;}
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
     * @param zeros if true then the 0-values are also included.
     * @return a list of tuples [name, value_1,...,value_n, accumulated values, average value]
     */
    public static ArrayList<Object[]>  combineDifferentStatistics(Statistic[] statistics, boolean zeros) {
        int size = statistics.length;
        if(size == 1) {return statistics[0].extractStatistic(zeros);}
        ArrayList<String> names = new ArrayList<>();
        for(Statistic statistic: statistics) {
            for(Field f : statistic.getClass().getFields()) {
                String name = f.getName();
                if(!names.contains(name)) {names.add(name);}}}
        ArrayList<Object[]> combinedStatistics = new ArrayList<>();
        Object[] ids = new Object[size];
        for(int i = 0; i < size; ++i) {ids[i] = statistics[i].id;}
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

    /** This method turns the statistics information, collected by a combine-method into a printable string.
     *
     * @param statistics A list of entries: [name, value_1,...,value_n, sum, average]
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

    /** This method turns the statistics information collected by the combine-methods into a string which can be printed to a csv-file
     *
     * @param statistics A list of entries: [name, value_1,...,value_n, sum, average]
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

    /** This method collects the descriptions of all statistics fields and puts them into a formatted string
     *
     * @return a formatted string with the descriptions of all statistic fields.
     */
    public static String descriptions() {
        StringBuilder st = new StringBuilder();
        int maxNames = 0;
        for(Class clazz : statisticsClasses) {
            for(Field f : clazz.getFields()) {maxNames = Math.max(maxNames,f.getName().length());}}
        String format = "%-"+maxNames+"s  =  %s";
        for(Class clazz : statisticsClasses) {
            for(Field f : clazz.getFields()) {
                if(f.getAnnotation(Description.class) != null) {
                    st.append(String.format(format,f.getName(),f.getAnnotation(Description.class).value())).append("\n");}}}
        return st.toString();}


}
