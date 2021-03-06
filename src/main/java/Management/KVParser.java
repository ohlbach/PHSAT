package Management;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

/**
 * Created by Ohlbach on 03.09.2018.
 * <br>
 * This parser reads and splits grouped  key-value pairs.<br>
 * The groups are indicated by special top-keys, for example "global", "problem", "solver".<br>
 * Each of these  top-keys introduces a block of key-value pairs.<br>
 * There may be arbitrary many of these blocks, arbitrarily intermixed.
 *<br><br>
 * The result ist a HashMap, one entry for each top-key, an ArrayList of HashMaps.<br>
 * Each of these HashMaps contains the key-value pairs (as Strings).
 *<br><br>
 * Empty lines are ignored. Comments can be added after //
 * The first block can be preceded by arbitrarily many header-strings.
 *<br><br>
 * The result of the parsing can be accessed by the public variables: header and kvList.
 *<br><br>
 * The class is used mainly for parsing specifications with all the control parameters for the PHSat system.
 *
 */
public class KVParser {
    /** The header lines before the first top-key.*/
    public StringBuilder header = new StringBuilder();

    /** The map of block-lists with the key-value pairs. */
    public HashMap<String,ArrayList<HashMap<String,String>>> kvList = new HashMap<>();

    private HashSet<String> topKeys = new HashSet<>();

    /** creates a parser
     *
     * @param keys the list of top-keys.
     */
    public KVParser(String... keys) {
        for(String key : keys) {
            topKeys.add(key);
            kvList.put(key,new ArrayList<>());}}

    /** returns the entry of one of the top-keys
     *
     * @param key a top.key
     * @return the entries for the top-key.
     */
    public ArrayList<HashMap<String,String>> get(String key) {
        return kvList.get(key);}

    public void set(String key, ArrayList<HashMap<String,String>> parameters) {
        kvList.put(key,parameters);}

    private boolean inHeader = true;
    private HashMap<String,String> currentMap = null;

    /** parses a single line
     *
     * @param line the line to be parsed.
     */
    public void addLine(String line) {
        line = line.trim();
        if(line.isEmpty() || line.startsWith("%")) {return;}
        String[] parts = line.split("\\s*//",2)[0].split("\\s*[=,:, ]+\\s*",2);
        String key = parts[0];
        if(topKeys.contains(key)){inHeader = false;}
        if(inHeader) {header.append(line+"\n"); return;}

        if(topKeys.contains(key)) {
            currentMap = new HashMap<>();
            kvList.get(key).add(currentMap);}
        currentMap.put(key,parts.length > 1 ? parts[1] : "true");}

    /** reads and parses the lines from an InputStream.
     * IO-Error leads to System.exit
     *
     * @param in the stream from where the lines are to be read.
     */
    public void parseStream(InputStream in) {
        BufferedReader reader = new BufferedReader(new InputStreamReader(in));
        String line;
        try{while((line = reader.readLine())!= null) {addLine(line);}}
        catch (IOException e) {e.printStackTrace(); System.exit(1);}}


    /** Parses a file.
     * IO-Error leads to System.exit
     *
     * @param filename of the file to be parsed.
     * @return true
     */
    public boolean parseFile(String filename) {
        try {parseStream(new FileInputStream(filename));}
        catch (FileNotFoundException e) {
            System.err.println("Cannot read from file " + filename);
            e.printStackTrace();
            System.exit(1);}
        return true;}

    /** parses an entire string.
     *
     * @param lines the string to be parsed.
     */
    public void parseString(String lines) {
        for(String line : lines.split("\n")) {addLine(line);}}

    /** lists the state of the parser.
     *
     * @return a description of the parsing-result.
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Top-Keys: ").append(topKeys.toString()).append("\n\n");
        st.append("HEADER\n");
        st.append(header.toString());
        st.append("KEY-VALUES:\n");
        for(Map.Entry<String,ArrayList<HashMap<String,String>>> entry : kvList.entrySet()) {
            ArrayList<HashMap<String,String>> list = entry.getValue();
            for(HashMap<String,String> map : list) {
                for(Map.Entry<String,String> kv : map.entrySet()) {
                    st.append(kv.getKey()).append(" = ").append(kv.getValue()).append("\n");}
                st.append("\n");}}
        return st.toString();
    }




}
