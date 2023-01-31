package Utilities;

import java.io.*;
import java.util.Iterator;

/** This iterator iterates over the lines in a file.
 */
public class FileIterator implements Iterator<String> {
    /** the reader for the file. */
    BufferedReader reader;

    /** the next line to be returned. */
    String line;

    /** opens the file and creates the iterator.
     *
     * @param filename   the name of the file.
     * @throws FileNotFoundException if the file cannot be openened.
     */
    public FileIterator(String filename) throws FileNotFoundException {
        reader = new BufferedReader(new InputStreamReader(new FileInputStream(filename)));}

    /** checks if the file has another line to be read
     *
     * @return true if there is still a line to be read.
     */
    @Override
    public boolean hasNext() {
        try{line = reader.readLine();}
        catch(IOException ex) {line = null;}
        return line != null;}

    /** returns the next line in the file.
     * The line does not end with the \n character.
     *
     * @return the next line in the file.
     */
    @Override
    public String next() {
        return line;}
}
