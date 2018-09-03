package Utilities;

/**
 * Created by Ohlbach on 03.09.2018.
 */
public class Utilities {

    public static Integer parseInteger(String place, String value, StringBuffer errors) {
        try{return Integer.parseInt(value);}
        catch(NumberFormatException ex) {errors.append(place+": " + value + " is no integer.\n");}
        return null;}
}
