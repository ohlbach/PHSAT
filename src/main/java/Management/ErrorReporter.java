package Management;

public class ErrorReporter {
    public static void reportError(String error) {
        System.out.println("Error: " + error);}

    public static void reportErrorAndStop(String error) {
        System.out.println("Error: " + error);
        System.out.println("System is stopped.");
        System.exit(1);}

    public static void reportWarning(String warning) {
        System.out.println("Warning: " + warning);
    }
}
