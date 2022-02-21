package Management.Monitor;

public interface Monitor {
    void print(String id, String... messages);
    void println(String id, String... messages);
    boolean wasFilled();
    void flush(boolean close);
}
