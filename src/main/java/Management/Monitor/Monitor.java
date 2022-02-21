package Management.Monitor;

public abstract class Monitor {
    public boolean monitoring = false;
    public abstract void print(String id, String... messages);
    public  abstract void println(String id, String... messages);
    public abstract void print(String id, StringBuilder messages);
    public abstract boolean wasFilled();
    public abstract void flush(boolean close);
}
