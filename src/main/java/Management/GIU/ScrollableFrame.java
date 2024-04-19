package Management.GIU;

import javax.swing.*;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.OutputStream;
import java.io.PrintStream;

public class ScrollableFrame  extends JFrame {

    private JTextArea textArea;

    public PrintStream printStream = null;

    public ScrollableFrame(int width, int height, int locationX, int locationY, String title) {
        // Create a JTextArea
        textArea = new JTextArea();
        // Wrap it in a JScrollPane
        JScrollPane scrollPane = new JScrollPane(textArea,JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        setTitle(title);
        // Add JScrollPane to the frame
        add(scrollPane, BorderLayout.CENTER);

        // Redirect System.out to textArea
        printStream = new PrintStream(new TextAreaOutputStream());

        // Set frame's properties
        setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        addWindowListener(new WindowAdapter(){
            public void windowClosing(WindowEvent e){
                System.out.println( title + " window is closing at " + System.nanoTime() + " ns");}});
        setSize(width, height);
        setLocation(locationX, locationY);
        setVisible(true);
    }

    public static PrintStream getPrintStream(int width, int height, int locationX, int locationY,String title) {
        ScrollableFrame[] sf = new ScrollableFrame[1];
        try {
            SwingUtilities.invokeAndWait(() -> {
                sf[0] = new ScrollableFrame(width, height, locationX, locationY, title);
            });
        }
        catch(Exception e) {System.out.println(e); System.exit(1);}

        return  sf[0].printStream;
    }

    public class TextAreaOutputStream extends OutputStream {
        @Override
        public void write(int b) {
            // Redirects data to the text area
            textArea.append(String.valueOf((char)b));
            // Scrolls the text area to the end of data
            textArea.setCaretPosition(textArea.getDocument().getLength());
        }}

    public static void main(String[] args) throws Exception {
        PrintStream[] st = new PrintStream[1];
        st[0] = ScrollableFrame.getPrintStream(500,400,100,100,"Logfile");
        final Object syncObject = new Object();

        PrintStream ps = st[0];
        for(int i = 0; i < 100; ++i)
            ps.print("a " + i );

        synchronized(syncObject) {
            syncObject.wait(1000);
        }

        st[0].println("CL");
        st[0].close();


    }


    }
