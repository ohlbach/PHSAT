package Management.GIU;

import javax.swing.*;
import java.awt.*;

public class GUITest extends JFrame {
    private JTextField field1;
    private JTextField field2;
    private JTextField field3;

    public GUITest() {
        // Frame-Einstellungen
        setTitle("QUSat Control Parameters");
        setSize(1000, 1000);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLocationRelativeTo(null);

        // Erstellung der Textfelder
        field1 = new JTextField(20);
        field2 = new JTextField(20);
        field3 = new JTextField(20);

        // Adding to the frame
        setLayout(new GridLayout(3, 1));
        add(field1);
        add(field2);
        add(field3);
    }

    public static void createAndShowGUI() {
        // Create and set up the window.
        GUITest frame = new GUITest();
        frame.pack();
        frame.setVisible(true);
    }

    public static void main(String[] args) {
        //Schedule a job for the event dispatching thread:
        //creating and showing this application's GUI.
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                createAndShowGUI();
            }
        });
    }
}