package Management.GIU;

import Management.GlobalParameters;
import Management.Parameters;

import javax.swing.*;
import java.awt.*;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class Frame {

    static JFrame frame;
    static BlockingQueue<Object> queue = new LinkedBlockingQueue();

    public static JFrame openFrame() {
        frame = new JFrame("QUSat Control Parameters");
        frame.setSize(500, 500);
        frame.setLocationRelativeTo(null);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.getContentPane().add(ParameterPanel.createPanel(parameters), BorderLayout.CENTER);
        JButton exitButton = new JButton("Exit");
        exitButton.addActionListener(e -> {
            queue.add(1);
            frame.setVisible(false);
            frame.dispose();
        });
        frame.getContentPane().add(exitButton, BorderLayout.SOUTH);
        frame.setVisible(true);
        return frame;
    }
    static Parameters parameters = GlobalParameters.parameters;

    public static void main(String[] args) throws Exception {
        openFrame();
        while(queue.isEmpty()) {}
        queue.clear();
        System.out.println("B " + parameters.parameters.get(0).value);}


}
