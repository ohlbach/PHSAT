package Management.GIU;

import Management.GlobalParameters;
import Management.Parameters;
import Management.Parameter;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class Frame {

    static JFrame frame;
    static BlockingQueue<Object> queue = new LinkedBlockingQueue();
    public static GlobalParameters globalParameters = new GlobalParameters();

    public static JFrame openFrame() {
        Parameters parameters = globalParameters.parameters;
        frame = new JFrame("QUSat Control Parameters");
        frame.setSize(500, 500);
        frame.setLocationRelativeTo(null);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.getContentPane().add(createPanel(parameters), BorderLayout.CENTER);
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



    public static JPanel createPanel(Parameters parameters) {
        JPanel panel = new JPanel(new BorderLayout());
        JLabel title = new JLabel(parameters.title);
        title.setFont(title.getFont().deriveFont(Font.BOLD));
        title.setForeground(Color.blue);
        panel.add(title, BorderLayout.NORTH);
        JPanel parametersPanel = new JPanel(flowLayout);
        for (Parameter parameter : parameters.parameters) {
            switch(parameter.type) {
                case String: parametersPanel.add(textField(parameter)); break;
                case OneOf:  parametersPanel.add(oneOf(parameter)); break;
                case Boolean: parametersPanel.add(bool(parameter)); break;
            }
        }
        panel.add(parametersPanel, BorderLayout.CENTER);
        return panel;
    }
    private static FlowLayout flowLayout = new FlowLayout(FlowLayout.LEFT);

    private static JPanel textField(Parameter parameter) {
        JPanel textPanel = new JPanel(flowLayout);
        textPanel.add(makeLabel(parameter));
        parameter.value = parameter.defaultName;
        JTextField textField = new JTextField(parameter.defaultName,15);
        textField.addMouseListener(new MouseAdapter() {
            public void mouseExited(MouseEvent e){
                parameter.value = textField.getText();}});
        textPanel.add(textField);
        return textPanel;
    }

    private static JPanel oneOf(Parameter parameter) {
        JPanel textPanel = new JPanel(flowLayout);
        textPanel.add(makeLabel(parameter));
        parameter.value = parameter.defaultName;
        ButtonGroup group = new ButtonGroup();
        for(Parameter param : parameter.parameters.parameters) {
            JRadioButton radioButton = new JRadioButton(param.name);
            radioButton.addActionListener(e -> {parameter.value = param.name;});
            group.add(radioButton);
            textPanel.add(radioButton);}
        return textPanel;}

    private static JPanel bool(Parameter parameter) {
        JPanel textPanel = new JPanel(flowLayout);
        textPanel.add(makeLabel(parameter));
        parameter.value = parameter.defaultName;
        ButtonGroup group = new ButtonGroup();
        JRadioButton trueButton = new JRadioButton("true");
        JRadioButton falseButton = new JRadioButton("false");
        trueButton.addActionListener(e -> {parameter.value = true;});
        falseButton.addActionListener(e -> {parameter.value = false;});
        group.add(trueButton);
        textPanel.add(trueButton);
        group.add(falseButton);
        textPanel.add(falseButton);
        return textPanel;}

    private static JLabel makeLabel(Parameter parameter) {
        JLabel label = new JLabel(parameter.name);
        if(parameter.description != null) {
            label.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseEntered(MouseEvent e) {
                    JOptionPane.showMessageDialog(frame, parameter.description, "Description", JOptionPane.INFORMATION_MESSAGE);}});}
        return label;}


    public static void main(String[] args)  {
        System.out.println("START");
        openFrame();
        while(queue.isEmpty()) {}
        queue.clear();
        System.out.println( globalParameters.parameters.toString());
    }


}
