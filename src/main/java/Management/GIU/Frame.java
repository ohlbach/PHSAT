package Management.GIU;

import Management.GlobalParameters;
import Management.Parameter;
import Management.Parameters;
import ProblemGenerators.ProblemGenerator;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.function.BiFunction;

public class Frame {

    static JFrame frame;
    static BlockingQueue<Object> queue = new LinkedBlockingQueue();
    public static GlobalParameters globalParams = new GlobalParameters();
    public static ArrayList<Parameters> generatorParams = ProblemGenerator.makeParameters();

    public static JFrame openFrame() {
        Parameters globalParameters = globalParams.parameters;
        frame = new JFrame("QUSat Control Parameters");
        frame.setSize(1300, 500);
        frame.setLocationRelativeTo(null);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.getContentPane().add(createPanel(globalParameters), BorderLayout.CENTER);
        frame.getContentPane().add(createGeneratorPanel(generatorParams), BorderLayout.EAST);
        JButton exitButton = new JButton("Exit");
        exitButton.addActionListener(e -> {
            queue.add(1);
            frame.setVisible(false);
            frame.dispose();
        });
        frame.getContentPane().add(exitButton, BorderLayout.SOUTH);
        //frame.pack();
        frame.setVisible(true);
        return frame;
    }

    public static JTabbedPane createGeneratorPanel(ArrayList<Parameters> parameters) {
        JTabbedPane tabpane = new JTabbedPane(JTabbedPane.TOP,JTabbedPane.SCROLL_TAB_LAYOUT );
        for(Parameters params : parameters) {
            JPanel panel = createPanel(params);
            tabpane.addTab(params.title, panel);}
        return tabpane;}


    public static JPanel createPanel(Parameters parameters) {
        JPanel panel = new JPanel(new BorderLayout());
        JLabel title = new JLabel(parameters.title);
        title.setFont(title.getFont().deriveFont(Font.BOLD));
        title.setForeground(Color.blue);
        panel.add(title, BorderLayout.NORTH);
        JPanel parametersPanel = new JPanel(flowLayout);
        parametersPanel.setBorder(BorderFactory.createLineBorder(Color.BLUE, 2));
        for (Parameter parameter : parameters.parameters) {
            switch(parameter.type) {
                case String: parametersPanel.add(textField(parameter,parameters)); break;
                case OneOf:  parametersPanel.add(oneOf(parameter)); break;
                case Boolean: parametersPanel.add(bool(parameter)); break;
                case Integer: parametersPanel.add(integer(parameter)); break;
            }
        }
        panel.add(parametersPanel, BorderLayout.CENTER);
        return panel;
    }
    private static FlowLayout flowLayout = new FlowLayout(FlowLayout.LEFT);

    private static JPanel textField(Parameter parameter,Parameters parameters) {
        JPanel textPanel = new JPanel(flowLayout);
        textPanel.add(makeLabel(parameter));
        Object oldValue = parameter.value;
        String defaultValue = parameter.defaultValue;
        JTextField textField = new JTextField(parameter.defaultValue,Math.max(15,parameter.defaultValue.length()));
        textField.addMouseListener(new MouseAdapter() {
            public void mouseExited(MouseEvent e){
                System.out.println("PV " + textField.getText() + "  "  +parameter.value);
                if(parameter.parser != null) {
                    StringBuilder errors = new StringBuilder();
                    parameter.value = parameter.parser.apply(textField.getText(),errors);
                    if(!errors.isEmpty()) {
                        JOptionPane.showMessageDialog(frame,errors.toString(),"Error", JOptionPane.INFORMATION_MESSAGE);
                        SwingUtilities.invokeLater(() -> textField.setText(defaultValue));
                        parameter.value = oldValue;}
                    BiFunction<Parameters, StringBuilder, Boolean> finalCheck = parameters.finalCheck;
                    if(finalCheck != null && errors.isEmpty()) {
                        boolean finalCheckResult = finalCheck.apply(parameters, errors);
                        if (!finalCheckResult) {
                            JOptionPane.showMessageDialog(frame, errors.toString(), "Error", JOptionPane.INFORMATION_MESSAGE);
                            SwingUtilities.invokeLater(() -> textField.setText(defaultValue));
                            parameter.value = oldValue;
                        }}}
                else parameter.value = textField.getText();}});
        textPanel.add(textField);
        return textPanel;
    }
    private static JPanel integer(Parameter parameter) {
        JPanel textPanel = new JPanel(flowLayout);
        textPanel.add(makeLabel(parameter));
        parameter.value = parameter.defaultValue;
        JTextField textField = new JTextField(parameter.defaultValue,15);
        textField.addMouseListener(new MouseAdapter() {
            public void mouseExited(MouseEvent e){
                StringBuilder errors = new StringBuilder();
                parameter.value = parameter.parser.apply(textField.getText(),errors);
                if(!errors.isEmpty()) {
                    JOptionPane.showMessageDialog(frame,errors.toString(),"Error", JOptionPane.INFORMATION_MESSAGE);
                }
                else System.out.println(((IntArrayList)parameter.value).toString());}});
        textPanel.add(textField);
        return textPanel;
    }
    private static JPanel oneOf(Parameter parameter) {
        JPanel textPanel = new JPanel(flowLayout);
        textPanel.add(makeLabel(parameter));
        parameter.value = parameter.defaultValue;
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
        parameter.value = parameter.defaultValue;
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
                public void mouseClicked(MouseEvent e) {
                    JOptionPane.showMessageDialog(null, parameter.description, "Description",
                            JOptionPane.INFORMATION_MESSAGE);};});}
        return label;}


    public static void main(String[] args)  {
        System.out.println("START");
        openFrame();
        while(queue.isEmpty()) {}
        queue.clear();
        System.out.println( globalParams.parameters.toString());
    }


}
