package Management.GIU;

import Management.GlobalParameters;
import Management.Parameter;
import Management.Parameters;
import ProblemGenerators.ProblemGenerator;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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
        Container contentPane = frame.getContentPane();
        contentPane.setLayout(new BorderLayout());
        contentPane.add(westPane(),BorderLayout.WEST);
        JTabbedPane tabpane = new JTabbedPane
                (JTabbedPane.TOP,JTabbedPane.SCROLL_TAB_LAYOUT );
        tabpane.add("Global Parameters", createParameterPanel("Global Parameters for all solver jobs",
                globalParameters));
        tabpane.add("Clause Generators",createGeneratorPanel(generatorParams));
        contentPane.add(tabpane,BorderLayout.CENTER);

        JButton exitButton = new JButton("Exit");
        exitButton.addActionListener(e -> {
            queue.add(1);
            frame.setVisible(false);
            frame.dispose();
        });
        contentPane.add(exitButton, BorderLayout.SOUTH);
        //frame.pack();
        frame.setVisible(true);
        return frame;
    }

    private static JPanel westPane() {
        JPanel westPane = new JPanel();
        westPane.setLayout(new BoxLayout(westPane, BoxLayout.Y_AXIS));
        westPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
        westPane.add(Box.createVerticalStrut(50));

        JLabel recentLabel = new JLabel("Recent");
        recentLabel.setFont(recentLabel.getFont().deriveFont(Font.BOLD, 16));
        westPane.add(recentLabel);

        JLabel loadLabel = new JLabel("Load");
        loadLabel.setFont(loadLabel.getFont().deriveFont(Font.BOLD, 16));
        westPane.add(loadLabel);

        JLabel saveLabel = new JLabel("Save");
        saveLabel.setFont(saveLabel.getFont().deriveFont(Font.BOLD, 16));
        westPane.add(saveLabel);

        JLabel runLabel = new JLabel("Run");
        runLabel.setFont(runLabel.getFont().deriveFont(Font.BOLD, 16));
        westPane.add(runLabel);
        return westPane;
    }

    public static JTabbedPane createGeneratorPanel(ArrayList<Parameters> parameters) {
        JTabbedPane tabpane = new JTabbedPane(JTabbedPane.TOP,JTabbedPane.SCROLL_TAB_LAYOUT );
        for(Parameters params : parameters) {
            JPanel parametersPanel = createParameterPanel(params.description,params);
            JPanel generatorPanel = new JPanel(flowLayout);
            JLabel generateLabel = new JLabel("Generate Clauses");
            generateLabel.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    super.mouseClicked(e);
                    generateClauses(params);}});
            generatorPanel.add(generateLabel);
            parametersPanel.add(generatorPanel);
            tabpane.addTab(params.title, parametersPanel);}
        return tabpane;}

    private static void generateClauses1(Parameters params) {
        StringBuilder errors = new StringBuilder();
        String clauses = params.operation.apply(params,errors);
        if(!errors.toString().isEmpty()) {
            JOptionPane.showMessageDialog(frame, errors.toString(), "Error", JOptionPane.INFORMATION_MESSAGE);
            return;}
        JTextArea textArea = new JTextArea(30, 50); // adjust size as per your requirement
        textArea.setText(clauses);
        textArea.setEditable(false);
        JScrollPane scrollPane = new JScrollPane(textArea);
        JOptionPane.showMessageDialog(null, scrollPane, "Clauses", JOptionPane.INFORMATION_MESSAGE);
    }

    private static void generateClauses(Parameters params) {
        StringBuilder errors = new StringBuilder();
        String clauses = params.operation.apply(params,errors);
        if(!errors.toString().isEmpty()) {
            JOptionPane.showMessageDialog(frame, errors.toString(), "Error", JOptionPane.INFORMATION_MESSAGE);
            return;}
        final JDialog dialog = new JDialog();
        dialog.setTitle("Clauses");

        JButton extraButton = new JButton("Save Clauses");
        extraButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                // Handle MouseEvent here
                System.out.println("Extra Button Clicked");
                dialog.setVisible(false);
            }
        });

        JButton okButton = new JButton("OK");
        okButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                dialog.setVisible(false);
            }
        });

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(extraButton);
        buttonPanel.add(okButton);

        JTextArea textArea = new JTextArea(30,50);
        textArea.setEditable(false);

        // Add your large text here
        textArea.setText(clauses);

        JScrollPane scrollPane = new JScrollPane(textArea);

        JPanel contentPanel = new JPanel();
        contentPanel.setLayout(new BorderLayout());
        contentPanel.add(scrollPane, BorderLayout.CENTER);
        contentPanel.add(buttonPanel, BorderLayout.PAGE_END);

        dialog.setContentPane(contentPanel);
        dialog.pack();
        dialog.setLocationRelativeTo(null);
        dialog.setVisible(true);
    }

    public static JPanel createParameterPanel(String header, Parameters parameters) {
        JPanel parametersPanel = new JPanel();
        parametersPanel.setLayout(new BoxLayout(parametersPanel, BoxLayout.Y_AXIS));
        parametersPanel.setBorder(BorderFactory.createLineBorder(Color.BLUE, 2));
        if(header != null) {
            JTextArea textArea = new JTextArea(header);
            textArea.setLineWrap(true); // allow line wrapping
            textArea.setWrapStyleWord(true); // wrap lines at word boundaries
            textArea.setEditable(false);
            textArea.setMaximumSize(new Dimension(Integer.MAX_VALUE, textArea.getPreferredSize().height));
            //headerLabel.setToolTipText(header);
            parametersPanel.add(textArea);}
        for (Parameter parameter : parameters.parameters) {
            JPanel panel = null;
            switch(parameter.type) {
                case String: panel = textField(parameter,parameters);
                    panel.setMaximumSize(new Dimension(Integer.MAX_VALUE, panel.getMinimumSize().height));
                    parametersPanel.add(panel); break;
                case OneOf:  panel = oneOf(parameter);
                    panel.setMaximumSize(new Dimension(Integer.MAX_VALUE, panel.getMinimumSize().height));
                    parametersPanel.add(panel); break;
                case Boolean: panel = bool(parameter);
                    panel.setMaximumSize(new Dimension(Integer.MAX_VALUE, panel.getMinimumSize().height));
                    parametersPanel.add(panel); break;
            }
        }

        return parametersPanel;
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
