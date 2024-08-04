package Management.GIU;

import Datastructures.Clauses.InputClauses;
import Management.*;
import ProblemGenerators.ProblemGenerator;
import Solvers.Solver;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.*;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Locale;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.function.BiFunction;


public class Frame {



    static JFrame frame;
    static BlockingQueue<Object> queue = new LinkedBlockingQueue();
    public static Parameters globalParams = GlobalParameters.makeGlobalParameters();
    public static ArrayList<Parameters> generatorParams = ProblemGenerator.makeParameters();
    public static ArrayList<Parameters> solverParams = Solver.makeParameters();

    public static JFrame openFrame() {
        StringBuilder errors = new StringBuilder();
        loadProjects(errors);
        if(!errors.isEmpty()) {
            JOptionPane.showMessageDialog(null, errors.toString() , "Error", JOptionPane.INFORMATION_MESSAGE);}

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
                globalParams));
        tabpane.add("Clause Generators",createGeneratorPanel(generatorParams));
        contentPane.add(tabpane,BorderLayout.CENTER);
        tabpane.add("Solvers",createSolverPanel(solverParams));
        contentPane.add(tabpane,BorderLayout.CENTER);

        JButton exitButton = new JButton("Exit");
        exitButton.addActionListener(e -> {
            queue.add(1);
            frame.setVisible(false);
            frame.dispose();
            System.exit(0);
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

        JLabel infoLabel = new JLabel("Info");
        infoLabel.setFont(infoLabel.getFont().deriveFont(Font.BOLD, 16));
        infoLabel.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        infoLabel.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                super.mouseClicked(e);
                JOptionPane.showMessageDialog(null,
                        "Recent:        load recent parameter configurations\n"+
                                "Load:          load parameter configuration\n"+
                                 "Save:         save parameter configuration\n"+
                                "Save Default:  save parameter configuration as default configuration\n"+
                                 "Run:          processTasks QuSat solver.",
                         "Info", JOptionPane.INFORMATION_MESSAGE);}});
        westPane.add(infoLabel);
        westPane.add(Box.createVerticalStrut(50));

        JLabel recentLabel = new JLabel("Recent");
        recentLabel.setFont(recentLabel.getFont().deriveFont(Font.BOLD, 16));
        recentLabel.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        if(!recentProjects.isEmpty()) {
            recentLabel.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                    JPopupMenu menu = new JPopupMenu();
                    for(int i = recentProjects.size()-1; i >= 0; --i) {
                        String projectName = recentProjects.get(i)[0];
                        String projectFile = recentProjects.get(i)[1];
                        JMenuItem menuItem = new JMenuItem(projectName);
                        menuItem.addActionListener(e1 -> loadParameters(new File(projectFile),false));
                        menu.add(menuItem);}
                    menu.show(e.getComponent(), e.getX(), e.getY());}});}
        westPane.add(recentLabel);

        JLabel loadLabel = new JLabel("Load");
        loadLabel.setFont(loadLabel.getFont().deriveFont(Font.BOLD, 16));

        loadLabel.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        loadLabel.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                super.mouseClicked(e);
                loadParameters();}});
        westPane.add(loadLabel);

        JLabel saveLabel = new JLabel("Save");
        saveLabel.setFont(saveLabel.getFont().deriveFont(Font.BOLD, 16));
        saveLabel.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        saveLabel.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                super.mouseClicked(e);
                saveParameters();}});
        westPane.add(saveLabel);

        JLabel defaultLabel = new JLabel("Save Default");
        defaultLabel.setFont(saveLabel.getFont().deriveFont(Font.BOLD, 16));
        defaultLabel.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        defaultLabel.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                super.mouseClicked(e);
                saveDefaultParameters();}});
        westPane.add(defaultLabel);



        JLabel runLabel = new JLabel("Run");
        runLabel.setFont(runLabel.getFont().deriveFont(Font.BOLD, 16));
        runLabel.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        runLabel.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                super.mouseClicked(e);
                runQuSatSolver();}
        });
        westPane.add(runLabel);
        return westPane;
    }

    public static void runQuSatSolver() {
        System.out.println("RUN");
        Thread thread = new Thread(()->{
            QuSatJob quSatJob = new QuSatJob(globalParams,generatorParams,solverParams);
            quSatJob.solveProblems();});
        // prepare for a new job.
        globalParams = globalParams.clone();
        ArrayList<Parameters> paramsList = new ArrayList<>(generatorParams.size());
        for(int i = 0; i < generatorParams.size(); ++i) paramsList.add(i, generatorParams.get(i).clone());
        generatorParams = paramsList;
        paramsList = new ArrayList<>(solverParams.size());
        for(int i = 0; i < solverParams.size(); ++i) paramsList.add(solverParams.get(i).clone());
        solverParams = paramsList;
        thread.start();}

    private static void saveParameters() {
        File homeDirectory = GlobalParameters.homeDirectory.toFile();
        File file = chooseFile(homeDirectory, ".txt");
        if (file == null) return;
        saveParameters(file);}

    private static void saveDefaultParameters() {
        saveParameters(QUSat.defaultFile);}

        /** Saves the parameters to a file.
         *
         * The file is chosen in a dialogue.<br>
         * The format is as follows:<br>
         * Global Parameters<br>
         * parameters<br>
         * <br>
         * Generator<br>
         * parameters<br>
         * ...<br>
         * Solver<br>
         * parameters<br>
         * ...<br>
         * <br>
         * The parameters start with the parameter group name and then lines with<br>
         * name: text representation: value string.<br>
         */
    private static void saveParameters(File file) {
        try {PrintStream stream = new PrintStream(file);
            stream.println(globalParams.toString());
            for(Parameters p: generatorParams) {
                stream.println("\nGenerator");
                stream.println(p.toString());}
            for(Parameters p: solverParams) {
                stream.println("\nSolver");
                stream.println(p.toString());}
            stream.println("END");
            stream.close();
            StringBuilder errors = new StringBuilder();
            saveProjects((String)globalParams.parameters.get(0).value,file,errors);
            if(!errors.isEmpty()) {
                JOptionPane.showMessageDialog(null, errors.toString() , "Error", JOptionPane.INFORMATION_MESSAGE);}
        } catch (IOException e) {
            JOptionPane.showMessageDialog(null, e +
                    file.getAbsolutePath() , "Error", JOptionPane.INFORMATION_MESSAGE);}
        JOptionPane.showMessageDialog(null, "Parameters saved to " +
                    file.getAbsolutePath() , "Saved", JOptionPane.INFORMATION_MESSAGE);}

    private static void loadParameters() {
        File homeDirectory = GlobalParameters.homeDirectory.toFile();
        JFileChooser chooser = new JFileChooser(homeDirectory);
        chooser.setDialogTitle("Select a file with the parameters");
        int result = chooser.showOpenDialog(null);
        if(result == JFileChooser.APPROVE_OPTION) {
            loadParameters(chooser.getSelectedFile(),false);}}

    public static void loadParameters(File file, boolean life) {
        try{
            StringBuilder errors = new StringBuilder();
            InputStream input = new FileInputStream(file);
            BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(input));
            String line = bufferedReader.readLine();
            if(!line.startsWith("Global Parameters")) {
                errors.append("'"+line + "' does not start with 'Global Parameters'\n");}
            globalParams.loadParameters(bufferedReader,errors);
            while(!(line = bufferedReader.readLine()).startsWith("END")) {
                if(line.isEmpty()) {while((line = bufferedReader.readLine()).isEmpty()) {}}
                if(line.startsWith("Generator")) {
                    line = bufferedReader.readLine();
                    boolean found = false;
                    for(Parameters p: generatorParams) {
                        if(line.startsWith(p.name)) {found = true; p.loadParameters(bufferedReader,errors);}}
                    if(!found) errors.append("unknown generator " + line);}
                if(line.startsWith("Solver")) {
                    line = bufferedReader.readLine();
                    boolean found = false;
                    for(Parameters p: solverParams) {
                        if(line.startsWith(p.name)) {found = true; p.loadParameters(bufferedReader,errors);}}
                    if(!found) errors.append("unknown solver " + line);}}
            if(!errors.toString().isEmpty()) {
                if(life) System.out.println(errors.toString());
                else JOptionPane.showMessageDialog(null,errors.toString(), "Error", JOptionPane.INFORMATION_MESSAGE);}
            input.close();
        }
        catch(Exception e) {
            if(life) System.out.println(e.toString());
            else JOptionPane.showMessageDialog(null, e, "Error", JOptionPane.INFORMATION_MESSAGE);
            loadParameters();}
        if(life) {
            System.out.println("Parameters loaded from " + file.getAbsolutePath());}
        else {
        frame.repaint();
            JOptionPane.showMessageDialog(null, "Parameters loaded from " +
                    file.getAbsolutePath() , "Loaded", JOptionPane.INFORMATION_MESSAGE);}
    }


    private static File chooseFile(File homeDirectory, String ending) {
        JFileChooser chooser = new JFileChooser(homeDirectory);
        chooser.setDialogTitle("Specify a file for the parameters");
        int result = chooser.showSaveDialog(null);
        if(result == JFileChooser.APPROVE_OPTION) {
            File file = chooser.getSelectedFile();
            if(!file.getAbsolutePath().endsWith(ending)) {
                file = new File(file.getAbsolutePath() + ".txt");}
            if (file.exists()) {
                Object[] options = {"Yes", "No"};
                int n = JOptionPane.showOptionDialog(null,
                        "The file exists, overwrite?",
                        "Warning",
                        JOptionPane.YES_NO_CANCEL_OPTION,
                        JOptionPane.QUESTION_MESSAGE,
                        null,
                        options,
                        options[1]);

                if (n == JOptionPane.YES_OPTION) {
                    file.delete();
                    return file;}
                else {if (n == JOptionPane.NO_OPTION) {
                    return chooseFile(homeDirectory,ending);}}}
            return file;}
        return null;}

    private static JPanel createSolverPanel(ArrayList<Parameters> parameters) {
        JPanel solverPane = new JPanel();
        solverPane.setLayout(new BoxLayout(solverPane, BoxLayout.Y_AXIS));
       for(Parameters p : parameters) {
            JLabel solverName = new JLabel(p.name);
           solverName.setFont(new Font("Arial", Font.BOLD, 18));
            solverName.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    super.mouseClicked(e);
                    JOptionPane.showMessageDialog(null, p.description, "Description", JOptionPane.INFORMATION_MESSAGE);}});

            solverPane.add(solverName);
            for(Parameter parameter : p.parameters) {
                JPanel panel = null;
                switch(parameter.displayType) {
                    case String: panel = textField(parameter,p);
                        panel.setMaximumSize(new Dimension(Integer.MAX_VALUE, panel.getMinimumSize().height));
                        solverPane.add(panel);
                        break;
                    case OneOf:  panel = oneOf(parameter);
                        panel.setMaximumSize(new Dimension(Integer.MAX_VALUE, panel.getMinimumSize().height));
                        solverPane.add(panel); break;
                    case Button: panel = makeButton(parameter);
                        panel.setMaximumSize(new Dimension(Integer.MAX_VALUE, panel.getMinimumSize().height));
                        solverPane.add(panel); break;
                    case Boolean: panel = bool(parameter);
                        panel.setMaximumSize(new Dimension(Integer.MAX_VALUE, panel.getMinimumSize().height));
                        solverPane.add(panel); break;
                    case File: panel = filePanel(parameter);
                        panel.setMaximumSize(new Dimension(Integer.MAX_VALUE, panel.getMinimumSize().height));
                        solverPane.add(panel); break;
                    case Directory: panel = directoryPanel(p,parameter);
                        panel.setMaximumSize(new Dimension(Integer.MAX_VALUE, panel.getMinimumSize().height));
                        solverPane.add(panel); break;}
                 }
            solverPane.add(Box.createVerticalStrut(50));
        }
        return solverPane;}

    public static JTabbedPane createGeneratorPanel(ArrayList<Parameters> parameters) {
        JTabbedPane tabpane = new JTabbedPane(JTabbedPane.TOP,JTabbedPane.SCROLL_TAB_LAYOUT );
        for(Parameters params : parameters) {
            JPanel parametersPanel = createParameterPanel(params.description,params);
            JPanel generatorPanel = new JPanel(flowLayout);
            JLabel generateLabel = new JLabel("Generate Clauses");
            generateLabel.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
            generateLabel.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    super.mouseClicked(e);
                    generateClauses(params);}});
            generatorPanel.add(generateLabel);
            parametersPanel.add(generatorPanel);
            tabpane.addTab(params.name, parametersPanel);}
        return tabpane;}


    private static void generateClauses(Parameters params) {
        StringBuilder errors = new StringBuilder();
        ArrayList<InputClauses> clauses = (ArrayList<InputClauses>)params.operation.apply(params,errors);
        if(!errors.toString().isEmpty()) {
            JOptionPane.showMessageDialog(frame, errors.toString(), "Error", JOptionPane.INFORMATION_MESSAGE);
            return;}
        final JDialog dialog = new JDialog();
        dialog.setTitle("Clauses");

        JButton extraButton = new JButton("Save Clauses");
        extraButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                File file = GlobalParameters.homeDirectory.toFile();
                JFileChooser chooser = new JFileChooser(file);
                chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                int result = chooser.showSaveDialog(null);
                if(result == JFileChooser.APPROVE_OPTION) {
                    File directory = chooser.getSelectedFile();
                    StringBuilder files = new StringBuilder();
                    for(InputClauses cls : clauses) {
                        try {files.append(cls.makeCNFFile (directory.toPath(), true).toString()).append("\n");}
                        catch (IOException ex) {
                        ex.printStackTrace();}}
                    JOptionPane.showMessageDialog(null,
                            "Clauses saved into file(s)\n" + files.toString(), "Saved",
                            JOptionPane.INFORMATION_MESSAGE);}
                dialog.setVisible(false);}});

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

        StringBuilder cl = new StringBuilder();
        for(InputClauses clause : clauses) {cl.append(clause.toString());}
        textArea.setText(cl.toString());

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
            JPanel panel;
            switch(parameter.displayType) {
                case String: panel = textField(parameter,parameters);
                    panel.setMaximumSize(new Dimension(Integer.MAX_VALUE, panel.getMinimumSize().height));
                    parametersPanel.add(panel);
                    break;
                case OneOf:  panel = oneOf(parameter);
                    panel.setMaximumSize(new Dimension(Integer.MAX_VALUE, panel.getMinimumSize().height));
                    parametersPanel.add(panel); break;
                case Button: panel = makeButton(parameter);
                    panel.setMaximumSize(new Dimension(Integer.MAX_VALUE, panel.getMinimumSize().height));
                    parametersPanel.add(panel); break;
                case Boolean: panel = bool(parameter);
                    panel.setMaximumSize(new Dimension(Integer.MAX_VALUE, panel.getMinimumSize().height));
                    parametersPanel.add(panel); break;
                case File: panel = filePanel(parameter);
                    panel.setMaximumSize(new Dimension(Integer.MAX_VALUE, panel.getMinimumSize().height));
                    parametersPanel.add(panel); break;
                case Directory: panel = directoryPanel(parameters,parameter);
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
        String defaultValue = parameter.valueString;
        JTextField textField = new JTextField(parameter.valueString,Math.max(15,parameter.valueString.length()));
        parameter.updater = (defaultV) -> textField.setText(defaultV);
        textField.addMouseListener(new MouseAdapter() {
            public void mouseExited(MouseEvent e){
                if(parameter.parser != null) {
                    StringBuilder errors = new StringBuilder();
                    parameter.value = parameter.parser.apply(textField.getText(),errors);
                    if(!errors.isEmpty()) {
                        JOptionPane.showMessageDialog(frame,errors.toString(),"Error", JOptionPane.INFORMATION_MESSAGE);
                        SwingUtilities.invokeLater(() -> textField.setText(defaultValue));
                        parameter.value = oldValue;}
                    parameter.valueString = textField.getText();
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

    private static JPanel makeButton(Parameter parameter) {
        JPanel buttonPanel = new JPanel(flowLayout);
        JRadioButton button = new JRadioButton(parameter.name);
        button.setHorizontalAlignment(SwingConstants.LEFT);
        button.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        button.setSelected((boolean)parameter.value);
        button.addActionListener(e -> {
            parameter.value = button.isSelected(); parameter.valueString = "true";});
        buttonPanel.add(button);
        return buttonPanel;
    }

    private static JPanel oneOf(Parameter parameter) {
        JPanel textPanel = new JPanel(flowLayout);
        textPanel.add(makeLabel(parameter));
        parameter.value = parameter.valueString;
        ButtonGroup group = new ButtonGroup();
        ArrayList<Object[]> buttons = new ArrayList<>();
        for(Parameter param : parameter.parameters.parameters) {
            JRadioButton radioButton = new JRadioButton(param.name);
            radioButton.addActionListener(e -> {parameter.valueString = param.name;parameter.value = param.name;});
            if(parameter.valueString.toLowerCase().equals(param.valueString.toLowerCase())) radioButton.setSelected(true);
            buttons.add(new Object[]{radioButton, param.name});
            group.add(radioButton);
            textPanel.add(radioButton);}
        parameter.updater = (defaultValue) -> {
            for(Object[] button : buttons) {
                if (defaultValue.toLowerCase().equals(((String)(button[1])).toLowerCase())) {
                    SwingUtilities.invokeLater(() ->((JRadioButton)button[0]).setSelected(true));}}};
        return textPanel;}

    private static JPanel filePanel(Parameter parameter) {
        JPanel filePanel = new JPanel(flowLayout);
        filePanel.add(makeLabel(parameter));
        JButton fileButton = new JButton("Load Clauses");
        fileButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                File file = GlobalParameters.homeDirectory.toFile();
                JFileChooser chooser = new JFileChooser(file);
                chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
                chooser.setMultiSelectionEnabled(true);
                int result = chooser.showOpenDialog(null);
                if(result == JFileChooser.APPROVE_OPTION) {
                    ArrayList<File> filesArray = new ArrayList<>();
                    File[] files = chooser.getSelectedFiles();
                    StringBuilder message = new StringBuilder(); message.append("Files Loaded:\n");
                    for(File f : files) {
                        if(f.isFile()) {filesArray.add(f);message.append(f.getAbsolutePath().toString()).append("\n");}
                        else {for(File fl : f.listFiles()) {
                            filesArray.add(fl); message.append(fl.getAbsolutePath().toString()).append("\n");}}}
                    parameter.value = filesArray;
                    JOptionPane.showMessageDialog(frame,message.toString(),"Files Loaded", JOptionPane.INFORMATION_MESSAGE);
                }}});
        filePanel.add(fileButton);
        return filePanel;}

    private static JPanel directoryPanel(Parameters parameters,Parameter parameter) {
        StringBuilder error = new StringBuilder();
        JPanel filePanel = new JPanel(flowLayout);
        filePanel.add(makeLabel(parameter));
        JTextField textField = new JTextField(parameter.valueString,Math.max(15,parameter.valueString.length()));
        JButton fileButton = new JButton("Choose Directory");
        fileButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                File file = ((Path)parameter.value).toFile();
                JFileChooser chooser = new JFileChooser(file);
                chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                int result = chooser.showOpenDialog(null);
                if(result == JFileChooser.APPROVE_OPTION) {
                    file = chooser.getSelectedFile();
                    parameter.valueString = file.getAbsolutePath();
                    parameter.value = file.toPath();
                    textField.setText(parameter.valueString);
                    JOptionPane.showMessageDialog(frame,"Directory " + parameter.valueString + " chosen",
                            "Directory", JOptionPane.INFORMATION_MESSAGE);
                }}});
        filePanel.add(fileButton);

        parameter.updater = (defaultV) -> textField.setText(defaultV);
        textField.addMouseListener(new MouseAdapter() {
            public void mouseExited(MouseEvent e){
                String pathString = textField.getText();
                if(pathString.isEmpty()) return;
                File directory = new File(pathString);
                if(!directory.isDirectory() || !directory.exists()) {
                    JOptionPane.showMessageDialog(frame,"'"+pathString+"' is no such directory",
                            "Error", JOptionPane.INFORMATION_MESSAGE);}
                if(!error.isEmpty()) {
                    JOptionPane.showMessageDialog(frame,error.toString(),
                            "Error", JOptionPane.INFORMATION_MESSAGE);}
                parameter.valueString = pathString;
                parameter.value=directory.toPath();}});
        filePanel.add(textField);
        return filePanel;}

    private static JPanel bool(Parameter parameter) {
        JPanel textPanel = new JPanel(flowLayout);
        textPanel.add(makeLabel(parameter));
        boolean selected = (Boolean)parameter.value;
        ButtonGroup group = new ButtonGroup();
        JRadioButton trueButton = new JRadioButton("true",selected);
        JRadioButton falseButton = new JRadioButton("false",!selected);
        trueButton.addActionListener(e -> {parameter.value = true; parameter.valueString = "true";});
        falseButton.addActionListener(e -> {parameter.value = false;parameter.valueString ="false";});
        parameter.updater = (value) -> {if(value.equals("true")) trueButton.setSelected(true); else falseButton.setSelected(true);};
        group.add(trueButton);

        textPanel.add(trueButton);
        group.add(falseButton);
        textPanel.add(falseButton);
        return textPanel;}

    private static JLabel makeLabel(Parameter parameter) {
        JLabel label = new JLabel(parameter.name);
        label.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        if(parameter.description != null) {
            label.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    JOptionPane.showMessageDialog(null, parameter.description, "Description",
                            JOptionPane.INFORMATION_MESSAGE);};});}
        return label;}

    private static ArrayList<String[]> recentProjects = new ArrayList<>();

    public static void loadProjects(StringBuilder errors) {
        Path quSatPath =  Paths.get("").toAbsolutePath();

        File quSatFile = Paths.get(quSatPath.toString(),"QuSatProjects.txt").toFile();
        System.out.println(quSatFile);
        if(!quSatFile.exists()) {return;}
        try {
            BufferedReader reader = new BufferedReader(new FileReader(quSatFile));
            String line;
            while ((line = reader.readLine()) != null) {
                String[] project = line.split("\\s*,\\s*");
                recentProjects.add(project);}
            reader.close();} catch (IOException e) {errors.append(e);}
    }

    public static void saveProjects(String name, File file, StringBuilder errors) {
       String filename = file.getAbsolutePath().toString();
        for(int i = 0; i < recentProjects.size(); i++) {
            if(recentProjects.get(i)[1].equals(filename)) {
                recentProjects.remove(i); break;}}
        if(recentProjects.size() > 15) {
            recentProjects.remove(0);}
        recentProjects.add(new String[]{name,filename});

        Path quSatPath = Paths.get("").toAbsolutePath();

        File quSatFile = Paths.get(quSatPath.toString(), "QuSatProjects.txt").toFile();
        System.out.println(quSatFile);
        if(quSatFile.exists()) {quSatFile.delete();}

        try (PrintWriter writer = new PrintWriter(quSatFile)) {
            for (String[] project : recentProjects) {
                writer.println(project[0] + "," + project[1]);
            }
        } catch (FileNotFoundException e) {
            errors.append(e);
        }
    }

    public static void main(String[] args)  {
        Locale.setDefault(Locale.US);
        File file = new File(System.getProperty("user.home")+"\\QuSat\\TestDir\\Test.txt");
        loadParameters(file,true);
        runQuSatSolver();
    }


}
