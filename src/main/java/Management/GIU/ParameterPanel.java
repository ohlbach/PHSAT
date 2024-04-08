package Management.GIU;

import Management.Parameter;
import Management.Parameters;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class ParameterPanel {

    public static JPanel createPanel(Parameters parameters) {
        JPanel panel = new JPanel(new BorderLayout());
        JLabel title = new JLabel(parameters.title);
        title.setFont(title.getFont().deriveFont(Font.BOLD));
        title.setForeground(Color.blue);
        panel.add(title, BorderLayout.NORTH);
        JPanel parametersPanel = new JPanel(new GridLayout(parameters.parameters.size(), 1));
        for (Parameter parameter : parameters.parameters) {
            parametersPanel.add(textField(parameter), BorderLayout.CENTER);
        }
        panel.add(parametersPanel, BorderLayout.CENTER);
        return panel;
    }

    private static JPanel textField(Parameter parameter) {
        JPanel textPanel = new JPanel();
        JLabel label = new JLabel(parameter.name);
        if(parameter.description != null) {
            label.addMouseListener(new MouseAdapter() {
                JOptionPane op;
                JDialog dialog;
                                       @Override
                public void mouseEntered(MouseEvent e) {
                    op = new JOptionPane( parameter.description,JOptionPane.INFORMATION_MESSAGE, JOptionPane.OK_CANCEL_OPTION );
                    op.setOptionType(JOptionPane.DEFAULT_OPTION);
                    dialog=op.createDialog(parameter.name);
                    dialog.setVisible(true);}
                public void mouseExited(MouseEvent e) {
                                           op.setVisible(false);
                                           dialog.dispose();}});}
        textPanel.add(label);
        parameter.value = parameter.defaultName;
        JTextField textField = new JTextField(parameter.defaultName,15);
        textField.addMouseListener(new MouseAdapter() {
            public void mouseExited(MouseEvent e){
                parameter.value = textField.getText();}});
        textPanel.add(textField);
        return textPanel;
    }
}
