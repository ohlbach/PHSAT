package Generators;

import org.junit.Test;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 06.09.2018.
 */
public class CNFReaderTest {


    @Test
    public void parseProblemParameters1() throws Exception {
        System.out.println("parseProblemParameters 1");
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        HashMap<String,String> parameters = new HashMap<>();
        parameters.put("file", "C:\\Users\\ohlbach\\Share\\LMU\\PMSat\\Ex\\2bitcomp_5.cnf, C:\\Users\\ohlbach\\Share\\LMU\\PMSat\\Ex\\example.cnf,");
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseProblemParameters(parameters,errors,warnings);
        System.out.println(errors.toString());
        System.out.println(pars);
        System.out.println(System.getenv("TEMP"));
    }

    @Test
    public void help() throws Exception {

    }

    @Test
    public void generate() throws Exception {

    }

}