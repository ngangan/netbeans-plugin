/*
 * Copyright (c) 2008, Your Corporation. All Rights Reserved.
 */

package org.netbeans.lib.javafx.lexer;

import org.netbeans.api.javafx.lexer.JFXTokenId;

import java.io.*;
import java.util.HashMap;

/**
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.orgm">RKo</a>)
 * This class is for verification of grammar file vs. {@link org.netbeans.api.javafx.lexer.JFXTokenId} enum.
 *
 * @todo Change to implement Ant task in future.
 */
@SuppressWarnings({"ALL"})
public class GrammarVerifier {
    public static void main(String[] args) {
        if (args == null || args.length == 0) {
            System.exit(-2);                
        }

        final String s = args[0];
        File f = new File(s);
        if (!f.exists()) {
            System.exit(-3);
        }

        try {
            BufferedReader r = new BufferedReader(new FileReader(f));
            HashMap<String, Integer> map = new HashMap<String, Integer>();
            String line;
            while ((line = r.readLine()) != null) {
                if (line.startsWith("\'")) continue;
                String[] elements = line.split("=");
                map.put(elements[0].trim(), Integer.parseInt(elements[1].trim()));
            }

            final JFXTokenId[] tokenIds = JFXTokenId.values();
            for (JFXTokenId id : tokenIds) {
                if (map.containsKey(id.name())) {
                    final Integer integer = map.get(id.name());
                    if (id.getTokenType() != integer) {
                        System.err.println("The token " + id + " has not correct id.");
                        System.exit(-100);
                    }
                    map.remove(id.name());
                } else if (id != JFXTokenId.UNKNOWN) {
                    System.err.println("The token " + id + " has not been found.");
                    System.exit(-100);
                } 
            }
            if (!map.isEmpty()) {
                System.err.printf("There are tokens missing in %s enum. \n", JFXTokenId.class.getName());
                System.exit(-100);
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
