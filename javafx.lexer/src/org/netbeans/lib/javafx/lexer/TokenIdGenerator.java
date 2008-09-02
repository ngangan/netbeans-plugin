/*
 * Copyright (c) 2008, Your Corporation. All Rights Reserved.
 */

package org.netbeans.lib.javafx.lexer;

import org.netbeans.api.javafx.lexer.JFXTokenId;

import java.io.*;
import java.util.Arrays;
import java.util.Map;
import java.util.HashMap;
import java.text.MessageFormat;

/**
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.orgm">RKo</a>)
 * @todo documentation
 */
public class TokenIdGenerator {
    public static final String FORMAT = "\t{0}(\"{1}\", {2}),";


    public static void main(String[] args) {
        if (args == null || args.length == 0) {
            System.exit(-2);
        }

        String tokens = get("-tokens", args);
        File tokenF = new File(tokens);
        if (tokens == null || !tokenF.exists()) {
            System.out.println("Use -tokens to specify token input file.");
            System.exit(-1);
        }

        BufferedReader r = null;
        JFXTokenId[] tokenIds = JFXTokenId.values();
        Map<String, String> nameToCat = new HashMap<String, String>(tokenIds.length);
        for (JFXTokenId tokenId : tokenIds) {
            nameToCat.put(tokenId.name(), tokenId.primaryCategory());
        }
        try {
            r = new BufferedReader(new FileReader(tokenF));
            MessageFormat f = new MessageFormat(FORMAT);
            String line;            
            while ((line = r.readLine()) != null) {
                if (line.startsWith("\'")) continue;
                String[] elements = line.split("=");
                String name = elements[0];
                String category;
                if (nameToCat.containsKey(name)) {
                    category = nameToCat.get(name);
                } else {
                    category = JFXTokenId.UNKNOWN.primaryCategory();
                }
                System.out.println(f.format(new Object[]{name, category, elements[1]}));
            }
            System.exit(0);
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(-2);
        } finally {
            if (r != null) try {
                r.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }


    private static String get(String param, String[] args) {
        int index = Arrays.binarySearch(args, param);
        if (index < 0) return null;
        if (index == args.length - 1) return null;
        return args[index + 1];
    }

}
