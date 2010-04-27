/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.fxd.schemamodel;

import java.util.HashMap;
import java.util.Map;

/**
 * Provides utility methods for FXD Schema
 * @author Andrey Korostelev
 */
public class FXDSchemaHelper {

    private static final String [] DEFAULT_IMPORTS = {
        "javafx.scene.image.",
        "javafx.scene.transform.",
        "javafx.geometry.",
        "javafx.scene.",
        "javafx.scene.paint.",
        "javafx.scene.effect.light.",
        "javafx.scene.shape.",
        "javafx.scene.text.",
        "javafx.scene.effect.",
        "javafx.fxd.",
        "javafx.animation.",
        "javafx.animation.transition.",
        "javafx.scene.control.",
        "javafx.scene.layout.",
        "",
        //because of the class com.sun.javafx.tools.fxd.FXD
        "com.sun.javafx.tools.fxd."
    };

    private static Map<String, String> FXD_ELEMENT_TO_SCHEMA_ID = new HashMap<String, String>();


    public static String[] getDefaultImports(){
        return DEFAULT_IMPORTS;
    }

    /**
     * returns fxd element or enumeration id from fxd schema associated with
     * given node id from document model.
     * @param docNodeId document model node id
     * @return fxd schema element or enumeration id
     */
    public static String getFXDSchemaId(String fxdElement) {
        String schemaId = FXD_ELEMENT_TO_SCHEMA_ID.get(fxdElement);
        if ( schemaId == null) {
            schemaId = getFXDSchemaIdImpl(fxdElement);
            FXD_ELEMENT_TO_SCHEMA_ID.put(fxdElement, schemaId);
        }
        return schemaId;
    }

    private static String getFXDSchemaIdImpl(String fxdElement) {
        for ( String importStr : getDefaultImports()) {
            String className = importStr.concat(fxdElement);
            try {
                Class clazz = Class.forName(className);
                return clazz.getCanonicalName().replace('.', '-'); // NOI18N
            } catch (ClassNotFoundException ex) {
            }
        }
        return null;
    }


}
