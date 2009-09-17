/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.api.javafx.editor;

import java.io.ByteArrayInputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import org.openide.filesystems.FileUtil;

/**
 * Copied from java editor tests
 *
 * @author Anton Chechel
 */
public final class TestUtilities {

    /**
     * Copies a string to a specified file.
     *
     * @param f the file to use.
     * @param content the contents of the returned file.
     * @return the created file
     */
    public final static File copyStringToFile(File f, String content) throws Exception {
        FileOutputStream os = new FileOutputStream(f);
        InputStream is = new ByteArrayInputStream(content.getBytes("UTF-8"));
        FileUtil.copy(is, os);
        os.close();
        is.close();

        return f;
    }

    /**
     * Returns a string which contains the contents of a file.
     *
     * @param f the file to be read
     * @return the contents of the file(s).
     */
    public final static String copyFileToString(java.io.File f) throws java.io.IOException {
        int s = (int) f.length();
        byte[] data = new byte[s];
        int len = new FileInputStream(f).read(data);
        if (len != s) {
            throw new EOFException("truncated file");
        }
        return new String(data);
    }
}
