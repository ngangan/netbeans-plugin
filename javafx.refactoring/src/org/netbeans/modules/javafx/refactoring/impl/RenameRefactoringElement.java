 /*
 *  DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 *  Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
 * 
 *  The contents of this file are subject to the terms of either the GNU
 *  General Public License Version 2 only ("GPL") or the Common
 *  Development and Distribution License("CDDL") (collectively, the
 *  "License"). You may not use this file except in compliance with the
 *  License. You can obtain a copy of the License at
 *  http://www.netbeans.org/cddl-gplv2.html
 *  or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 *  specific language governing permissions and limitations under the
 *  License.  When distributing the software, include this License Header
 *  Notice in each file and include the License file at
 *  nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 *  particular file as subject to the "Classpath" exception as provided
 *  by Sun in the GPL Version 2 section of the License file that
 *  accompanied this code. If applicable, add the following below the
 *  License Header, with the fields enclosed by brackets [] replaced by
 *  your own identifying information:
 *  "Portions Copyrighted [year] [name of copyright owner]"
 * 
 *  Contributor(s):
 * 
 *  Portions Copyrighted 1997-2009 Sun Microsystems, Inc.
 */

package org.netbeans.modules.javafx.refactoring.impl;

import java.io.IOException;
import java.lang.ref.SoftReference;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Position.Bias;
import javax.swing.text.StyledDocument;
import org.netbeans.editor.GuardedDocument;
import org.netbeans.editor.Utilities;
import org.netbeans.modules.refactoring.spi.SimpleRefactoringElementImplementation;
import org.openide.cookies.EditorCookie;
import org.openide.cookies.LineCookie;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObject;
import org.openide.text.DataEditorSupport;
import org.openide.text.Line;
import org.openide.text.NbDocument;
import org.openide.text.PositionBounds;
import org.openide.util.Lookup;

/**
 *
 * @author Jaroslav Bachorik
 */
public class RenameRefactoringElement extends SimpleRefactoringElementImplementation {
    final private static Logger LOGGER = Logger.getLogger(RenameRefactoringElement.class.getName());
    final private static boolean DEBUG = LOGGER.isLoggable(Level.FINEST);
    
    private Lookup context;
    private String oldText;
    private int startPosition = -1;
    private String newName;
    private ElementLocation location;
    private DataEditorSupport des;
    private GuardedDocument doc;
    private LineCookie lc;

    private SoftReference<String> newContent = null;

    final public static RenameRefactoringElement create(ElementLocation location, String newName, String oldName, Lookup context) {
        try {
            return new RenameRefactoringElement(location, newName, oldName, context);
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, null, e);
        }
        return null;
    }

    final public static RenameRefactoringElement create(ElementLocation location, String newName, Lookup context) {
        try {
            return new RenameRefactoringElement(location, newName, location.getElement().getSimpleName().toString(), context);
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, null, e);
        }
        return null;
    }

    private RenameRefactoringElement(ElementLocation location, String newName, String oldName, Lookup context) throws IOException {
        this.location = location;
        this.newName = newName;
        this.oldText = oldName;
        this.context = context;
        init();
    }

    public String getDisplayText() {
        try {
            StringBuilder origLine = new StringBuilder();
            int delta = extractLine(startPosition, origLine);

            StringBuilder newLine = new StringBuilder(origLine);
            newLine.replace(delta, delta + oldText.length(), newName);

            return processDiff(newLine.toString(), origLine.toString());
        } catch (Exception e) {
            e.printStackTrace();
            return "Renaming";
        }
    }

    @Override
    protected String getNewFileContent() {
        if (newContent != null) {
            String content = newContent.get();
            if (content != null) {
                return content;
            }
        }
        StringBuilder content = null;
        try {
            content = new StringBuilder(doc.getText(0, doc.getLength()));
            content.replace(startPosition, startPosition + oldText.length(), newName);
            newContent = new SoftReference<String>(content.toString());
        } catch (BadLocationException badLocationException) {
            return null;
        }
        return content.toString();
    }

    public Lookup getLookup() {
        return context;
    }

    public FileObject getParentFile() {
        return location.getSourceFile();
    }

    public PositionBounds getPosition() {
        return new PositionBounds(des.createPositionRef(startPosition, Bias.Forward), des.createPositionRef(startPosition + oldText.length(), Bias.Forward));
    }

    public String getText() {
        return "Rename element";
    }

    public void performChange() {
        final Document doc = des.getDocument();
        if (doc instanceof GuardedDocument) {
            ((GuardedDocument)doc).runAtomic(new Runnable() {

                public void run() {
                    try {
                        synchronized(doc) {
                            TransformationContext tc = context.lookup(TransformationContext.class);
                            int offset = tc.getRealOffset(startPosition);
                            String realText = doc.getText(offset, oldText.length());
                            if (realText.equals(oldText)) {
                                doc.remove(offset, oldText.length());
                                doc.insertString(offset, newName, null);
                                tc.replaceText(startPosition, oldText.length(), newName.length());
                            } else {
                                if (DEBUG) {
                                    StringBuilder sb = new StringBuilder();
                                    extractLine(offset, sb);
                                    LOGGER.finest("Can not rename due to name mismatch: " + processDiff(oldText, realText)); // NOI18N
                                }
                            }
                        }
                    } catch (BadLocationException e) {
                        e.printStackTrace();
                    }
                }
            });
        }
    }

    private String processDiff(String newText, String oldText) {
        StringBuilder sb = new StringBuilder();

        int newLength = newText.length();
        int oldLength = oldText.length();

        char[] newChars = new char[newLength];
        char[] oldChars = new char[oldLength];

        newText.getChars(0, newLength, newChars, 0);
        oldText.getChars(0, oldLength, oldChars, 0);

        // opt[i][j] = length of LCS of oldChars[i..oldLength] and newChars[j..newLength]
        int[][] opt = new int[oldLength+1][newLength+1];

        // compute length of LCS and all subproblems via dynamic programming
        for (int i = oldLength-1; i >= 0; i--) {
            for (int j = newLength-1; j >= 0; j--) {
                if (oldChars[i] == newChars[j])
                    opt[i][j] = opt[i+1][j+1] + 1;
                else
                    opt[i][j] = Math.max(opt[i+1][j], opt[i][j+1]);
            }
        }

        // recover LCS itself and print out non-matching lines to standard output
        int i = 0, j = 0;
        String closingTag = "";
        while(i < oldLength && j < newLength) {
            if (oldChars[i] == newChars[j]) {
                sb.append(closingTag);
                closingTag = "";
                sb.append(oldChars[i]);
                i++;
                j++;
            } else {
                int oldLCS = (i+1 == oldLength) ? Integer.MIN_VALUE : opt[i+1][j];
                int newLCS = (j+1 == newLength) ? Integer.MIN_VALUE : opt[i][j+1];
                if (oldLCS >= newLCS)  {
                    if (!closingTag.equals("</b>]")) {
                        sb.append(closingTag);
                        sb.append("[<b>");
                        closingTag = "</b>]";
                    }
                    sb.append(oldChars[i++]);
                } else {
                    if (!closingTag.equals("</b>")) {
                        sb.append(closingTag);
                        sb.append("<b>");
                        closingTag = "</b>";
                    }
                    sb.append(newChars[j++]);
                }
            }
        }
        sb.append(closingTag);
        return sb.toString();
    }

    private int extractLine(int offset, StringBuilder sb) throws BadLocationException {
        int lineNo = Utilities.getLineOffset(doc, offset);
        Line l = lc.getLineSet().getCurrent(lineNo);
        sb.append(l.getText().trim());
        int lineOff = NbDocument.findLineOffset((StyledDocument)doc, lineNo);

        lineOff = Utilities.getFirstNonWhiteFwd(doc, lineOff);
        return offset - lineOff;
    }

    private void init() throws IOException {
        DataObject dobj = DataObject.find(location.getSourceFile());
        des = (DataEditorSupport)dobj.getCookie(EditorCookie.class);
        doc = (GuardedDocument)des.openDocument();
        lc = dobj.getCookie(LineCookie.class);

        try {
        startPosition = location.getStartPosition();
        int origLine = Utilities.getLineOffset(doc, startPosition);
        if (oldText.contains(".")) { // an FQN; probably package name
            int pos = 0;
            StringTokenizer st = new StringTokenizer(oldText, ".");
            while (st.hasMoreTokens()) {
                String part = st.nextToken();
                if (part.equals(location.getSimpleName())) {
                    startPosition -= pos;
                    int newLine = Utilities.getLineOffset(doc, startPosition);
                    if (origLine != newLine) {
                        startPosition++; // compensate the error introduced somehwere ...
                    }
                    break;
                }
                pos += part.length() + 1;
            }
        }
        } catch (BadLocationException e) {
            IOException ioe = (IOException)new IOException().initCause(e);
            throw ioe;
        }
    }
}
