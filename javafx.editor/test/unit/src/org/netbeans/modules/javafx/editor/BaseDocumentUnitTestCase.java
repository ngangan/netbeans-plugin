/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 * Oracle and Java are registered trademarks of Oracle and/or its affiliates.
 * Other names may be trademarks of their respective owners.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common
 * Development and Distribution License("CDDL") (collectively, the
 * "License"). You may not use this file except in compliance with the
 * License. You can obtain a copy of the License at
 * http://www.netbeans.org/cddl-gplv2.html
 * or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 * specific language governing permissions and limitations under the
 * License.  When distributing the software, include this License Header
 * Notice in each file and include the License file at
 * nbbuild/licenses/CDDL-GPL-2-CP.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * Contributor(s):
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 *
 * If you wish your version of this file to be governed by only the CDDL
 * or only the GPL Version 2, indicate your decision by adding
 * "[Contributor] elects to include this software in this distribution
 * under the [CDDL or GPL Version 2] license." If you do not indicate a
 * single choice of license, a recipient has the option to distribute
 * your version of this file under either the CDDL, the GPL Version 2 or
 * to extend the choice of license to its licensees as provided above.
 * However, if you add GPL Version 2 code and therefore, elected the GPL
 * Version 2 license, then the option applies only if the new code is
 * made subject to such option by the copyright holder.
 */

package org.netbeans.modules.javafx.editor;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.EventListenerList;
import javax.swing.text.BadLocationException;
import javax.swing.text.Caret;
import javax.swing.text.Document;
import javax.swing.text.EditorKit;
import org.netbeans.editor.BaseDocument;
import org.netbeans.editor.BaseKit;
import org.netbeans.junit.NbTestCase;

/**
 * Support for creation of unit tests working with document.
 *
 * @author Miloslav Metelka
 */
public class BaseDocumentUnitTestCase extends NbTestCase {
    
    private EditorKit editorKit;
    
    private String loadDocumentText;
    
    private BaseDocument doc;
    
    private Caret caret;
    
    private int loadCaretOffset = -1;
    
    
    public BaseDocumentUnitTestCase(String testMethodName) {
        super(testMethodName);
        
    }
    
    /**
     * Set the text that the document obtained by {@link #getDocument()}
     * would be loaded with.
     *
     * <p>
     * The text is parsed and the first occurrence of "|" sets
     * the caret offset which is available by {@link #getLoadCaretOffset()}.
     * <br>
     * The "|" itself is removed from the document text and subsequent
     * calls to {@link #getLoadDocumentText()} do not contain it.
     */
    protected void setLoadDocumentText(String loadDocumentText) {
        // [TODO] a more elaborated support could be done e.g. "||" to a real "|" etc.
        loadCaretOffset = loadDocumentText.indexOf('|');
        if (loadCaretOffset != -1) {
            loadDocumentText = loadDocumentText.substring(0, loadCaretOffset)
                + loadDocumentText.substring(loadCaretOffset + 1);
        }
        
        this.loadDocumentText = loadDocumentText;
    }
    
    /**
     * Get the text that the document obtained by {@link #getDocument()}
     * would be loaded with.
     *
     * @return text to be loaded into the document or null if nothing
     *  should be loaded.
     */
    protected final String getLoadDocumentText() {
        return loadDocumentText;
    }
    
    /**
     * Return caret offset based on the scanning of the text passed
     * to {@link #setLoadDocumentText(String)}.
     *
     * @return valid caret offset or -1 if no caret offset was determined
     *  in the document text.
     */
    protected final int getLoadCaretOffset() {
        return loadCaretOffset;
    }
    
    /**
     * Return the caret instance that can be used for testing.
     * <br>
     * The caret listens on document changes and its initial
     * position is set to {@link #getLoadCaretOffset()}.
     *
     * @return caret instance.
     */
    protected synchronized final Caret getCaret () {
        if (caret == null) {
            caret = new CaretImpl(getDocument(), loadCaretOffset);
        }
        return caret;
    }
    
    /**
     * Get the offset of where the caret resides in the document.
     */
    protected final int getCaretOffset() {
        return getCaret().getDot();
    }
    
    /**
     * Get the document that the test should work with.
     * <br>
     * If the document does not exist yet it will be created
     * and loaded with the text from {@link #getLoadDocumentText()}.
     */
    protected synchronized BaseDocument getDocument() {
        if (doc == null) {
            doc = createAndLoadDocument();
        }
        return doc;
    }
    
    /**
     * Return text of the whole document.
     * <br>
     * The document is retrieved by {@link #getDocument()}.
     */
    protected String getDocumentText() {
        try {
            Document d = getDocument();
            return d.getText(0, d.getLength());
        } catch (BadLocationException ex) {
            ex.printStackTrace(getLog());
            fail(ex.getMessage());
            return null; // should never be reached
        }
    }
    
    /**
     * Assert whether the document available through {@link #getDocument()}
     * has a content equal to <code>expectedText</code>.
     */
    protected void assertDocumentText(String msg, String expectedText) {
        String docText = getDocumentText();
        if (!docText.equals(expectedText)) {
            StringBuffer sb = new StringBuffer();
            sb.append(msg);
            sb.append("\n----- expected text: -----\n");
            appendDebugText(sb, expectedText);
            sb.append("\n----- document text: -----\n");
            appendDebugText(sb, docText);
            sb.append("\n-----\n");

            fail(sb.toString());
        }
    }
    
    protected final void appendDebugChar(StringBuffer sb, char ch) {
        switch (ch) {
            case '\n':
                sb.append("\\n\n");
                break;
            case '\t':
                sb.append("\\t");
                break;

            default:
                sb.append(ch);
                break;
        }
    }
    
    protected final void appendDebugText(StringBuffer sb, String text) {
        for (int i = 0; i < text.length(); i++) {
            appendDebugChar(sb, text.charAt(i));
        }
    }
    
    protected final String debugText(String text) {
        StringBuffer sb = new StringBuffer();
        appendDebugText(sb, text);
        return sb.toString();
    }

    /**
     * Assert whether the document available through {@link #getDocument()}
     * has a content equal to <code>expectedText</code> and whether the caret
     * position {@link #getCaretOffset()}
     * indicated by "|" in the passed text is at the right place.
     */
    protected void assertDocumentTextAndCaret(String msg, String expectedTextAndCaretPipe) {
        // [TODO] a more elaborated support could be done e.g. "||" to a real "|" etc.
        int expectedCaretOffset = expectedTextAndCaretPipe.indexOf('|');
        if (expectedCaretOffset == -1) { // caret position not indicated
            fail("Caret position not indicated in '" + expectedTextAndCaretPipe + "'");
        }

        String expectedDocumentText= expectedTextAndCaretPipe.substring(0, expectedCaretOffset)
            + expectedTextAndCaretPipe.substring(expectedCaretOffset + 1);

        assertDocumentText(msg, expectedDocumentText);
        if (expectedCaretOffset != getCaretOffset()) {
            fail("caretOffset=" + getCaretOffset()
                + " but expectedCaretOffset=" + expectedCaretOffset
                + " in '" + expectedTextAndCaretPipe + "'"
            );
        }
    }

    /**
     * Create editor kit instance to be returned
     * by {@link #getEditorKit()}.
     * <br>
     * The returned editor kit should return
     * <code>BaseDocument</code> instances
     * from its {@link javax.swing.text.EditorKit.createDefaultDocument()}.
     */
    @SuppressWarnings("deprecation")
    protected EditorKit createEditorKit() {
        return BaseKit.getKit(BaseKit.class);
    }
    
    /**
     * Get the kit that should be used
     * when creating the <code>BaseDocument</code>
     * instance.
     * <br>
     * The editor kit instance is created in {@link #createEditorKit()}.
     *
     * @return editor kit instance.
     */
    public final EditorKit getEditorKit() {
        if (editorKit == null) {
            editorKit = createEditorKit();
        }
        return editorKit;
    }
    
    private BaseDocument createAndLoadDocument() {
        BaseDocument bd = (BaseDocument)getEditorKit().createDefaultDocument();

        if (loadDocumentText != null) {
            try {
                bd.insertString(0, loadDocumentText, null);
            } catch (BadLocationException e) {
                e.printStackTrace(getLog());
                fail();
            }
        }
        return bd;
    }

    class CaretImpl implements Caret, DocumentListener {
        
        private Document doc;
        
        private int dot;
        
        private int mark;
        
        private boolean visible = true;
        
        private boolean selectionVisible;
        
        private int blinkRate = 300;
        
        private EventListenerList listenerList = new EventListenerList();
        
        private ChangeEvent changeEvent;
        
        CaretImpl(Document doc, int dot) {
            this.doc = doc;
            doc.addDocumentListener(this);
            setDot(dot);
        }

        public void deinstall (javax.swing.text.JTextComponent c) {
            fail("Not yet implemented");
        }
        
        public void install (javax.swing.text.JTextComponent c) {
            fail("Not yet implemented");
        }
        
        public java.awt.Point getMagicCaretPosition () {
            fail("Not yet implemented");
            return null;
        }
        
        public void setMagicCaretPosition (java.awt.Point p) {
            fail("Not yet implemented");
        }
        
        public int getDot () {
            return dot;
        }
        
        public int getMark () {
            return mark;
        }
        
       public void setDot (int dot) {
            this.mark = this.dot;
            changeCaretPosition(dot);
        }
        
        public void moveDot (int dot) {
            changeCaretPosition(dot);
        }
        
        public int getBlinkRate () {
            return blinkRate;
        }
        
        public void setBlinkRate (int rate) {
            this.blinkRate = blinkRate;
        }
        
        public boolean isVisible () {
            return visible;
        }
        
        public void setVisible (boolean v) {
            this.visible = visible;
        }
        
        public boolean isSelectionVisible () {
            return selectionVisible;
        }
        
        public void setSelectionVisible (boolean v) {
            this.selectionVisible = v;
        }
        
        public void addChangeListener (ChangeListener l) {
            listenerList.add(ChangeListener.class, l);
        }
        
        public void removeChangeListener (ChangeListener l) {
            listenerList.remove(ChangeListener.class, l);
        }
        
        public void fireChangeListener() {
            // Lazily create the event
            if (changeEvent == null) {
                changeEvent = new ChangeEvent(this);
            }

            Object[] listeners = listenerList.getListenerList();
            for (int i = 0; i < listeners.length; i++) {
                ((ChangeListener)listeners[i + 1]).stateChanged(changeEvent);
            }
        }
        
        public void paint (java.awt.Graphics g) {
        }
        
        public void insertUpdate(DocumentEvent e) {
            int offset = e.getOffset();
            int length = e.getLength();
            int newDot = dot;
            short changed = 0;
            if (newDot >= offset) {
                newDot += length;
                changed |= 1;
            }
            int newMark = mark;
            if (newMark >= offset) {
                newMark += length;
                changed |= 2;
            }

            if (changed != 0) {
                if (newMark == newDot) {
                    setDot(newDot);
                    ensureValidPosition();
                } else {
                    setDot(newMark);
                    if (getDot() == newMark) {
                        moveDot(newDot);
                    }
                    ensureValidPosition();
                }

            }
        }
        
        public void removeUpdate(DocumentEvent e) {
            int offs0 = e.getOffset();
            int offs1 = offs0 + e.getLength();
            int newDot = dot;
            if (newDot >= offs1) {
                newDot -= (offs1 - offs0);
            } else if (newDot >= offs0) {
                newDot = offs0;
            }
            int newMark = mark;
            if (newMark >= offs1) {
                newMark -= (offs1 - offs0);
            } else if (newMark >= offs0) {
                newMark = offs0;
            }
            if (newMark == newDot) {
                setDot(newDot);
                ensureValidPosition();
            } else {
                setDot(newMark);
                if (getDot() == newMark) {
                    moveDot(newDot);
                }
                ensureValidPosition();
            }
        }
        
        public void changedUpdate(DocumentEvent e) {
            
        }

        private void changeCaretPosition(int dot) {
            if (this.dot != dot) {
                this.dot = dot;
                fireChangeListener();
            }
        }
        
       private void ensureValidPosition() {
            int length = doc.getLength();
            if (dot > length || mark > length) {
                setDot(length);
            }
        }

    }
    
}
