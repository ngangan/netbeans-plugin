/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
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
 * nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Sun in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * Contributor(s):
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2007 Sun
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

package org.netbeans.modules.javafx.debugger;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.netbeans.api.debugger.jpda.LineBreakpoint;

import org.netbeans.modules.javafx.debugger.breakpoints.JavaFXLineBreakpoint;
import org.openide.cookies.LineCookie;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.URLMapper;
import org.openide.loaders.DataObject;
import org.openide.loaders.DataObjectNotFoundException;
import org.openide.text.Line;
import org.openide.util.WeakListeners;

/**
 * Translation utility for handling of lines that are shifted during source modifications.
 * 
 * @author Martin Entlicher
 */
class LineTranslations {
    
    private static LineTranslations translations;

    private ChangeListener          changedFilesListener;
    private Map<Object, Registry>   timeStampToRegistry = new WeakHashMap<Object, Registry>();
    private Map<JavaFXLineBreakpoint, BreakpointLineUpdater> lineUpdaters = new HashMap<JavaFXLineBreakpoint, BreakpointLineUpdater>();
    private Map<Object, Map<LineBreakpoint, Integer>> originalBreakpointLines = new WeakHashMap<Object, Map<LineBreakpoint, Integer>>();
    private Map<Object, PropertyChangeListener> breakpointListeners = new WeakHashMap<Object, PropertyChangeListener>();
    
    private LineTranslations() {
    }
    
    static synchronized LineTranslations getTranslations() {
        if (translations == null) {
            translations = new LineTranslations();
        }
        return translations;
    }

    /**
     * Creates a new time stamp.
     *
     * @param timeStamp a new time stamp
     */
    void createTimeStamp (Object timeStamp) {
        Set<DataObject> modifiedDataObjects = DataObject.getRegistry().getModifiedSet();
        Registry r = new Registry ();
        synchronized (this) {
            timeStampToRegistry.put (timeStamp, r);
            for (DataObject dobj : modifiedDataObjects) {
                r.register (dobj);
            }

            if (changedFilesListener == null) {
                changedFilesListener = new ChangedFilesListener ();
                DataObject.getRegistry ().addChangeListener (changedFilesListener);
            }
        }
    }

    /**
     * Disposes given time stamp.
     *
     * @param timeStamp a time stamp to be disposed
     */
    synchronized void disposeTimeStamp (Object timeStamp) {
        timeStampToRegistry.remove (timeStamp);
        if (timeStampToRegistry.isEmpty ()) {
            DataObject.getRegistry ().removeChangeListener (changedFilesListener);
            changedFilesListener = null;
        }
        originalBreakpointLines.remove(timeStamp);
        breakpointListeners.remove(timeStamp);
    }
    
    /**
     * Returns the original line number of a breakpoint.
     *
     * @param url The URL
     * @param currentLineNumber The current line number
     * @param timeStamp a time stamp to be used
     *
     * @return The original line number
     */
    int getOriginalLineNumber (
        final LineBreakpoint lb,
        final Object timeStamp
    ) {
        Map<LineBreakpoint, Integer> bpLines;
        PropertyChangeListener lineNumberListener;
        synchronized (this) {
            bpLines = originalBreakpointLines.get(timeStamp);
            if (bpLines != null) {
                Integer line = bpLines.get(lb);
                if (line != null) {
                    //System.err.println("Original line of "+lb+" IS "+line);
                    return line.intValue();
                }
            } else {
                bpLines = new WeakHashMap<LineBreakpoint, Integer>();
                originalBreakpointLines.put(timeStamp, bpLines);
            }
        }
        int line = getOriginalLineNumber(lb.getURL(), lb.getLineNumber(), timeStamp);
        synchronized (this) {
            bpLines.put(lb, line);
            lineNumberListener = new PropertyChangeListener() {
                public void propertyChange(PropertyChangeEvent evt) {
                    if (LineBreakpoint.PROP_LINE_NUMBER.equals(evt.getPropertyName())) {
                        final Map<LineBreakpoint, Integer> bpLines;
                        synchronized (LineTranslations.this) {
                            bpLines = originalBreakpointLines.get(timeStamp);
                            if (bpLines == null) {
                                return ;
                            }
                        }
                        LineBreakpoint lb = (LineBreakpoint) evt.getSource();
                        int line = getOriginalLineNumber(lb.getURL(), lb.getLineNumber(), timeStamp);
                        synchronized (LineTranslations.this) {
                            bpLines.put(lb, line);
                        }
                    }
                }
            };
            breakpointListeners.put(timeStamp, lineNumberListener);
        }
        lb.addPropertyChangeListener(WeakListeners.propertyChange(lineNumberListener, lb));
        return line;
    }
    
    /**
     * Returns the original line number.
     *
     * @param url The URL
     * @param currentLineNumber The current line number
     * @param timeStamp a time stamp to be used
     *
     * @return The original line number
     */
    int getOriginalLineNumber (
        String url,
        int currentLineNumber,
        Object timeStamp
    ) {
        //System.err.println("getOriginalLineNumber("+url+", "+currentLineNumber+", "+timeStamp+")");
        if (timeStamp == null) {
            return currentLineNumber;
        } else {
            Line.Set lineSet = getLineSet (url, timeStamp);
            if (lineSet == null) return currentLineNumber;
            //System.err.println("  lineSet = "+lineSet+"date = "+lineSet.getDate());
            try {
                //Line line = lineSet.getCurrent(currentLineNumber);
                //System.err.println("  current line = "+line);
                //System.err.println("  original line = "+lineSet.getOriginalLineNumber(line));
                //System.err.println("  original line2 = "+lineSet.getOriginal(currentLineNumber));
                //System.err.println("Original line of "+currentLineNumber+" IS "+lineSet.getOriginalLineNumber(lineSet.getCurrent(currentLineNumber)));
                return lineSet.getOriginalLineNumber(lineSet.getCurrent(currentLineNumber));
            } catch (IndexOutOfBoundsException ioobex) {
                //ioobex.printStackTrace();
                //System.err.println("  getOriginalLineNumber.return "+currentLineNumber);
                return currentLineNumber;
            }
        }
    }
    
    /**
     * Updates timeStamp for gived url.
     *
     * @param timeStamp time stamp to be updated
     * @param url an url
     */
    void updateTimeStamp (Object timeStamp, String url) {
        //System.err.println("LineTranslations.updateTimeStamp("+timeStamp+", "+url+")");
        DataObject dobj = getDataObject (url);
        synchronized (this) {
            Registry registry = timeStampToRegistry.get (timeStamp);
            registry.register (dobj);
            Map<LineBreakpoint, Integer> bpLines = originalBreakpointLines.get(timeStamp);
            if (bpLines != null) {
                Set<LineBreakpoint> bpts = new HashSet<LineBreakpoint>(bpLines.keySet());
                for (LineBreakpoint bp : bpts) {
                    if (url.equals(bp.getURL())) {
                        bpLines.remove(bp);
                    }
                }
            }
        }
    }

    Line.Set getLineSet (String url, Object timeStamp) {
        DataObject dataObject = getDataObject (url);
        if (dataObject == null) return null;
        
        if (timeStamp != null) {
            // get original
            synchronized (this) {
                Registry registry = timeStampToRegistry.get (timeStamp);
                if (registry != null) {
                    Line.Set ls = registry.getLineSet (dataObject);
                    if (ls != null) return ls;
                }
            }
        }
        
        // get current
        LineCookie lineCookie = dataObject.getCookie(LineCookie.class);
        if (lineCookie == null) return null;
        return lineCookie.getLineSet ();
    }

    Line getLine (String url, int lineNumber, Object timeStamp) {
        //System.err.println("LineTranslations.getLine("+lineNumber+", "+timeStamp+")");
        Line.Set ls = getLineSet (url, timeStamp);
        //System.err.println("  Line.Set = "+ls+", date = "+ls.getDate());
        //System.err.println("  current("+(lineNumber-1)+") = "+ls.getCurrent (lineNumber - 1));
        //System.err.println("  originl("+(lineNumber-1)+") = "+ls.getOriginal (lineNumber - 1));
        if (ls == null) return null;
        try {
            if (timeStamp == null)
                return ls.getCurrent (lineNumber - 1);
            else
                return ls.getOriginal (lineNumber - 1);
        } catch (IndexOutOfBoundsException e) {
        } catch (IllegalArgumentException e) {
        }
        return null;
    }
    
    void registerForLineUpdates( JavaFXLineBreakpoint lb ) {
        //translatedBreakpoints.add(lb);
        DataObject dobj = getDataObject(lb.getURL());
        if (dobj != null) {
            BreakpointLineUpdater blu = new BreakpointLineUpdater(lb, dobj);
            try {
                blu.attach();
                synchronized (this) {
                    lineUpdaters.put(lb, blu);
                }
            } catch (IOException ioex) {
                // Ignore
            }
        }
    }

    void unregisterFromLineUpdates( JavaFXLineBreakpoint lb ) {
        //translatedBreakpoints.remove(lb);
        BreakpointLineUpdater blu;
        synchronized (this) {
            blu = lineUpdaters.remove(lb);
        }
        if (blu != null) {
            blu.detach();
        }
        //if (timeStampToRegistry.isEmpty () && translatedBreakpoints.isEmpty()) {
        //    DataObject.getRegistry ().removeChangeListener (changedFilesListener);
        //   changedFilesListener = null;
        //}
    }

    private static DataObject getDataObject (String url) {
        FileObject file;
        try {
            file = URLMapper.findFileObject (new URL (url));
        } catch (MalformedURLException e) {
            return null;
        }

        if (file == null) return null;
        try {
            return DataObject.find (file);
        } catch (DataObjectNotFoundException ex) {
            return null;
        }
    }
    
    
    
    
    private static class Registry {
        
        private Map<DataObject, Line.Set> dataObjectToLineSet = new HashMap<DataObject, Line.Set>();
        
        synchronized void register (DataObject dataObject) {
            LineCookie lc = dataObject.getCookie (LineCookie.class);
            if (lc == null) return;
            dataObjectToLineSet.put (dataObject, lc.getLineSet ());
        }
        
        synchronized void registerIfNotThere(DataObject dataObject) {
            if (!dataObjectToLineSet.containsKey(dataObject)) {
                register(dataObject);
            }
        }
        
        synchronized Line.Set getLineSet (DataObject dataObject) {
            return dataObjectToLineSet.get (dataObject);
        }

    }
    
    private class ChangedFilesListener implements ChangeListener {
        public void stateChanged (ChangeEvent e) {
            Set<DataObject> newDOs = new HashSet<DataObject>(
                DataObject.getRegistry ().getModifiedSet()
            );
            synchronized (LineTranslations.this) {
                //newDOs.removeAll (modifiedDataObjects);
                for (Registry r : timeStampToRegistry.values ()) {
                    for (DataObject dobj : newDOs) {
                        r.registerIfNotThere (dobj);
                    }
                }
                //modifiedDataObjects = DataObject.getRegistry().getModifiedSet();
            }
        }
    }
    
    private class BreakpointLineUpdater implements PropertyChangeListener {
        
        private final JavaFXLineBreakpoint lb;
        private DataObject dataObject;
        private Line line;
        private boolean updatingLine = false;
        
        public BreakpointLineUpdater( JavaFXLineBreakpoint lb, DataObject dataObject ) {
            this.lb = lb;
            this.dataObject = dataObject;
        }
        
        public synchronized void attach() throws IOException {
            LineCookie lc = dataObject.getCookie (LineCookie.class);
            if (lc == null) return ;
            lb.addPropertyChangeListener(this);
            try {
                this.line = lc.getLineSet().getCurrent(lb.getLineNumber() - 1);
                line.addPropertyChangeListener(this);
            } catch( IndexOutOfBoundsException ioobex ) {
                // ignore document changes for BP with bad line number
            }
        }
        
        public synchronized void detach() {
            lb.removePropertyChangeListener(this);
            if (line != null) {
                line.removePropertyChangeListener(this);
            }
        }

        private void update(Line l) {
            try {
                int ln;
                synchronized (this) {
                    updatingLine = true;
                    ln = l.getLineNumber() + 1;
                }
                lb.setLineNumber(ln);
            } finally {
                synchronized (this) {
                    updatingLine = false;
                }
            }
        }

        public void propertyChange(PropertyChangeEvent evt) {
            String propertyName = evt.getPropertyName();
            Line l;
            boolean ul;
            synchronized (this) {
                l = this.line;
                ul = this.updatingLine;
            }
            if (Line.PROP_LINE_NUMBER.equals(propertyName) && l == evt.getSource()) {
                update(l);
                return ;
            }
            if (!ul && LineBreakpoint.PROP_LINE_NUMBER.equals(propertyName)) {
                DataObject dobj;
                synchronized (this) {
                    line.removePropertyChangeListener(this);
                    if (dataObject == null) return ;
                    dobj = dataObject;
                }
                Line newLine;
                try {
                    LineCookie lc = dobj.getCookie (LineCookie.class);
                    newLine = lc.getLineSet().getCurrent(lb.getLineNumber() - 1);
                    newLine.addPropertyChangeListener(this);
                } catch (IndexOutOfBoundsException ioobex) {
                    newLine = null;
                }
                synchronized (this) {
                    line = newLine;
                }
            }
            if (LineBreakpoint.PROP_URL.equals(propertyName)) {
                DataObject newDO = getDataObject(lb.getURL());
                Line newLine;
                if (newDO != null) {
                    LineCookie lc = newDO.getCookie (LineCookie.class);
                    try {
                        newLine = lc.getLineSet().getCurrent(lb.getLineNumber() - 1);
                        newLine.addPropertyChangeListener(this);
                    } catch (IndexOutOfBoundsException ioobex) {
                        // ignore document changes for BP with bad line number
                        newLine = null;
                    }
                } else {
                    newLine = null;
                }
                synchronized (this) {
                    // detach
                    line.removePropertyChangeListener(this);

                    // update DataObject
                    this.dataObject = newDO;

                    // attach
                    this.line = newLine;
                }
            }
        }
        
    }
    
}
