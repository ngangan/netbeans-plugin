/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring.transformations;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.netbeans.editor.BaseDocument;
import org.netbeans.modules.refactoring.api.ProgressEvent;
import org.netbeans.modules.refactoring.api.ProgressListener;
import org.netbeans.modules.refactoring.api.RefactoringSession;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObject;
import org.openide.loaders.DataObjectNotFoundException;
import org.openide.text.DataEditorSupport;

/**
 *
 * @author Jaroslav Bachorik <jaroslav.bachorik@sun.com>
 */
abstract public class Transformer {
    final private static Logger LOGGER = Logger.getLogger(Transformer.class.getName());
    
    private StringBuilder builder;
    private TransformationContext context = new TransformationContext();
    
    final private static Map<RefactoringSession, Map<FileObject, Transformer>> foTransformers = new WeakHashMap<RefactoringSession, Map<FileObject, Transformer>>();

    final private List<Transformation> transformations;

    private int applyMark = -1, revertMark = -1;

    final private ProgressListener pl = new ProgressListener() {

        public void start(ProgressEvent pe) {
            applyMark = -1;
            revertMark = -1;
        }

        public void step(ProgressEvent pe) {
            // ignore
        }

        public void stop(ProgressEvent pe) {
            applyMark = -1;
            revertMark = -1;
        }
    };

    protected Transformer(CharSequence content) {
        builder = new StringBuilder(content);
        transformations = new ArrayList<Transformation>();
    }

    public static Transformer forText(String text) {
        return new StringTransformer(text);
    }

    public static Transformer forDocument(BaseDocument doc) {
        return new DocumentTransformer(doc);
    }

    public static Transformer forFileObject(FileObject fo, RefactoringSession session) {
        return forFileObject(fo, session, true);
    }

    public static Transformer forFileObject(FileObject fo, RefactoringSession session, boolean shared) {
        synchronized(foTransformers) {
            Map<FileObject, Transformer> map = foTransformers.get(session);
            if (map == null) {
                map = new WeakHashMap<FileObject, Transformer>();
                foTransformers.put(session, map);
            }
            Transformer t = map.get(fo);
            if (t == null) {
                try {
                    DataObject dobj = DataObject.find(fo);
                    DataEditorSupport des = (DataEditorSupport) dobj.getCookie(EditorCookie.class);
                    t = forDocument((BaseDocument) des.openDocument());
                    map.put(fo, t);
                    session.addProgressListener(t.pl);
                } catch (DataObjectNotFoundException dataObjectNotFoundException) {
                    LOGGER.log(Level.WARNING, null, dataObjectNotFoundException);
                } catch (IOException e) {
                    LOGGER.log(Level.WARNING, null, e);
                }
            }
            return shared ? t : t.newClone();
        }
    }

    final synchronized void insertText(int pos, String text) {
        int realPos = context.getRealOffset(pos);
        builder.insert(realPos, text);
        context.replaceText(pos, 0, text.length());
    }

    final synchronized String removeText(int pos, int len) {
        int realPos = context.getRealOffset(pos);
        String removed = builder.subSequence(realPos, realPos + len).toString();
        builder.delete(realPos, realPos + len);
        context.replaceText(pos, len, 0);
        return removed;
    }

    final synchronized void replaceText(int pos, String oldText, String newText) {
        int realPos = context.getRealOffset(pos);
        builder.replace(realPos, realPos + oldText.length(), newText);
        context.replaceText(pos, oldText.length(), newText.length());
    }

    final public void addTransformation(Transformation t) {
        if (!transformations.contains(t)) transformations.add(t);
    }

    final public void removeTransformation(Transformation t) {
        transformations.remove(t);
    }

    final public void addTransformations(Collection<Transformation> ts) {
        for(Transformation t : ts) {
            addTransformation(t);
        }
    }

    final public void addTransformations(Transformation ... ts) {
        addTransformations(Arrays.asList(ts));
    }

    synchronized private String applyTransforms() {
        try {
            int transformationCounter = 0;

            for(Transformation t : transformations) {
                if (transformationCounter > applyMark) {
                    t.perform(this);
                    applyMark = transformationCounter;
                }
                transformationCounter++;
            }
            return builder.toString();
        } finally {

        }
    }

    synchronized private String revertTransforms() {
        try {
            int transformationCounter = 0;

            for(int i=transformations.size() - 1;i>=0;i--) {
                Transformation t = transformations.get(i);
                if (transformationCounter > revertMark) {
                    t.revert(this);
                    revertMark = transformationCounter;
                }
                transformationCounter++;
            }
            return builder.toString();
        } finally {
            
        }
    }

    final synchronized public String preview() {
        return applyTransforms();
    }

    final synchronized public void transform() {
        String transformed = applyTransforms();
        saveTransformed(transformed);
    }

    final synchronized public void revert() {
        String transformed = revertTransforms();
        saveTransformed(transformed);
    }

    abstract protected void saveTransformed(String transformed);
    abstract protected Transformer newClone();
}
