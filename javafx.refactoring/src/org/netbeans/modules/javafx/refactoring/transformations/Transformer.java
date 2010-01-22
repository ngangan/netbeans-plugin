/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring.transformations;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;
import org.netbeans.editor.BaseDocument;
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
    private StringBuilder builder;
    private TransformationContext context;
    
    final private static Map<FileObject, Transformer> foTransformers = new WeakHashMap<FileObject, Transformer>();

    final private List<Transformation> transformations;
    private int lastAppliedTransformation = -1;
    
    final private String contentBackup;

    protected Transformer(CharSequence content) {
        builder = new StringBuilder(content);
        context = new TransformationContext();
        transformations = new ArrayList<Transformation>();
        contentBackup = builder.toString();
    }

    public static Transformer forText(String text) {
        return new StringTransformer(text);
    }

    public static Transformer forDocument(BaseDocument doc) {
        return new DocumentTransformer(doc);
    }

    public static Transformer forFileObject(FileObject fo) {
        synchronized(foTransformers) {
            Transformer t = foTransformers.get(fo);
            if (t == null) {
                try {
                    DataObject dobj = DataObject.find(fo);
                    DataEditorSupport des = (DataEditorSupport) dobj.getCookie(EditorCookie.class);
                    t = forDocument((BaseDocument) des.openDocument());
                    foTransformers.put(fo, t);
                } catch (DataObjectNotFoundException dataObjectNotFoundException) {
                    // LOG
                } catch (IOException e) {
                    // LOG
                }
            }
            return t;
        }
    }

    final synchronized void insertText(int pos, String text) {
        int realPos = context.getRealOffset(pos);
        builder.insert(realPos, text);
        context.replaceText(pos, 0, text.length());
    }

    final synchronized void removeText(int pos, int len) {
        int realPos = context.getRealOffset(pos);
        builder.delete(realPos, realPos + len - 1);
        context.replaceText(pos, len, 0);
    }

    final synchronized void replaceText(int pos, String oldText, String newText) {
        int realPos = context.getRealOffset(pos);
        builder.replace(realPos, realPos + oldText.length(), newText);
        context.replaceText(pos, oldText.length(), newText.length());
    }

    final public void addTransformation(Transformation t) {
        transformations.add(t);
    }

    final public void removeTransformation(Transformation t) {
        transformations.remove(t);
    }

    final public void addTransformations(Collection<Transformation> ts) {
        transformations.addAll(ts);
    }

    final public void addTransformations(Transformation ... ts) {
        transformations.addAll(Arrays.asList(ts));
    }

    synchronized public String preview() {
        try {
            for(Transformation t : transformations) {
                t.perform(this);
            }
            return builder.toString();
        } finally {
            context = new TransformationContext();
            builder = new StringBuilder(contentBackup);
        }
    }

    synchronized public void transform() {
        throw new UnsupportedOperationException();
    }

    abstract public Transformer newClone();
}
