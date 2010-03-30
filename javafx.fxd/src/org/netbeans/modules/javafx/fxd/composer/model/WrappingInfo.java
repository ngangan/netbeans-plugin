/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
package org.netbeans.modules.javafx.fxd.composer.model;

import com.sun.javafx.tools.fxd.FXDRootElement;
import com.sun.javafx.tools.fxd.container.ContainerEntry;
import java.util.ArrayList;
import java.util.List;
import org.netbeans.modules.editor.structure.api.DocumentElement;

/**
 *
 * @author Andrey Korostelev
 */
public class WrappingInfo {

    private ContainerEntry m_entry;
    private FXDRootElement m_root;
    private List<DocumentElement> m_unresolved;

    public WrappingInfo(ContainerEntry ce, FXDRootElement root) {
        m_entry = ce;
        m_root = root;
        m_unresolved = new ArrayList<DocumentElement>();
    }

    public boolean addUnresolved(DocumentElement elem){
        return m_unresolved.add(elem);
    }

    public boolean isUnresolved(DocumentElement elem){
        return m_unresolved.contains(elem);
    }

    public boolean rmUnresolved(DocumentElement elem){
        return m_unresolved.remove(elem);
    }

    /**
     * returns stored ContainerEntry
     * @return stored Container Entry
     */
    public ContainerEntry getEntry() {
        return m_entry;
    }

    /**
     * returns stored root node
     * @return stored root node
     */
    public FXDRootElement getRoot() {
        return m_root;
    }

    protected void setRoot(FXDRootElement root){
        m_root = root;
    }
}
