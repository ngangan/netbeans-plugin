/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
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
 * Portions Copyrighted 2008 Sun Microsystems, Inc.
 */

package org.netbeans.modules.javafx.editor.imports;

import com.sun.javafx.api.tree.ImportTree;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.javafx.api.tree.Tree;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.modules.javafx.editor.JFXImportManager;

import javax.lang.model.element.Element;
import java.util.Collection;
import java.util.List;
import java.util.TreeSet;
import java.util.logging.Logger;

/**
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.orgm">RKo</a>)
 * @todo documentation
 */
public final class ImportsModel {
    public static final int UNSET = -1;
    public static final Logger logger = Logger.getLogger(JFXImportManager.class.getName());
    private final Collection<ModelEntry> entries = new TreeSet<ModelEntry>();
    private final int start;
    private final int end;


    ImportsModel(CompilationInfo ci) {
        if (ci == null) throw new IllegalArgumentException(java.util.ResourceBundle.getBundle("org/netbeans/modules/javafx/editor/imports/Bundle").getString("Compilation_info_cannot_be_null.")); // NOI18N
        List<? extends ImportTree> trees = ci.getCompilationUnit().getImports();
        if (!trees.isEmpty()) {
            int start = Integer.MAX_VALUE;
            int end = 0;
            for (ImportTree anImport : trees) {
                entries.add(new ModelEntry(anImport));
                start = Math.min(start, getStart(anImport, ci));
                end = Math.max(end, getEnd(anImport, ci));
            }
            this.start = start;
            this.end = end;
        } else {
            end = start = UNSET;            
        }
    }

    public int getStart() {
        return start;
    }

    public int getEnd() {
        return end;
    }

    private int getEnd(Tree node, CompilationInfo ci) {
        return (int) sp(ci).getEndPosition(ci.getCompilationUnit(), node);
    }

    private SourcePositions sp(CompilationInfo ci) {
        return ci.getTrees().getSourcePositions();
    }

    private int getStart(Tree node, CompilationInfo ci) {
        return (int) sp(ci).getStartPosition(ci.getCompilationUnit(), node);
    }


    public void addImport(String qn) {
        entries.add(new ModelEntry(qn));
    }

    private boolean isLocal(Element e) {
        return false;
    }

    boolean isImported(Element e) {
        if (isLocal(e)) return true;
        for (ModelEntry entry : entries) {
            // #158596 - use e.toString() instead of e.asType().toString() to prevent problems with generified types
            if (entry != null && entry.includes(e.toString())) {
                entry.setUsage();
                return true;
            }
        }
        return false;
    }


    void optimize() {

    }

    void append(ModelEntry modelEntry) {
        entries.add(modelEntry);
    }

    public Iterable<ModelEntry> getEntries() {
        return entries;
    }


    static class ModelEntry implements Comparable<ModelEntry> {
        String type;
        ImportTree tree;
        boolean stared;
        boolean dStared;
        boolean isUsed = false;

        public ModelEntry(ImportTree tree) {
            this.tree = tree;
            Tree qi = tree.getQualifiedIdentifier();
            type = qi.toString();
            verifyType();
        }

        private void verifyType() {
            stared = type.endsWith(".*"); // NOI18N
            dStared = type.endsWith(".**"); // NOI18N
            if (stared || dStared) {
                int index = type.indexOf(".*"); // NOI18N
                type = type.substring(0, index);
            }
        }

        ModelEntry(String type) {
            this.type = type;
            verifyType();
            isUsed = true;
        }

        boolean includes(String type) {
            if (type == null) return false;
            if (dStared) {
                return type.startsWith(this.type);
            } else if (stared) {
                int dotIndex = type.lastIndexOf('.'); // NOI18N
                return dotIndex > -1 && this.type.equals(type.substring(0, dotIndex));
            }
            return this.type.equals(type) || canBeThisType(type);
        }

        private boolean canBeThisType(String type) { // NOI18N
            int index = this.type.lastIndexOf('.');
            return index > 0 && this.type.substring(index).equals(type);
        }

        String toImportStatement() {
            return "import " + type + (stared ? ".*" : "") + (dStared ? ".**" : "") + ";"; // NOI18N
        }

        void setUsage() {
            isUsed = true;
        }

        boolean isUsed() {
            return isUsed;
        }

        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            ModelEntry that = (ModelEntry) o;

//            if (dStared != that.dStared) return false;
//            if (stared != that.stared) return false;
//            if (tree != null ? !tree.equals(that.tree) : that.tree != null) return false;
            if (type != null ? !type.equals(that.type) : that.type != null) return false;

            return true;
        }

        public int hashCode() {
            int result;
            result = (type != null ? type.hashCode() : 0);
//            result = 31 * result + (tree != null ? tree.hashCode() : 0);
//            result = 31 * result + (stared ? 1 : 0);
//            result = 31 * result + (dStared ? 1 : 0);
            return result;
        }

        public int compareTo(ModelEntry o) {
            return type != null ? o != null ? type.compareToIgnoreCase(o.type) : -1 : 1;
        }


        public String toString() {
            return "ModelEntry[" + // NOI18N
                    "type='" + type + '\'' + // NOI18N
                    ']'; // NOI18N
        }
    }


}
