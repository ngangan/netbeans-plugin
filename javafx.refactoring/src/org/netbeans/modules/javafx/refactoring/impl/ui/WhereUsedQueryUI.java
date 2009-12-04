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
package org.netbeans.modules.javafx.refactoring.impl.ui;

import java.text.MessageFormat;
import java.util.ResourceBundle;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.swing.event.ChangeListener;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.modules.javafx.refactoring.impl.ElementLocation;
import org.netbeans.modules.javafx.refactoring.impl.WhereUsedQueryConstants;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.TreePathHandle;
import org.netbeans.modules.refactoring.api.AbstractRefactoring;
import org.netbeans.modules.refactoring.api.WhereUsedQuery;
import org.netbeans.modules.refactoring.spi.ui.CustomRefactoringPanel;
import org.netbeans.modules.refactoring.spi.ui.RefactoringUI;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;
import org.openide.util.lookup.Lookups;

/**
 * WhereUsedQueryUI from the Java refactoring module, only moderately modified for Ruby
 * 
 * @author Martin Matula, Jan Becicka
 */
public class WhereUsedQueryUI implements RefactoringUI {

    private WhereUsedQuery query = null;
    private final String name;
    private WhereUsedPanel panel;
    private final ElementLocation location;
    private ElementKind kind;
    private AbstractRefactoring delegate;

    public WhereUsedQueryUI(ElementLocation location, CompilationInfo info) {
        this.query = new WhereUsedQuery(Lookups.singleton(location));
        // XXX - Parsing API
//        ClasspathInfo classpathInfoFor = RetoucheUtils.getClasspathInfoFor(jmiObject);
//        if (classpathInfoFor != null) {
//            this.query.getContext().add(classpathInfoFor);
//        }
        this.location = location;
//        this.handle = tph;
        Element e = location.getElement(info);
        name = e.getSimpleName().toString();
        kind = e.getKind();
    }
    
//    public WhereUsedQueryUI(TreePathHandle tph, String name, AbstractRefactoring delegate) {
//        this.delegate = delegate;
//        //this.query.getContext().add(info.getClasspathInfo());
//        this.handle = tph;
//        this.name = name;
//    }
    
    public boolean isQuery() {
        return true;
    }

    public CustomRefactoringPanel getPanel(ChangeListener parent) {
        if (panel == null) {
            panel = new WhereUsedPanel(name, location, parent);
        }
        return panel;
    }

    public org.netbeans.modules.refactoring.api.Problem setParameters() {
        query.putValue(WhereUsedQuery.SEARCH_IN_COMMENTS,panel.isSearchInComments());
        switch (kind) {
            case METHOD: {
                setForMethod();
                return query.checkParameters();    
            }
            case CLASS: {
                setForClass();
                return query.checkParameters();
            }
            default: {
                return null;
            }
        }
    }
    
    private void setForMethod() {
//        if (panel.isMethodFromBaseClass()) {
//            query.setRefactoringSource(Lookups.singleton(panel.getBaseMethod()));
//        } else {
//            query.setRefactoringSource(Lookups.singleton(handle));
//        }
        query.setRefactoringSource(Lookups.singleton(location));

        query.putValue(WhereUsedQueryConstants.FIND_OVERRIDING_METHODS,panel.isMethodOverriders());
        query.putValue(WhereUsedQuery.FIND_REFERENCES,panel.isMethodFindUsages());
        query.putValue(WhereUsedQueryConstants.SEARCH_FROM_BASECLASS, panel.isMethodFromBaseClass());
    }
    
    private void setForClass() {
        query.putValue(WhereUsedQueryConstants.FIND_SUBCLASSES,panel.isClassSubTypes());
        query.putValue(WhereUsedQueryConstants.FIND_DIRECT_SUBCLASSES,panel.isClassSubTypesDirectOnly());
        query.putValue(WhereUsedQuery.FIND_REFERENCES,panel.isClassFindUsages());
    }
    
    public org.netbeans.modules.refactoring.api.Problem checkParameters() {
        switch(kind) {
            case METHOD: {
                setForMethod();
                return query.fastCheckParameters();
            }
            case CLASS: {
                setForClass();
                return query.fastCheckParameters();
            }
            default: {
                return null;
            }
        }
    }

    public org.netbeans.modules.refactoring.api.AbstractRefactoring getRefactoring() {
        return query!=null?query:delegate;
    }

    public String getDescription() {
        if (panel!=null) {
            switch(kind) {
                case METHOD: {
                    String description = null;
                    if (panel.isMethodFindUsages()) {
                        description = getString("DSC_FindUsages");
                    }

                    if (panel.isMethodOverriders()) {
                        if (description != null) {
                            description += " " + getString("DSC_And") + " ";
                        } else {
                            description = "";
                        }
                        description += getString("DSC_WhereUsedMethodOverriders");
                    }

                    description += " " + getString("DSC_WhereUsedOf", panel.getMethodDeclaringClass() + '.' + name); //NOI18N
                    return description;
                }
                case CLASS: {
                    if (!panel.isClassFindUsages())
                    if (!panel.isClassSubTypesDirectOnly()) {
                    return getString("DSC_WhereUsedFindAllSubTypes", name);
                    } else {
                    return getString("DSC_WhereUsedFindDirectSubTypes", name);
                    }
                }
            }
        }
        return getString("DSC_WhereUsed", name);
    }
    
    private ResourceBundle bundle;
    private String getString(String key) {
        if (bundle == null) {
            bundle = NbBundle.getBundle(WhereUsedQueryUI.class);
        }
        return bundle.getString(key);
    }
    
    private String getString(String key, String value) {
        return new MessageFormat(getString(key)).format (new Object[] {value});
    }


    public String getName() {
        return new MessageFormat(NbBundle.getMessage(WhereUsedPanel.class, "LBL_WhereUsed")).format (
                    new Object[] {name}
                );
    }
    
    public boolean hasParameters() {
        return true;
    }

    public HelpCtx getHelpCtx() {
        return null;
    }
}
