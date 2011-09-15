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


package org.netbeans.modules.visage.navigation;


import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.Modifier;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JToggleButton;
import org.netbeans.modules.visage.navigation.ElementNode.Description;
import org.netbeans.modules.visage.navigation.actions.SortActionSupport;
import org.netbeans.modules.visage.navigation.base.FiltersDescription;
import org.netbeans.modules.visage.navigation.base.FiltersManager;
import org.openide.util.NbBundle;
import org.openide.util.NbPreferences;
import org.openide.util.Utilities;

/** Creates filtering for the ClassMemberPanel
 *
 * @author phrebejk
 */
public final class ClassMemberFilters {
    
    private ClassMemberPanelUI ui;
    
    /** constants for defined filters */
    private static final String SHOW_NON_PUBLIC = "show_non_public"; // NOI18N
    private static final String SHOW_STATIC = "show_static"; // NOI18N
    private static final String SHOW_FIELDS = "show_fields"; // NOI18N
    private static final String SHOW_INHERITED = "show_inherited"; // NOI18N
    
//    private static final String SORT_ALPHA = "sort_alpha";
//    private static final String SORT_POSITION = "sort_position";
    
    private FiltersManager filters;
    
    private boolean naturalSort = false;
    
    ClassMemberFilters(ClassMemberPanelUI ui) {
        this.ui = ui;
        naturalSort = NbPreferences.forModule(ClassMemberFilters.class).getBoolean("naturalSort", false); //NOI18N
    }

    public FiltersManager getInstance() {
        if (filters == null) {
            filters = createFilters();
        }
        return filters;
    }

    public JComponent getComponent() {
        FiltersManager f = getInstance();
        return f.getComponent(createSortButtons());
    }

    public Collection<Description> filter(Collection<Description> original) {

        boolean non_public = filters.isSelected(SHOW_NON_PUBLIC);
        boolean statik = filters.isSelected(SHOW_STATIC);
        boolean fields = filters.isSelected(SHOW_FIELDS);
        boolean inherited = filters.isSelected(SHOW_INHERITED);

        ArrayList<Description> result = new ArrayList<Description>(original.size());
        for (Description description : original) {

            if (!inherited && description.isInherited) {
                continue;
            }
            if (!non_public && description.modifiers != null && !description.modifiers.contains(Modifier.PUBLIC)) {
                continue;
            }

            if (!statik && description.modifiers != null && description.modifiers.contains(Modifier.STATIC)) {
                continue;
            }

            if (!fields && description.kind == ElementKind.FIELD) {
                continue;
            }

            // XXX Inherited members

            result.add(description);
        }

        Collections.sort(result, isNaturalSort() ? Description.POSITION_COMPARATOR : Description.ALPHA_COMPARATOR);

        return result;
    }

    public boolean isNaturalSort() {
        return naturalSort;
    }

    public void setNaturalSort(boolean naturalSort) {
        this.naturalSort = naturalSort;
        NbPreferences.forModule(ClassMemberFilters.class).putBoolean("naturalSort", naturalSort); //NOI18N
        if (null != sortByNameButton) {
            sortByNameButton.setSelected(!naturalSort);
        }
        if (null != sortByPositionButton) {
            sortByPositionButton.setSelected(naturalSort);
        }
        ui.sort();
    }

    // Privare methods ---------------------------------------------------------
    /** Creates filter descriptions and filters itself */
    private static FiltersManager createFilters() {
        FiltersDescription desc = new FiltersDescription();

        desc.addFilter(SHOW_INHERITED,
                NbBundle.getMessage(ClassMemberFilters.class, "LBL_ShowInherited"), //NOI18N
                NbBundle.getMessage(ClassMemberFilters.class, "LBL_ShowInheritedTip"), //NOI18N
                false,
                new ImageIcon(Utilities.loadImage("org/netbeans/modules/visage/navigation/resources/filterHideInherited.png")), //NOI18N
                null);
        desc.addFilter(SHOW_FIELDS,
                NbBundle.getMessage(ClassMemberFilters.class, "LBL_ShowFields"), //NOI18N
                NbBundle.getMessage(ClassMemberFilters.class, "LBL_ShowFieldsTip"), //NOI18N
                true,
                new ImageIcon(Utilities.loadImage("org/netbeans/modules/visage/navigation/resources/filterHideFields.gif")), //NOI18N
                null);
        desc.addFilter(SHOW_STATIC,
                NbBundle.getMessage(ClassMemberFilters.class, "LBL_ShowStatic"), //NOI18N
                NbBundle.getMessage(ClassMemberFilters.class, "LBL_ShowStaticTip"), //NOI18N
                true,
                new ImageIcon(Utilities.loadImage("org/netbeans/modules/visage/navigation/resources/filterHideStatic.png")), //NOI18N
                null);
        desc.addFilter(SHOW_NON_PUBLIC,
                NbBundle.getMessage(ClassMemberFilters.class, "LBL_ShowNonPublic"), //NOI18N
                NbBundle.getMessage(ClassMemberFilters.class, "LBL_ShowNonPublicTip"), //NOI18N
                true,
                new ImageIcon(Utilities.loadImage("org/netbeans/modules/visage/navigation/resources/filterHideNonPublic.png")), //NOI18N
                null);

        return FiltersDescription.createManager(desc);
    }
    private JToggleButton sortByNameButton;
    private JToggleButton sortByPositionButton;

    private JToggleButton[] createSortButtons() {
        JToggleButton[] res = new JToggleButton[2];

        if (null == sortByNameButton) {
            sortByNameButton = new JToggleButton(new SortActionSupport.SortByNameAction(this));
            sortByNameButton.setToolTipText(sortByNameButton.getText());
            sortByNameButton.setText(null);
            sortByNameButton.setSelected(!naturalSort);
            sortByNameButton.setFocusable(false);
        }
        res[0] = sortByNameButton;

        if (null == sortByPositionButton) {
            sortByPositionButton = new JToggleButton(new SortActionSupport.SortBySourceAction(this));
            sortByPositionButton.setToolTipText(sortByPositionButton.getText());
            sortByPositionButton.setText(null);
            sortByPositionButton.setSelected(naturalSort);
            sortByPositionButton.setFocusable(false);
        }
        res[1] = sortByPositionButton;
        return res;
    }
}
