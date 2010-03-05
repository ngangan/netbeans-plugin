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
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2008 Sun
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

package org.netbeans.modules.javafx.fxd.composer.editor.completion.providers;

import com.sun.javafx.tools.fxd.schema.model.AbstractSchemaElement;
import com.sun.javafx.tools.fxd.schema.model.Element;
import com.sun.javafx.tools.fxd.schema.model.Enumeration;
import com.sun.javafx.tools.fxd.schema.model.PrimitiveType;
import com.sun.javafx.tools.fxd.schema.model.Property;
import com.sun.javafx.tools.fxd.schema.model.SchemaVisitor;
import com.sun.javafx.tools.fxd.schema.model.Type;
import com.sun.javafx.tools.fxd.schema.model.Value;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.modules.editor.structure.api.DocumentElement;
import org.netbeans.modules.javafx.fxd.composer.editor.completion.FXDCompletionItem;
import org.netbeans.modules.javafx.fxd.composer.editor.completion.FXDCompletionQuery;
import org.netbeans.modules.javafx.fxd.composer.lexer.FXDTokenId;
import org.netbeans.modules.javafx.fxd.composer.lexer.TokenUtils;
import org.netbeans.modules.javafx.fxd.composer.model.FXDFileModel;
import org.netbeans.modules.javafx.fxd.schemamodel.FXDSchemaHelper;
import org.netbeans.spi.editor.completion.CompletionResultSet;

/**
 *
 * @author avk
 */
public abstract class AbstractCompletionProvider {

    private static final Logger LOG = Logger.getLogger(AbstractCompletionProvider.class.getName());

    protected abstract void fillCompletionItems(final CompletionResultSet resultSet,
            DocumentElement el, int caretOffset, TokenSequence<FXDTokenId> ts);

    public static void fillItemsForElement(CompletionResultSet resultSet,
            DocumentElement el, int caretOffset, TokenSequence<FXDTokenId> ts) {

        AbstractCompletionProvider provider = null;
        String type = el.getType();
        if (FXDFileModel.FXD_NODE.equals(type)) {
            provider = new NodeCompletionProvider();
        } else if (FXDFileModel.FXD_ATTRIBUTE_ARRAY.equals(type)) {
            provider = new AttributeArrayCompletionProvider();
        } else if (FXDFileModel.DOCUMENT_ROOT_ELEMENT_TYPE.equals(type)) {
            provider = new RootElemCompletionProvider();
        } else if (FXDFileModel.FXD_ATTRIBUTE.equals(type)) {
            // is not used?
            provider = new AttributeCompletionProvider();
        } else if (FXDFileModel.FXD_ARRAY_ELEM.equals(type)) {
            // is used in the following cursor position(marked as '|'): 'content[ |xxx ]'
            provider = new ArrayElemCompletionProvider();
        }

        if (provider != null){
            provider.fillCompletionItems(resultSet, el, caretOffset, ts);
        }
    }

    protected void processParentDocElement(final CompletionResultSet resultSet,
            DocumentElement el, int caretOffset, TokenSequence<FXDTokenId> ts) {
        DocumentElement parent = el.getParentElement();
        if (parent != null) {
            fillItemsForElement(resultSet, parent, caretOffset, ts);
        }
    }

    protected FXDTokenId getPrevNonWhiteID(DocumentElement el, int caretOffset, TokenSequence<FXDTokenId> ts) {
        Token<FXDTokenId> t = TokenUtils.getNextNonWhiteBwd(ts, caretOffset);
        if (t == null) {
            return null;
        }
        if (el.getStartOffset() > ts.offset() + t.length()) {
            return null;
        }
        return t.id();
    }

    protected FXDTokenId getNextNonWhiteID(DocumentElement el, int caretOffset, TokenSequence<FXDTokenId> ts) {
        Token<FXDTokenId> t = TokenUtils.getNextNonWhiteFwd(ts, caretOffset);
        if (t == null) {
            return null;
        }
        if (el.getEndOffset() < ts.offset()) {
            return null;
        }
        return t.id();
    }

    protected void fillItemsWithNodeAttrs(final CompletionResultSet resultSet,
            DocumentElement el, int caretOffset, String nameStart) {

        AbstractSchemaElement schemaEl = getSchemaElementsForDE(el);
        if (schemaEl == null) {
            return;
        }
        List<Property> attrs = new ArrayList<Property>();

        collectElementAttrs(attrs, schemaEl, null);

        fillCompletionWithSchemaElements(resultSet, attrs, caretOffset, nameStart);
    }

    // TODO add check for parent node to suggest only valid ids.
    protected void fillCompletionByNameStart(final CompletionResultSet resultSet,
            DocumentElement el, final int caretOffset) {
        final String nameStart = el.getName().substring(0, caretOffset - el.getStartOffset());
        final int startOffset = el.getStartOffset();
        //LOG.warning("---- FIND COMPLETION FOR : "+nameStart);
        FXDCompletionQuery.getFXDSchema().visit(new SchemaVisitor() {

            public void visitSchemaElement(AbstractSchemaElement ae) {
                // collect schema elements with matching ids || element and enum names
                if (ae.id.startsWith(nameStart) || nameStartsWith(ae.id, nameStart)) {
                    resultSet.addItem(new FXDCompletionItem(ae, startOffset));
                }
            }

            private boolean nameStartsWith(String id, String nameStart) {
                int idx = id.lastIndexOf('-');
                if (idx > -1) {
                    return id.substring(idx).startsWith(nameStart);
                }
                return false;
            }
        });
    }


    /**
     * finds schema element that describes gived Document Element.
     * Schema element is searced by document element name.
     * @param el
     * @return
     */
    protected AbstractSchemaElement getSchemaElementsForDE(DocumentElement el) {
        final List<AbstractSchemaElement> result = new ArrayList<AbstractSchemaElement>();
        final String id = getSchemaIdByDocElem(el);
        FXDCompletionQuery.getFXDSchema().visit(new SchemaVisitor() {

            public void visitSchemaElement(AbstractSchemaElement ae) {
                if (ae instanceof Element || ae instanceof Enumeration) {
                    if (ae.id.equals(id)) {
                        result.add(ae);
                    }
                }
            }
        });

        return !result.isEmpty() ? result.get(0) : null;
    }

    protected AbstractSchemaElement getParentSchemaElementsForDE(DocumentElement el) {
        DocumentElement parent = el.getParentElement();
        if (parent == null) {
            return null;
        }
        return getSchemaElementsForDE(parent);
    }

    protected List<String> getDocElementAttrsIds(DocumentElement elem) {
        List<String> attrIDs = new ArrayList<String>();
        List<DocumentElement> children = elem.getChildren();
        for (DocumentElement ch : children) {
            String type = ch.getType();
            if (FXDFileModel.FXD_ATTRIBUTE_ARRAY.equals(type) || FXDFileModel.FXD_ATTRIBUTE.equals(type)) {
                attrIDs.add(ch.getName());
            }
        }
        return attrIDs;
    }


    /**
     * fills provided list with all possible Properties of AbstractSchemaElement.
     * including it's direct and inherited attributes.
     * @param attrs
     * @param ae
     * @param excludeIDs
     */
    protected void collectElementAttrs(List<Property> attrs,
            AbstractSchemaElement ae, List<String> excludeIDs) {
        if (ae instanceof Element) {
            Element elem = (Element) ae;
            Property[] props = elem.properties;
            for (Property prop : props) {
                if (excludeIDs != null && excludeIDs.contains(prop.id)){
                    continue;
                }
                attrs.add(prop);
            }
            if (elem.extendsElement != null) {
                collectElementAttrs(attrs, elem.extendsElement, excludeIDs);
            }

        }
    }

    protected void collectElementAttrs(List<Property> attrs,
            AbstractSchemaElement[] aes, List<String> excludeIDs) {
        for (AbstractSchemaElement ae : aes) {
            collectElementAttrs(attrs, ae, excludeIDs);
        }
    }
    
    /**
     * Finds Property of given AbstractSchemaElement by given property id.
     * Looks for element's direct and inherited attributes.
     * @param ae
     * @param attrName
     */
    protected Property findElementPropertyByName(AbstractSchemaElement parentElem, String attrId) {
        assert attrId != null;
        if (parentElem instanceof Element) {
            Element elem = (Element) parentElem;
            Property[] props = elem.properties;
            for (Property prop : props) {
                if (attrId.equalsIgnoreCase(prop.id)){
                    return prop;
                }
            }
            if (elem.extendsElement != null) {
                Property p = findElementPropertyByName(elem.extendsElement, attrId);
                if (p != null){
                    return p;
                }
            }

        }
        return null;
    }

    protected Property findElementPropertyByName(AbstractSchemaElement[] parentElements, String attrId) {
        for (AbstractSchemaElement parentElem : parentElements) {
            Property p = findElementPropertyByName(parentElem, attrId);
            if (p != null) {
                return p;
            }
        }
        return null;
    }

    protected void fillCompletionWithSchemaElements(CompletionResultSet resultSet,
            List<? extends AbstractSchemaElement> aes, int caretOffset) {
        fillCompletionWithSchemaElements(resultSet, null, caretOffset, null);
    }
    protected void fillCompletionWithSchemaElements(CompletionResultSet resultSet,
            List<? extends AbstractSchemaElement> aes, int caretOffset, String nameStart) {
        if (nameStart == null || nameStart.equals("")) {
            for (AbstractSchemaElement ae : aes) {
                resultSet.addItem(new FXDCompletionItem(ae, caretOffset));
            }
        } else {
            for (AbstractSchemaElement ae : aes) {
                if (ae.id.startsWith(nameStart)) {
                    resultSet.addItem(new FXDCompletionItem(ae, caretOffset));
                }
            }
        }
    }

    protected void processAttrValue(final CompletionResultSet resultSet,
            DocumentElement el, int caretOffset){

        AbstractSchemaElement parentEl = getParentSchemaElementsForDE(el);
        if (parentEl == null){
            return;
        }
        Property prop = findElementPropertyByName(parentEl, el.getName());
        fillItemsForProperty(resultSet, prop, caretOffset);
    }

    private void fillItemsForProperty(CompletionResultSet resultSet,
            Property prop, int caretOffset) {
        Type propType = prop.type;
        if (propType instanceof Element) {
            Element elem = (Element) propType;
            List<Element> successors = new ArrayList<Element>();
            collectSuccessors(successors, elem);
            for (Element e : successors) {
                resultSet.addItem(new FXDCompletionItem(e, caretOffset));
            }
        } else if (propType instanceof Enumeration) {
            Enumeration enumn = (Enumeration) propType;
            Value[] values = enumn.values;
            for (Value value : values) {
                resultSet.addItem(new FXDCompletionItem(value, caretOffset));
            }
        } else if (propType instanceof PrimitiveType) {
            //PrimitiveType primType = (PrimitiveType)propType;
            //resultSet.addItem(new FXDCompletionItem(primType, caretOffset));
        }
    }

    private void collectSuccessors(final List<Element> successors, final Element elem) {
        if (!elem.isAbstract()) {
            successors.add(elem);
        }
        if (!elem.isExtended()) {
            return;
        }
        FXDCompletionQuery.getFXDSchema().visit(new SchemaVisitor() {

            public void visitSchemaElement(AbstractSchemaElement ae) {
                if (ae instanceof Element) {
                    Element child = (Element) ae;
                    if (isExtendedBy(elem, child)) {
                        collectSuccessors(successors, child);
                    }
                }
            }

            private boolean isExtendedBy(Element parent, Element child) {
                if (child.extendsElement != null) {
                    for (int i = 0; i < child.extendsElement.length; i++) {
                        if (child.extendsElement[i].id.equals(parent.id)) {
                            return true;
                        }
                    }
                }
                return false;
            }
        });

    }

    private String getSchemaIdByDocElem(DocumentElement el) {
        return getSchemaIdByName(el.getName());
    }

    private String getSchemaIdByName(String elementName) {
        return FXDSchemaHelper.getFXDSchemaId(elementName);
    }


}
