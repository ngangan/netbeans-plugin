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
import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.modules.editor.structure.api.DocumentElement;
import org.netbeans.modules.javafx.fxd.composer.editor.completion.FXDCompletionItem;
import org.netbeans.modules.javafx.fxd.composer.editor.completion.FXDCompletionQuery;
import org.netbeans.modules.javafx.fxd.composer.lexer.FXDTokenId;
import org.netbeans.modules.javafx.fxd.composer.lexer.TokenUtils;
import org.netbeans.spi.editor.completion.CompletionResultSet;

/**
 *
 * @author avk
 */
public class AttributeArrayCompletionProvider extends AbstractCompletionProvider {

    private static final Logger LOG = Logger.getLogger(AttributeArrayCompletionProvider.class.getName());

    @Override
    protected void fillCompletionItems(CompletionResultSet resultSet, DocumentElement el, int caretOffset, TokenSequence<FXDTokenId> ts) {
        resultSet.addItem(new FXDCompletionItem("NOT READY " + el.getName() + "[" + el.getType() + "]", caretOffset));

        FXDTokenId prev = getPrevNonWhiteID(el, caretOffset, ts);
        FXDTokenId next = getNextNonWhiteID(el, caretOffset, ts);
        LOG.warning("ATTR_ARR PREV = " + prev + ", NEXT = " + next);

        if (prev == null && next == FXDTokenId.IDENTIFIER_ATTR) {
            // move ts to next non-white token. DO NOT REMOVE
            TokenUtils.getNextNonWhiteFwd(ts, caretOffset);
            if (ts.offset() < caretOffset) {
                // inside identifier
                processAttrId(resultSet, el, caretOffset);
            } else {
                // before identifier
                processParentDocElement(resultSet, el, caretOffset, ts);
            }
        } else if (prev == FXDTokenId.IDENTIFIER_ATTR && next == FXDTokenId.COLON) {
            // move ts to previous non-white token
            Token<FXDTokenId> prevT = TokenUtils.getNextNonWhiteBwd(ts, caretOffset);
            if (ts.offset() + prevT.length() == caretOffset) {
                // at the end of id before :
                processAttrId(resultSet, el, caretOffset);
            } else {
                // between id and :
                // nothing to suggest?
            }
        } else if (prev == FXDTokenId.COLON && next == FXDTokenId.LBRACKET) {
            // between id and {
            // nothing to suggest?
        } else if (prev == FXDTokenId.RBRACKET && next == null) {
            // after }.
            processParentDocElement(resultSet, el, caretOffset, ts);
        } else if (next != FXDTokenId.UNKNOWN) {
            // between { and }, but not before error
            //suggest possible attribute_array values
            processAttrArrayElements(resultSet, el, caretOffset);
        }
    }

    private void processAttrId(final CompletionResultSet resultSet,
            DocumentElement el, int caretOffset) {
        String nameStart = el.getName().substring(0, caretOffset - el.getStartOffset());
        DocumentElement parent = el.getParentElement();
        if (parent == null) {
            return;
        }
        fillItemsWithNodeAttrs(resultSet, parent, caretOffset, nameStart);
    }

    private void processAttrArrayElements(final CompletionResultSet resultSet,
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
        if (propType instanceof Element){
            Element elem = (Element)propType;
            List<Element> successors = new ArrayList<Element>();
            collectSuccessors(successors, elem);
            for (Element e : successors){
                resultSet.addItem(new FXDCompletionItem(e, caretOffset));
            }
        } else if (propType instanceof Enumeration){
            Enumeration enumn = (Enumeration)propType;
            Value[] values = enumn.values;
            for (Value value : values){
                resultSet.addItem(new FXDCompletionItem(value, caretOffset));
            }
        } else if (propType instanceof PrimitiveType){
            PrimitiveType primType = (PrimitiveType)propType;
            resultSet.addItem(new FXDCompletionItem(primType, caretOffset));
        }
    }

    private void collectSuccessors(final List<Element> successors, final Element elem) {
        if(!elem.isAbstract()){
            successors.add(elem);
        }
        if (!elem.isExtended()){
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

}
