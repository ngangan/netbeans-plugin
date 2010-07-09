/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2010 Oracle and/or its affiliates. All rights reserved.
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
 * 
 * Contributor(s):
 * 
 * Portions Copyrighted 2008 Sun Microsystems, Inc.
 */
package qa.javafx.functional.library.project;

import org.netbeans.jemmy.operators.JPopupMenuOperator;
import org.netbeans.jemmy.operators.JTextComponentOperator;
import org.netbeans.jemmy.operators.JListOperator;
import qa.javafx.functional.library.Util;
import org.netbeans.jellytools.MainWindowOperator;

import java.awt.event.KeyEvent;
import java.awt.event.InputEvent;
import java.util.LinkedList;
import java.util.List;
import javax.swing.ListModel;

import javax.swing.SwingUtilities;
import qa.javafx.functional.library.operator.ClassNameComponentChooser;
import org.netbeans.jellytools.modules.editor.CompletionJListOperator;


/**
 *
 * @author andromeda
 */
public class EditorOperator extends org.netbeans.jellytools.EditorOperator {

    JTextComponentOperator textComponent;

    public EditorOperator(String name) {
        super(name);
        textComponent = new JTextComponentOperator(this);
        Util.waitScanFinished();

    }

    public void setText(){
        setText("");
    }
    
    public void setText(String text){
        textComponent.setText(text);
    }

    public void typeText(String text){
        //textComponent.setText(text);
        typeText(text, 0, 0);
    }
    
    public void typeText(String text, int line, int row){
        setCaretPosition(line, row);
        insert(text);
    }

    public void format(){
        clickForPopup();
        //Util.sleep(2000);
        //JPopupMenuOperator popup = new JPopupMenuOperator(textComponent);
        //JPopupMenuOperator popup = new JPopupMenuOperator(this);

        JPopupMenuOperator popup = new JPopupMenuOperator( MainWindowOperator.getDefault());

        //JPopupMenuOperator popup = new JPopupMenuOperator();
        popup.pushMenuNoBlock("Format");
    }

    public List<String> codecompletion(){
        typeKey(KeyEvent.VK_SPACE, ' ', InputEvent.CTRL_DOWN_MASK );

        Util.sleep(3000);

        final List<String> items = new LinkedList<String>();

        SwingUtilities.invokeLater(
                new Runnable(){

                    public void run(){
                        JListOperator list = new JListOperator(Util.getMainWindowOperator(), new ClassNameComponentChooser("CompletionJList"));
                        //Util.showComponents(list);
                        ListModel model = list.getModel();
                        for(int i=0; i< model.getSize(); i++){
                            items.add(model.getElementAt(i).toString());
                        }
//                        CompletionJListOperator completion  = new CompletionJListOperator();
//
//                        try{
//                            items.addAll(completion.getCompletionItems());
//                        }catch(Exception e){
//                            e.printStackTrace();
//                        }
                        System.out.println("---------------------------");
                        System.out.println("Items: " + items.size());
                        for(String item: items ){
                            System.out.println("Item: " + item);
                        }
                    }
                }
        );

        return items;
        //JPopupMenuOperator popup = new JPopupMenuOperator(this);

        //Util.showComponents(popup);
        
        //clickForPopup();
        //Util.sleep(2000);
        //JPopupMenuOperator popup = new JPopupMenuOperator(textComponent);
        //JPopupMenuOperator popup = new JPopupMenuOperator(this);

        //JPopupMenuOperator popup = new JPopupMenuOperator( MainWindowOperator.getDefault());

        //JPopupMenuOperator popup = new JPopupMenuOperator();
        //popup.pushMenuNoBlock("Format");
    }


    
    
}
