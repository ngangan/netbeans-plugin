/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright 2008 Sun Microsystems, Inc. All rights reserved.
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

package org.netbeans.modules.javafx.editor;

import com.sun.tools.javac.code.Symbol;
import java.lang.reflect.Constructor;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.ArrayType;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import org.netbeans.api.java.source.ElementHandle;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.openide.filesystems.FileObject;

/**
 *
 * @author nenik
 */
final class ElementOpen {

    private ElementOpen() {}
    
    /**
     * Opens the source file containing given {@link Element} and seek to
     * the declaration.
     * 
     * @param cpInfo ClasspathInfo which should be used for the search
     * @param el     declaration to open
     */
    public static void open(final CompilationInfo comp, final Element el) throws Exception {
        TypeElement tel = getEnclosingClassElement(el);

        if (!comp.getJavafxTypes().isJFXClass((Symbol)tel)) { // java
            openThroughJavaSupport(comp.getJavaFXSource().getFileObject(), el);
        } else {
            // todo: implement go to javafx element
    
        }
    }
    
    private static TypeElement getEnclosingClassElement(Element el) {
        while (el != null && el.getKind() != ElementKind.CLASS) {
            el = el.getEnclosingElement();
        }
        return (TypeElement)el; // or null
    }        


    // All of the following code is a hack to call through to the java
    // implementation of open support, where we need to pass "their"
    // implementation of Element/ElementHandle.
    // All of the Element serialization logic is copied and adapted from
    // javasource's ElementHandle and ClassFileUtils
    private static void openThroughJavaSupport(FileObject reference, Element el) throws Exception {
        org.netbeans.api.java.source.ClasspathInfo cpi = 
                org.netbeans.api.java.source.ClasspathInfo.create(reference);
        // Load the right version of the ElementKind class and convert our instance to it
        Class ekClass = ElementHandle.class.getClassLoader().loadClass("javax.lang.model.element.ElementKind");
        Object ekInstance = Enum.valueOf(ekClass, el.getKind().name());
        
        String[] sig = getSignatures(el);
        Class strArrClass = sig.getClass();
        
        Constructor ehCtor = ElementHandle.class.getDeclaredConstructor(ekClass, strArrClass);
        ehCtor.setAccessible(true);
        ElementHandle eh = (ElementHandle)ehCtor.newInstance(ekInstance, sig);
        
        org.netbeans.api.java.source.ui.ElementOpen.open(cpi, eh);
    }
    

    private static String[] getSignatures (final Element element) throws IllegalArgumentException {
        assert element != null;
        ElementKind kind = element.getKind();
        String[] signatures;
        switch (kind) {
            case PACKAGE:
                assert element instanceof PackageElement;
                signatures = new String[]{((PackageElement)element).getQualifiedName().toString()};
                break;
            case CLASS:
            case INTERFACE:
            case ENUM:
            case ANNOTATION_TYPE:
                signatures = new String[] {encodeClassNameOrArray((TypeElement)element)};
                break;
            case METHOD:
            case CONSTRUCTOR:                
            case INSTANCE_INIT:
            case STATIC_INIT:
                signatures = createExecutableDescriptor((ExecutableElement)element);
                break;
            case FIELD:
            case ENUM_CONSTANT:
                signatures = createFieldDescriptor((VariableElement)element);
                break;
            default:
                throw new IllegalArgumentException(kind.toString());
        }
        return signatures;
    }

    private static String encodeClassNameOrArray (TypeElement td) {
        assert td != null;
        CharSequence qname = td.getQualifiedName();
        TypeMirror enclosingType = td.getEnclosingElement().asType();
        if (qname != null && enclosingType != null && enclosingType.getKind() == TypeKind.NONE && "Array".equals(qname.toString())) {     //NOI18N
            return "[";  //NOI18N
        }
        else {
            return encodeClassName(td);
        }
    }
    
    private static String encodeClassName (TypeElement td) {
        assert td != null;
        StringBuilder sb = new StringBuilder ();
        encodeClassName(td, sb,'.');    // NOI18N
        return sb.toString();
    }
    
    private static void encodeType (final TypeMirror type, final StringBuilder sb) {
	switch (type.getKind()) {
	    case VOID:
		sb.append('V');	    // NOI18N
		break;
	    case BOOLEAN:
		sb.append('Z');	    // NOI18N
		break;
	    case BYTE:
		sb.append('B');	    // NOI18N
		break;
	    case SHORT:
		sb.append('S');	    // NOI18N
		break;
	    case INT:
		sb.append('I');	    // NOI18N
		break;
	    case LONG:
		sb.append('J');	    // NOI18N
		break;
	    case CHAR:
		sb.append('C');	    // NOI18N
		break;
	    case FLOAT:
		sb.append('F');	    // NOI18N
		break;
	    case DOUBLE:
		sb.append('D');	    // NOI18N
		break;
	    case ARRAY:
		sb.append('[');	    // NOI18N
		assert type instanceof ArrayType;
		encodeType(((ArrayType)type).getComponentType(),sb);
		break;
	    case DECLARED:
            {
		sb.append('L');	    // NOI18N
		TypeElement te = (TypeElement) ((DeclaredType)type).asElement();
		encodeClassName(te, sb,'/');
		sb.append(';');	    // NOI18N
		break;
            }
	    default:
		throw new IllegalArgumentException ();
	}                
    }        
    
    private static void encodeClassName (TypeElement te, final StringBuilder sb, final char separator) {
        sb.append(((Symbol.ClassSymbol)te).flatname.toString().replace('.', separator));
    }

    private static String[] createExecutableDescriptor (final ExecutableElement ee) {
        assert ee != null;
        final ElementKind kind = ee.getKind();
        final String[] result = (kind == ElementKind.STATIC_INIT || kind == ElementKind.INSTANCE_INIT) ? new String[2] : new String[3];
        final Element enclosingType = ee.getEnclosingElement();
	assert enclosingType instanceof TypeElement;
        result[0] = encodeClassNameOrArray ((TypeElement)enclosingType);        
        if (kind == ElementKind.METHOD || kind == ElementKind.CONSTRUCTOR) {
            final StringBuilder retType = new StringBuilder ();
            if (kind == ElementKind.METHOD) {
                result[1] = ee.getSimpleName().toString();
                encodeType(ee.getReturnType(), retType);
            }
            else {
                result[1] = "<init>";   // NOI18N
                retType.append('V');    // NOI18N
            }
            StringBuilder sb = new StringBuilder ();
            sb.append('(');             // NOI18N
            for (VariableElement pd : ee.getParameters()) {
                encodeType(pd.asType(),sb);
            }
            sb.append(')');             // NOI18N
            sb.append(retType);
            result[2] = sb.toString();
        }
        else if (kind == ElementKind.INSTANCE_INIT) {
            result[1] = "<init>";       // NOI18N
        } 
        else if (kind == ElementKind.STATIC_INIT) {
            result[1] = "<cinit>";      // NOI18N
        }
        else {
            throw new IllegalArgumentException ();
        }
        return result;
    }

    private static String[] createFieldDescriptor (final VariableElement ve) {
	assert ve != null;
        String[] result = new String[3];
	Element enclosingElement = ve.getEnclosingElement();
	assert enclosingElement instanceof TypeElement;
        result[0] = encodeClassNameOrArray ((TypeElement) enclosingElement);
        result[1] = ve.getSimpleName().toString();
        StringBuilder sb = new StringBuilder ();
        encodeType(ve.asType(),sb);
        result[2] = sb.toString();        
        return result;
    }        

}
