package org.netbeans.api.javafx.editor;

import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.code.Symbol.ClassSymbol;
import com.sun.tools.javac.code.Type;
import com.sun.tools.javac.code.Type.MethodType;
import com.sun.tools.javac.code.TypeTags;
import com.sun.tools.javac.util.List;
import com.sun.tools.javafx.code.FunctionType;
import com.sun.tools.javafx.code.JavafxTypes;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLEncoder;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.StringTokenizer;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.TypeParameterElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.ArrayType;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.java.queries.JavadocForBinaryQuery;
import org.netbeans.api.java.queries.SourceForBinaryQuery;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.modules.javafx.source.classpath.FileObjects;
import org.netbeans.spi.java.classpath.support.ClassPathSupport;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileStateInvalidException;
import org.openide.filesystems.URLMapper;
import org.openide.util.Exceptions;

/**
 * 
 * @author Anton Chechel
 */
public final class FXSourceUtils {

    private static final char[] CODE_COMPL_SUBST_BREAKERS = {';', '.', ',', '+', '-', '/', '%', '^', '|', '&', '(', ')', '{', '}', ' ', '\t', '\n', '\r'}; // NOI18N
    private static final String PACKAGE_SUMMARY = "package-summary"; // NOI18N

    // colors for navigator
    private static final String TYPE_COLOR = "#707070"; // NOI18N
    private static final String INHERITED_COLOR = "#7D694A"; // NOI18N

    private FXSourceUtils() {
    }

    /// Editor tooltips
    /// =====================
    public static String getElementTooltip(JavafxTypes types, Element element) {
        if (element instanceof VariableElement) {
            String prefix = "<html>"; // NOI18N
            VariableElement var = (VariableElement) element;
            ElementKind kind = var.getKind();
            if (kind == ElementKind.FIELD) {
                prefix = prefix + var.getEnclosingElement() + '.'; // NOI18N
            }

            Symbol sym = (Symbol) element;
            Type type = sym.asType();

            StringBuilder s = new StringBuilder();
            s.append(prefix);
            s.append("<b>"); // NOI18N
            s.append(var);
            s.append("</b> : "); // NOI18N
            s.append(typeToString(types, type));

            return s.toString();
        }

        if (element instanceof TypeElement) {
            return element.toString(); // TODO use typeElementToString() ?
        }

        if (element instanceof ExecutableElement) {
            ExecutableElement var = (ExecutableElement) element;
            Symbol sym = (Symbol) element;
            Type type = sym.asType();
            
            StringBuilder s = new StringBuilder();
            s.append("<html>"); // NOI18N
            s.append(var.getEnclosingElement());
            s.append(".<b>"); // NOI18N
            s.append(sym.name);
            s.append("</b>"); // NOI18N
            s.append(methodToString(types, (MethodType) type));

            return s.toString();
        }

        return null;
    }

    private static String typeToString(JavafxTypes types, Type type) {
        String suffix = ""; // NOI18N
        if (type instanceof FunctionType) {
            MethodType mtype = ((FunctionType) type).asMethodType();
            return "function" + methodToString(types, mtype); // NOI18N
        }

        if (types.isSequence(type)) {
            suffix = "[ ]"; // NOI18N
            type = types.elementType(type);
        }
        switch (type.tag) {
            case TypeTags.DOUBLE:
                return "Number" + suffix; // NOI18N

            case TypeTags.INT:
                return "Integer" + suffix; // NOI18N

            case TypeTags.VOID:
                return "Void" + suffix; // NOI18N

            default:
                return type.toString() + suffix;
        }
    }

    private static String methodToString(JavafxTypes types, MethodType mtype) {
        StringBuilder s = new StringBuilder();
        s.append('('); // NOI18N
        if (mtype == null) {
            s.append("???"); // NOI18N
        } else {
            List<Type> args = mtype.argtypes;
            for (List<Type> l = args; l.nonEmpty(); l = l.tail) {
                if (l != args) {
                    s.append(", "); // NOI18N
                }
                s.append(':'); // NOI18N
                s.append(typeToString(types, l.head));
            }
        }
        s.append("):"); // NOI18N
        final Type restype = mtype.restype;
        if (mtype != null && TypeKind.VOID != mtype.getKind()) {
            s.append(mtype == null ? "???" : typeToString(types,restype)); // NOI18N
        }
        return s.toString();
    }

    /// Navigator
    /// =====================
    public static String executableElementToString(JavafxTypes types, ExecutableElement element, boolean isDeprecated, boolean isInherited) {
        StringBuilder sb = new StringBuilder();
        if (isDeprecated) {
            sb.append("<s>"); // NOI18N
        }
        if (isInherited) {
            sb.append("<font color="); // NOI18N
            sb.append(INHERITED_COLOR);
            sb.append('>'); // NOI18N
        }
        if (element.getKind() == ElementKind.CONSTRUCTOR) {
            sb.append(element.getEnclosingElement().getSimpleName());
        } else {
            sb.append(element.getSimpleName());
        }
        if (isDeprecated) {
            sb.append("</s>"); // NOI18N
        }

        sb.append('('); // NOI18N
        java.util.List<? extends VariableElement> params = element.getParameters();
        for (Iterator<? extends VariableElement> it = params.iterator(); it.hasNext();) {
            VariableElement param = it.next();
            sb.append(param.getSimpleName());
            sb.append(": "); // NOI18N     
            sb.append("<font color="); // NOI18N
            sb.append(TYPE_COLOR);
            sb.append('>'); // NOI18N
            sb.append(typeToString(types, (Type) param.asType()));
            sb.append("</font>"); // NOI18N
            if (it.hasNext()) {
                sb.append(", "); // NOI18N
            }
        }
        sb.append(')'); // NOI18N

        if (element.getKind() != ElementKind.CONSTRUCTOR) {
            TypeMirror rt = element.getReturnType();
            if (rt.getKind() != TypeKind.VOID) {
                sb.append(": "); // NOI18N     
                sb.append("<font color="); // NOI18N
                sb.append(TYPE_COLOR);
                sb.append('>'); // NOI18N
                sb.append(typeToString(types, (Type) element.getReturnType()));
                sb.append("</font>"); // NOI18N                    
            }
        }

        return sb.toString();
    }

    public static String variableElementToString(JavafxTypes types, VariableElement element, boolean isDeprecated, boolean isInherited) {
        StringBuilder sb = new StringBuilder();

        if (isDeprecated) {
            sb.append("<s>"); // NOI18N
        }
        if (isInherited) {
            sb.append("<font color="); // NOI18N
            sb.append(INHERITED_COLOR);
            sb.append('>'); // NOI18N
        }
        sb.append(element.getSimpleName());
        if (isDeprecated) {
            sb.append("</s>"); // NOI18N
        }

        sb.append(": "); // NOI18N
        sb.append("<font color="); // NOI18N
        sb.append(TYPE_COLOR);
        sb.append('>'); // NOI18N

        final Type type = (Type) element.asType();
        if (element instanceof ExecutableElement) {
//        if (type instanceof FunctionType) {

            Symbol.TypeSymbol sym = type.asElement();
            String name = sym.name.toString();

            sb.append("function "); // NOI18N
            sb.append(element.getSimpleName());
            sb.append('('); // NOI18N
            java.util.List<? extends VariableElement> params = ((ExecutableElement) element).getParameters();
            for (Iterator<? extends VariableElement> it = params.iterator(); it.hasNext();) {
                VariableElement param = it.next();
                sb.append(param.getSimpleName());
                sb.append(": "); // NOI18N
                sb.append("<font color="); // NOI18N
                sb.append(TYPE_COLOR);
                sb.append('>'); // NOI18N
                sb.append(typeToString(types, (Type) param.asType()));
                sb.append("</font>"); // NOI18N
                if (it.hasNext()) {
                    sb.append(", "); // NOI18N
                }
            }
            sb.append(')'); // NOI18N

            TypeMirror rt = ((ExecutableElement) element).getReturnType();
            if (rt.getKind() != TypeKind.VOID) {
                sb.append(": "); // NOI18N
                sb.append("<font color="); // NOI18N
                sb.append(TYPE_COLOR);
                sb.append('>'); // NOI18N
                sb.append(typeToString(types, (Type) ((ExecutableElement) element).getReturnType()));
                sb.append("</font>"); // NOI18N
            }
        } else {
            sb.append(typeToString(types, type));
        }
        sb.append("</font>"); // NOI18N

        return sb.toString();
    }

    public static String typeElementToString(JavafxTypes types, TypeElement element, boolean isDeprecated, boolean isInherited) {
        StringBuilder sb = new StringBuilder();
        if (isDeprecated) {
            sb.append("<s>"); // NOI18N
        }
        if (isInherited) {
            sb.append("<font color="); // NOI18N
            sb.append(INHERITED_COLOR);
            sb.append('>'); // NOI18N
        }
        sb.append(element.getSimpleName());
        if (isDeprecated) {
            sb.append("</s>"); // NOI18N
        }

        java.util.List<? extends TypeParameterElement> typeParams = element.getTypeParameters();
        if (typeParams != null && !typeParams.isEmpty()) {
            sb.append("&lt;"); // NOI18N

            for (Iterator<? extends TypeParameterElement> it = typeParams.iterator(); it.hasNext();) {
                TypeParameterElement tp = it.next();
                sb.append(tp.getSimpleName());
                try {
                    java.util.List<? extends TypeMirror> bounds = tp.getBounds();
                    if (!bounds.isEmpty()) {
                        sb.append(boundsToString(bounds, types));
                    }
                } catch (NullPointerException npe) {
                    npe.printStackTrace(); // TODO
                }
                if (it.hasNext()) {
                    sb.append(", "); // NOI18N
                }
            }

            sb.append("&gt;"); // NOI18N
        }

        // Add superclass and implemented interfaces
        TypeMirror sc = element.getSuperclass();
        String scName = typeToString(types, (Type) sc);

        if (sc == null || element.getKind() == ElementKind.ENUM || element.getKind() == ElementKind.ANNOTATION_TYPE ||
                "Object".equals(scName) || // NOI18N
                "<none>".equals(scName)) { // NOI18N
            scName = null;
        }

        java.util.List<? extends TypeMirror> ifaces = element.getInterfaces();
        if ((scName != null || !ifaces.isEmpty()) && element.getKind() != ElementKind.ANNOTATION_TYPE) {
            sb.append(" :: "); // NOI18N
            if (scName != null) {
                sb.append("<font color="); // NOI18N
                sb.append(TYPE_COLOR);
                sb.append('>'); // NOI18N
                sb.append(scName);
                sb.append("</font>"); // NOI18N
            }
            
            if (!ifaces.isEmpty()) {
                if (scName != null) {
                    sb.append(" : "); // NOI18N
                }
                
                for (Iterator<? extends TypeMirror> it = ifaces.iterator(); it.hasNext();) {
                    TypeMirror typeMirror = it.next();
                    sb.append("<font color="); // NOI18N
                    sb.append(TYPE_COLOR);
                    sb.append('>'); // NOI18N
                    sb.append(typeToString(types, (Type) typeMirror));
                    sb.append("</font>"); // NOI18N
                    if (it.hasNext()) {
                        sb.append(", "); // NOI18N
                    }
                }
            }
        }

        return sb.toString();
    }

    private static String boundsToString(java.util.List<? extends TypeMirror> bounds, JavafxTypes types) {
        if (bounds.size() == 1 && "java.lang.Object".equals(bounds.get(0).toString())) { // NOI18N
            return ""; // NOI18N
        }

        StringBuilder sb = new StringBuilder();
        sb.append(" extends "); // NOI18N
        for (Iterator<? extends TypeMirror> it = bounds.iterator(); it.hasNext();) {
            TypeMirror bound = it.next();
            sb.append(typeToString(types, (Type) bound));
            if (it.hasNext()) {
                sb.append(" & "); // NOI18N
            }
        }

        return sb.toString();
    }

    /// =====================
    public static int getSubstitutionLenght(final String text, final int offset, int length) {
        if (text == null) {
            return length;
        }
        int tlength = text.length();
        int index = offset + tlength;
        for (int i = 0; i < CODE_COMPL_SUBST_BREAKERS.length; i++) {
            int k = text.indexOf(CODE_COMPL_SUBST_BREAKERS[i], offset);
            if (k != -1 && k < index) {
                index = k;
            }
        }
        if (tlength < index) {
            index = tlength;
        }
        int ret = index - offset;
        if (length > ret) {
            ret = length;
        }
        return ret;
    }

    public static URL getJavadoc(final Element element, final ClasspathInfo cpInfo) {
        if (element == null || cpInfo == null) {
            throw new IllegalArgumentException("Cannot pass null as an argument of the FXSourceUtils.getJavadoc");  //NOI18N
        }

        ClassSymbol clsSym = null;
        String pkgName;
        String pageName;
        boolean buildFragment = false;
        if (element.getKind() == ElementKind.PACKAGE) {
            java.util.List<? extends Element> els = element.getEnclosedElements();
            for (Element e : els) {
//                if (e.getKind().isClass() || e.getKind().isInterface()) {
                if (e.getKind().isClass()) {
                    clsSym = (ClassSymbol) e;
                    break;
                }
            }
            if (clsSym == null) {
                return null;
            }
            pkgName = FileObjects.convertPackage2Folder(((PackageElement) element).getQualifiedName().toString());
            pageName = PACKAGE_SUMMARY;
        } else {
            Element prev = null;
            Element enclosing = element;
            while (enclosing.getKind() != ElementKind.PACKAGE) {
                prev = enclosing;
                enclosing = enclosing.getEnclosingElement();
            }
//            if (prev == null || (!prev.getKind().isClass() && !prev.getKind().isInterface())) {
            if (prev == null || !prev.getKind().isClass()) {
                return null;
            }
            clsSym = (ClassSymbol) prev;
            pkgName = FileObjects.convertPackage2Folder(clsSym.getEnclosingElement().getQualifiedName().toString());
            pageName = clsSym.getSimpleName().toString();
            buildFragment = element != prev;
        }

        if (clsSym.completer != null) {
            clsSym.complete();
        }

        URL sourceRoot = null;
        Set<URL> binaries = new HashSet<URL>();
        try {
            if (clsSym.classfile != null) {
                FileObject fo = URLMapper.findFileObject(clsSym.classfile.toUri().toURL());
                StringTokenizer tk = new StringTokenizer(pkgName, "/");             //NOI18N
                for (int i = 0; fo != null && i <= tk.countTokens(); i++) {
                    fo = fo.getParent();
                }
                if (fo != null) {
                    URL url = fo.getURL();
                    // TODO 
//                    sourceRoot = Index.getSourceRootForClassFolder(url);
                    if (sourceRoot == null) {
                        binaries.add(url);
                    } else {
                        // sourceRoot may be a class root in reality
                        binaries.add(sourceRoot);
                    }
                }
            }
            if (sourceRoot != null) {
                FileObject sourceFo = URLMapper.findFileObject(sourceRoot);
                if (sourceFo != null) {
                    ClassPath exec = ClassPath.getClassPath(sourceFo, ClassPath.EXECUTE);
                    ClassPath compile = ClassPath.getClassPath(sourceFo, ClassPath.COMPILE);
                    ClassPath source = ClassPath.getClassPath(sourceFo, ClassPath.SOURCE);
                    if (exec == null) {
                        exec = compile;
                        compile = null;
                    }
                    if (exec != null && source != null) {
                        Set<URL> roots = new HashSet<URL>();
                        for (ClassPath.Entry e : exec.entries()) {
                            roots.add(e.getURL());
                        }
                        if (compile != null) {
                            for (ClassPath.Entry e : compile.entries()) {
                                roots.remove(e.getURL());
                            }
                        }
                        java.util.List<FileObject> sourceRoots = Arrays.asList(source.getRoots());
                        out:
                        for (URL e : roots) {
                            FileObject[] res = SourceForBinaryQuery.findSourceRoots(e).getRoots();
                            for (FileObject fo : res) {
                                if (sourceRoots.contains(fo)) {
                                    binaries.add(e);
                                    continue out;
                                }
                            }
                        }
                    }
                }
            }
            for (URL binary : binaries) {
                URL[] result = JavadocForBinaryQuery.findJavadoc(binary).getRoots();
                ClassPath cp = ClassPathSupport.createClassPath(result);
                FileObject fo = cp.findResource(pkgName);
                if (fo != null) {
                    for (FileObject child : fo.getChildren()) {
                        if (pageName.equals(child.getName()) && FileObjects.HTML.equalsIgnoreCase(child.getExt())) {
                            URL url = child.getURL();
                            CharSequence fragment = null;
                            if (url != null && buildFragment) {
                                fragment = getFragment(element);
                            }
                            if (fragment != null && fragment.length() > 0) {
                                try {
                                    // Javadoc fragments may contain chars that must be escaped to comply with RFC 2396.
                                    // Unfortunately URLEncoder escapes almost everything but
                                    // spaces replaces with '+' char which is wrong so it is
                                    // replaced with "%20"escape sequence here.
                                    String encodedfragment = URLEncoder.encode(fragment.toString(), "UTF-8"); // NOI18N
                                    encodedfragment = encodedfragment.replace("+", "%20"); // NOI18N
                                    return new URI(url.toExternalForm() + '#' + encodedfragment).toURL();
                                } catch (URISyntaxException ex) {
                                    Exceptions.printStackTrace(ex);
                                } catch (UnsupportedEncodingException ex) {
                                    Exceptions.printStackTrace(ex);
                                } catch (MalformedURLException ex) {
                                    Exceptions.printStackTrace(ex);
                                }
                            }
                            return url;
                        }
                    }
                }
            }

        } catch (MalformedURLException e) {
            Exceptions.printStackTrace(e);
        } catch (FileStateInvalidException e) {
            Exceptions.printStackTrace(e);
        }
        return null;
    }

    private static CharSequence getFragment(Element e) {
        StringBuilder sb = new StringBuilder();
        if (!e.getKind().isClass() && !e.getKind().isInterface()) {
            if (e.getKind() == ElementKind.CONSTRUCTOR) {
                sb.append(e.getEnclosingElement().getSimpleName());
            } else {
                sb.append(e.getSimpleName());
            }
            if (e.getKind() == ElementKind.METHOD || e.getKind() == ElementKind.CONSTRUCTOR) {
                ExecutableElement ee = (ExecutableElement) e;
                sb.append('('); //NOI18N
                for (Iterator<? extends VariableElement> it = ee.getParameters().iterator(); it.hasNext();) {
                    VariableElement param = it.next();
                    appendType(sb, param.asType(), ee.isVarArgs() && !it.hasNext());
                    if (it.hasNext()) {
                        sb.append(", "); //NOI18N
                    }
                }
                sb.append(')'); //NOI18N
            }
        }
        return sb;
    }

    private static void appendType(StringBuilder sb, TypeMirror type, boolean varArg) {
        switch (type.getKind()) {
            case ARRAY:
                appendType(sb, ((ArrayType) type).getComponentType(), false);
                sb.append(varArg ? "..." : "[]"); //NOI18N
                break;
            case DECLARED:
                sb.append(((TypeElement) ((DeclaredType) type).asElement()).getQualifiedName());
                break;
            default:
                sb.append(type);
        }
    }
}
