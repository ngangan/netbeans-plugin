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

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.logging.Logger;
import javax.lang.model.element.TypeElement;
import javax.swing.Icon;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.javafx.editor.ElementOpen;
import org.netbeans.api.javafx.editor.FXSourceUtils;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.api.javafx.source.JavaFXSourceUtils;
import org.netbeans.api.project.FileOwnerQuery;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectInformation;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.api.project.ui.OpenProjects;
import org.netbeans.modules.javafx.project.JavaFXProject;
import org.netbeans.spi.jumpto.type.SearchType;
import org.netbeans.spi.jumpto.type.TypeDescriptor;
import org.netbeans.spi.jumpto.type.TypeProvider;
import org.openide.filesystems.FileObject;



/**
 *
 * @author nenik
 */
public class JavaFXTypeProvider implements TypeProvider {
    private static final Logger LOGGER = Logger.getLogger(JavaFXTypeProvider.class.getName());
    private static final ClassPath EMPTY_CLASSPATH = org.netbeans.spi.java.classpath.support.ClassPathSupport.createClassPath( new FileObject[0] );
    private Set<CacheItem> cache;
    private volatile boolean isCanceled = false;

    public String name() {
        return "javafx"; // system name: NOI18N
    }

    public String getDisplayName() {
        return "JavaFX Types"; // XXX internationalization
    }

    public void cancel() {
        isCanceled = true;
    }

    public void cleanup() {
        isCanceled = false;
        cache = null;
    }

    
    public void computeTypeNames(TypeProvider.Context context, TypeProvider.Result res) {
        isCanceled = false;
        String text = context.getText();
        SearchType searchType = context.getSearchType();
        
//        boolean hasBinaryOpen = Lookup.getDefault().lookup(BinaryElementOpen.class) != null;
        final ClassIndex.NameKind nameKind;
        switch (searchType) {
            case EXACT_NAME: nameKind = ClassIndex.NameKind.SIMPLE_NAME; break;
            case CASE_INSENSITIVE_EXACT_NAME: nameKind = ClassIndex.NameKind.CASE_INSENSITIVE_REGEXP; break;
            case PREFIX: nameKind = ClassIndex.NameKind.PREFIX; break;
            case CASE_INSENSITIVE_PREFIX: nameKind = ClassIndex.NameKind.CASE_INSENSITIVE_PREFIX; break;
            case REGEXP: nameKind = ClassIndex.NameKind.REGEXP; break;
            case CASE_INSENSITIVE_REGEXP: nameKind = ClassIndex.NameKind.CASE_INSENSITIVE_REGEXP; break;
            case CAMEL_CASE: nameKind = ClassIndex.NameKind.CAMEL_CASE; break;
            default: throw new RuntimeException("Unexpected search type: " + searchType); // NOI18N
        }
        
        long time = 0;

        long cp, gss, gsb, sfb, gtn, add, sort;
        cp = gss = gsb = sfb = gtn = add = sort = 0;

        Future<Project[]> openProjectsTask = OpenProjects.getDefault().openProjects();
        Project[] projs = new Project[0];
        try {
            projs = openProjectsTask.get();
        } catch (InterruptedException ex) {
            LOGGER.fine(ex.getMessage());
        } catch (ExecutionException ex) {
            LOGGER.fine(ex.getMessage());
        }
        
        if (cache == null) {
            Set<CacheItem> sources = new HashSet<CacheItem>();
            Set<FileObject> roots = new HashSet<FileObject>();
            for (Project project : projs) {
                if (! (project instanceof JavaFXProject)) continue;
                JavaFXProject jfxp = (JavaFXProject)project;
                ClassPath pcp = jfxp.getClassPathProvider().getProjectSourcesClassPath(ClassPath.SOURCE);

                for (FileObject root : pcp.getRoots()) {
                    if (roots.add(root)) {
                        ClassPath src = org.netbeans.spi.java.classpath.support.ClassPathSupport.createClassPath(root);
                        ClasspathInfo ci = ClasspathInfo.create(EMPTY_CLASSPATH, EMPTY_CLASSPATH, src);
                        if ( isCanceled ) return;

                        sources.add(new CacheItem(root, ci, false));
                    }
                }
            }
                
            if (isCanceled) return;
            cache = sources;
        }

        ArrayList<JavaFXTypeDescription> types = new ArrayList<JavaFXTypeDescription>();
        
        res.setMessage(null); // no startup scanning yet.

        final String textForQuery;
        switch( nameKind ) {
            case REGEXP:
            case CASE_INSENSITIVE_REGEXP:
//                text = removeNonJavaChars(text);
                String pattern = searchType == SearchType.CASE_INSENSITIVE_EXACT_NAME ? text : text + "*"; // NOI18N
                pattern = pattern.replace( "*", ".*" ).replace( '?', '.' );
                textForQuery = pattern;
                break;
            default:
                textForQuery = text;
        }
        LOGGER.fine("Text For Query '" + textForQuery + "'.");

        for(final CacheItem ci : cache) {
            @SuppressWarnings("unchecked")
            final Set<ElementHandle<TypeElement>> names = ci.classpathInfo.getClassIndex().getDeclaredTypes(
                        textForQuery, nameKind, EnumSet.of(ci.isBinary ? ClassIndex.SearchScope.DEPENDENCIES : ClassIndex.SearchScope.SOURCE)
                    );
            for (ElementHandle<TypeElement> name : names) {
                JavaFXTypeDescription td = new JavaFXTypeDescription(ci, name);
                types.add(td);
                if (isCanceled) {
                    return;
                }
            }
        }

        if ( isCanceled ) return;

        res.addResult(types);
    }
    
    class JavaFXTypeDescription extends TypeDescriptor {
        private Icon icon;

        private final CacheItem cacheItem;

        private final ElementHandle<TypeElement> handle;
        private String simpleName;
        private String outerName;
        private String packageName;

        public JavaFXTypeDescription(CacheItem cacheItem, final ElementHandle<TypeElement> handle ) {
            this.cacheItem = cacheItem;
            this.handle = handle; 
            init();
        }

        private void init() {
            final String typeName = this.handle.getSignatures()[0]; //getBinaryName();
            int lastDot = typeName.lastIndexOf('.'); // NOI18N
            int lastDollar = typeName.lastIndexOf('$'); // NOI18N
            if ( lastDot == -1 ) {
                if ( lastDollar == -1 ) {
                    simpleName = typeName;
                }
                else {
                    simpleName = typeName.substring(lastDollar + 1);
                    outerName = typeName.substring(0, lastDollar ).replace( '$', '.');  //NOI18N;
                }
            }
            else {
                packageName = typeName.substring( 0, lastDot );

                if ( lastDollar == -1 ) {
                    simpleName = typeName.substring( lastDot + 1 ).replace( '$', '.');  //NOI18N
                }
                else {
                    simpleName = typeName.substring(lastDollar + 1);
                    outerName = typeName.substring(lastDot + 1, lastDollar ).replace( '$', '.');  //NOI18N;
                }

            }
            icon = null;//Icons.getElementIcon (handle.getKind(), null);
        }

        @Override
        public String getSimpleName() {
            return simpleName;
        }

        @Override
        public String getOuterName() {
            return outerName;
        }

        @Override
        public String getTypeName() {
            StringBuilder sb = new StringBuilder( simpleName );
            if( outerName != null  ) {
                sb.append(" in ").append( outerName );
            }
            return sb.toString();
        }

        @Override
        public String getContextName() {
            StringBuilder sb = new StringBuilder();
            sb.append( " (").append( packageName == null ? "Default Package" : packageName).append(")");
            return sb.toString();
        }

        @Override
        public Icon getIcon() {
            return icon;
        }

        @Override
        public String getProjectName() {
            String projectName = cacheItem.getProjectName();
            return projectName == null ? "" : projectName; // NOI18N
        }

        @Override
        public Icon getProjectIcon() {
            return cacheItem.getProjectIcon();
        }

        @Override
        public FileObject getFileObject() {
            return cacheItem.getRoot();
        }

        @Override
        public int getOffset() {
            throw new UnsupportedOperationException("Not supported yet.");
        }

        @Override
        public void open() {
            ClasspathInfo ci = ClasspathInfo.create(cacheItem.getRoot());
            FileObject file = JavaFXSourceUtils.getFile(handle, ci);
            try {
                ElementOpen.open(file, handle);
            } catch (Exception exception) {
                LOGGER.fine(exception.getMessage());
                System.err.println("Exc: " + exception);
            }
        }
        
    }

   static class CacheItem {
        public final boolean isBinary;
        public final FileObject fileObject;
        public final ClasspathInfo classpathInfo;
        public String projectName;
        public Icon projectIcon;
        private ClassPath.Entry defEntry;
                
        public CacheItem ( FileObject fileObject, ClasspathInfo classpathInfo, boolean isBinary ) {
            this.isBinary = isBinary;
            this.fileObject = fileObject;
            this.classpathInfo = classpathInfo;
        }
        
        @Override
        public int hashCode () {
            return this.fileObject == null ? 0 : this.fileObject.hashCode();
        }
        
        @Override
        public boolean equals (Object other) {
            if (other instanceof CacheItem) {
                CacheItem otherItem = (CacheItem) other;
                return this.fileObject == null ? otherItem.fileObject == null : this.fileObject.equals(otherItem.fileObject);
            }
            return false;
        }
    
        public FileObject getRoot() {
            return fileObject;
        }
        
        public boolean isBinary() {
            return isBinary;
        }
        
        public synchronized String getProjectName() {
            if ( !isBinary && projectName == null) {
                initProjectInfo();
            }
            return projectName;
        }
        
        public synchronized Icon getProjectIcon() {
            if ( !isBinary && projectIcon == null ) {
                initProjectInfo();
            }
            return projectIcon;
        }
        
        private void initProjectInfo() {
            Project p = FileOwnerQuery.getOwner(fileObject);                    
            if (p != null) {
                ProjectInformation pi = ProjectUtils.getInformation( p );
                projectName = pi.getDisplayName();
                projectIcon = pi.getIcon();
            }
        }
        
    }

}
