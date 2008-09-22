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

package org.netbeans.api.javafx.source;

import java.util.HashSet;
import java.util.Set;
import javax.lang.model.element.TypeElement;
import org.netbeans.api.java.classpath.ClassPath;

/**
 * A JavaFX class index that, for given class path, provide searching services
 * over both java and javafx classes.
 * @author nenik
 */
public class ClassIndex {

    /**
     * Encodes a type of the name kind used by 
     * {@link ClassIndex#getDeclaredTypes} method.
     *
     */
    public enum NameKind {
        /**
         * The name parameter of the {@link ClassIndex#getDeclaredTypes}
         * is an exact simple name of the package or declared type.
         */
        SIMPLE_NAME,
        
        /**
         * The name parameter of the {@link ClassIndex#getDeclaredTypes} 
         * is an case sensitive prefix of the package or declared type name.
         */
        PREFIX,
        
        /**
         * The name parameter of the {@link ClassIndex#getDeclaredTypes} is 
         * an case insensitive prefix of the declared type name.
         */
        CASE_INSENSITIVE_PREFIX,
        
        /**
         * The name parameter of the {@link ClassIndex#getDeclaredTypes} is 
         * an camel case of the declared type name.
         */
        CAMEL_CASE,
        
        
        /**
         * The name parameter of the {@link ClassIndex#getDeclaredTypes} is 
         * an regular expression of the declared type name.
         */
        REGEXP,
        
        /**
         * The name parameter of the {@link ClassIndex#getDeclaredTypes} is 
         * an case insensitive regular expression of the declared type name.
         */
        CASE_INSENSITIVE_REGEXP,
        
        /**
         * The name parameter of the {@link ClassIndex#getDeclaredTypes} is 
         * a camel case or case insensitive prefix of the declared type name.
         * For example all these names NPE, NulPoEx, NULLPOInter leads to NullPointerException returned.
         * @since 0.28.0
         */
        CAMEL_CASE_INSENSITIVE
    };

    /**
     * Scope used by {@link ClassIndex} to search in
     */
    public enum SearchScope {
        /**
         * Search is done in source path
         */
        SOURCE,
        /**
         * Search is done in compile and boot path
         */
        DEPENDENCIES
    };

    ClassIndex(final ClassPath bootPath, final ClassPath classPath, final ClassPath sourcePath) {
        assert bootPath != null;
        assert classPath != null;
        assert sourcePath != null;
/*        this.bootPath = bootPath;
        this.classPath = classPath;
        this.sourcePath = sourcePath;
        this.oldBoot = new HashSet<URL>();
        this.oldCompile = new  HashSet<URL>();
        this.oldSources = new HashSet<URL>();
        this.depsIndeces = new HashSet<ClassIndexImpl>();
        this.sourceIndeces = new HashSet<ClassIndexImpl>();
        
        final ClassIndexManager manager = ClassIndexManager.getDefault();
        manager.addClassIndexManagerListener(WeakListeners.create(ClassIndexManagerListener.class, (ClassIndexManagerListener) this.spiListener, manager));
        this.bootPath.addPropertyChangeListener(WeakListeners.propertyChange(spiListener, this.bootPath));
        this.classPath.addPropertyChangeListener(WeakListeners.propertyChange(spiListener, this.classPath));
        this.sourcePath.addPropertyChangeListener(WeakListeners.propertyChange(spiListener, this.sourcePath));                
        reset (true, true);	    */
    }

    /**
     * Returns {@link ElementHandle}s for all declared types in given classpath corresponding to the name.
     * All the types belonging to the JDK, JavaFX SDK and user classes are
     * considered, but even java types are returned in the form of JavaFX
     * ElementHandle.
     * 
     * @param name case sensitive prefix, case insensitive prefix, exact simple name,
     * camel case or regular expression depending on the kind parameter.
     * @param kind of the name {@see NameKind}
     * @param scope to search in {@see SearchScope}
     * @return set of all matched declared types
     * It may return null when the caller is a CancellableTask&lt;CompilationInfo&gt; and is cancelled
     * inside call of this method.
     */
    public Set<ElementHandle<TypeElement>> getDeclaredTypes (final String name, final NameKind kind, final Set<SearchScope> scope) {
        assert name != null;
        assert kind != null;
        final Set<ElementHandle<TypeElement>> result = new HashSet<ElementHandle<TypeElement>>();        
//        final Iterable<? extends ClassIndexImpl> queries = this.getQueries (scope);        
//        final ResultConvertor<ElementHandle<TypeElement>> thConvertor = ResultConvertor.elementHandleConvertor();
//        try {
//            for (ClassIndexImpl query : queries) {
//                query.getDeclaredTypes (name, kind, thConvertor, result);
//            }
//            LOGGER.fine(String.format("ClassIndex.getDeclaredTypes returned %d elements\n", result.size()));
//            return Collections.unmodifiableSet(result);
//        } catch (InterruptedException e) {
//            return null;
//        }
        return result;
    }

    /**
     * Returns names af all packages in given classpath starting with prefix.
     * @param prefix of the package name
     * @param directOnly if true treats the packages as folders and returns only
     * the nearest component of the package.
     * @param scope to search in {@see SearchScope}
     * @return set of all matched package names
     * It may return null when the caller is a CancellableTask&lt;CompilationInfo&gt; and is cancelled
     * inside call of this method.
     */
    public Set<String> getPackageNames (final String prefix, boolean directOnly, final Set<SearchScope> scope) {
        assert prefix != null;
        final Set<String> result = new HashSet<String> ();        
//        final Iterable<? extends ClassIndexImpl> queries = this.getQueries (scope);
//        try {
//            for (ClassIndexImpl query : queries) {
//                query.getPackageNames (prefix, directOnly, result);
//            }
//            return Collections.unmodifiableSet(result);
//        } catch (InterruptedException e) {
//            return null;
//        }
        return result;
    }

}
