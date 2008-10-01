/*
 * Copyright (c) 2008, Your Corporation. All Rights Reserved.
 */

package org.netbeans.api.javafx.source;

import org.netbeans.api.java.classpath.ClassPath;

import javax.lang.model.element.TypeElement;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Set;

/**
 * A JavaFX class index that, for given class path, provide searching services
 * over both java and javafx classes.
 * @author nenik
 */
public class ClassIndex {
    //INV: Never null
    private final ClassPath bootPath;
    //INV: Never null
    private final ClassPath classPath;
    //INV: Never null
    private final ClassPath sourcePath;

    private final org.netbeans.api.java.source.ClassIndex javaIndex;
    
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
        this.bootPath = bootPath;
        this.classPath = classPath;
        this.sourcePath = sourcePath;
        
        javaIndex = org.netbeans.api.java.source.ClasspathInfo.create(bootPath, classPath, sourcePath).getClassIndex();
/*        this.oldBoot = new HashSet<URL>();
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

        // get the partial result from java support:
        Set<?> javaEl = javaIndex.getDeclaredTypes(name, toJava(kind), toJava(scope));
        for (Object o : javaEl) {
            result.add(ElementHandle.fromJava((org.netbeans.api.java.source.ElementHandle)o));
        }
        
        // and TODO add our data:
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

        // get the partial result from java support:
        result.addAll(javaIndex.getPackageNames(prefix, directOnly, toJava(scope)));

        // and TODO add our data:
        
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
    
    private static Set<org.netbeans.api.java.source.ClassIndex.SearchScope> toJava(Set<SearchScope> scopes) {
        Set<org.netbeans.api.java.source.ClassIndex.SearchScope> cScopes = EnumSet.noneOf(org.netbeans.api.java.source.ClassIndex.SearchScope.class);
        for (SearchScope scope : scopes) cScopes.add(org.netbeans.api.java.source.ClassIndex.SearchScope.valueOf(scope.toString()));
        return cScopes;
    }

    private org.netbeans.api.java.source.ClassIndex.NameKind toJava(NameKind kind) {
        return org.netbeans.api.java.source.ClassIndex.NameKind.valueOf(kind.name());
    }

}
