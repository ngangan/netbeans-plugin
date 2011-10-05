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

package org.netbeans.api.visage.source;

import java.io.*;
import java.util.*;
import java.util.HashMap;
import java.util.Set;
import javax.tools.*;
import javax.tools.JavaFileObject.Kind;
import java.net.URI;
/*import org.netbeans.api.project.Project;
import org.netbeans.modules.visage.project.VisageProject;
import org.netbeans.spi.project.support.ant.PropertyEvaluator;*/
import java.nio.CharBuffer;
import javax.lang.model.element.NestingKind;

public class MemoryFileManager implements JavaFileManager {
    
    List<SimpleJavaFileObject> buffers = new ArrayList<SimpleJavaFileObject>();
    public ClassLoader getClassLoader(Location location) {
        throw new UnsupportedOperationException("Not supported yet."); // NOI18N
    }

    public Iterable<JavaFileObject> list(Location location, String packageName, Set<Kind> kinds, boolean recurse) throws IOException {
        ArrayList<JavaFileObject> results = new ArrayList<JavaFileObject>();
        
	/*List<JavaFileObject> results = new LinkedList<JavaFileObject>();
	for (JavaFileObject o : result) {
	    results.add(o);
	}*/
        
	/*String prefix = packageName.equals("") ? "" : packageName + ".";
	for (SimpleJavaFileObject b : buffers) {
	    String name = b.getName().replace("/", ".");
	    name = name.substring(1, name.length() - (name.endsWith(EXT) ? EXT.length() : 0));
	    if (prefix.length() == 0) {
		if (!name.contains(".")) {
		    results.add(b);
		}
	    } else {
		if (name.startsWith(prefix)) {
		    name = name.substring(prefix.length());
		    if (!name.contains(".")) {
			results.add(b);
		    }
		}
	    }
	}*/
	return results;
    }

    public String inferBinaryName(Location location, JavaFileObject file) {
        if (file instanceof StringInputBuffer) {
	    return ((StringInputBuffer)file).getBinaryName();
	} else if (file instanceof ClassOutputBuffer) {
	    return ((ClassOutputBuffer)file).getBinaryName();
	} else if (file instanceof ClassInputBuffer) {
	    return ((ClassInputBuffer)file).getBinaryName();
        }
        return null;
    }
    
    private class ClassInputBuffer extends SimpleJavaFileObject {
        private String name;
        byte buf[];

        ClassInputBuffer(String name, byte[] buf) { 
            super(toURI(name), Kind.CLASS);
            this.name = name;
            this.buf = buf;
        }

	public String getBinaryName() {
	    return name;
	}

        @Override
        public InputStream openInputStream() {
            return new ByteArrayInputStream(buf);
        }
    }
    
    private static class StringInputBuffer extends SimpleJavaFileObject {
        final String code;
        final boolean isFXSourceFile;
	String binaryName;
        
	public String getBinaryName() {
	    return binaryName.equals("__VISAGE_SCRIPT__.visage") ? "__VISAGE_SCRIPT__" : binaryName; // NOI18N
	}

        StringInputBuffer(String name, String code) {
            super(toURI(name), Kind.SOURCE);
            this.code = code;
	    binaryName = name;
            isFXSourceFile = name.endsWith(".visage"); // NOI18N
        }
        
        @Override
        public CharBuffer getCharContent(boolean ignoreEncodingErrors) {
            return CharBuffer.wrap(code);
        }

        public Reader openReader() {
            return new StringReader(code);
        }

        @Override
        public Kind getKind() {
            //return isFXSourceFile ? JavaFileObject.Kind.SOURCE : super.getKind();
	    return JavaFileObject.Kind.SOURCE;
        }

        @Override
        public String getName() {
            return super.getName();
        }

        @Override
        public NestingKind getNestingKind() {
            return super.getNestingKind();
        }

        @Override
        public boolean isNameCompatible(String simpleName, Kind kind) {
            return super.isNameCompatible(simpleName, kind);
        }

        @Override
        public InputStream openInputStream() throws IOException {
            return super.openInputStream();
        }

        @Override
        public OutputStream openOutputStream() throws IOException {
            return super.openOutputStream();
        }

        @Override
        public Reader openReader(boolean ignoreEncodingErrors) throws IOException {
            return super.openReader(ignoreEncodingErrors);
        }

        @Override
        public Writer openWriter() throws IOException {
            return super.openWriter();
        }
    }

    public boolean isSameFile(javax.tools.FileObject a, javax.tools.FileObject b) {
        throw new UnsupportedOperationException("Not supported yet."); // NOI18N
    }

    public boolean handleOption(String current, Iterator<String> remaining) {
        throw new UnsupportedOperationException("Not supported yet."); // NOI18N
    }

    public boolean hasLocation(Location location) {
        throw new UnsupportedOperationException("Not supported yet."); // NOI18N
    }

    public JavaFileObject getJavaFileForInput(Location location, String className, Kind kind) throws IOException {
        throw new UnsupportedOperationException("Not supported yet."); // NOI18N
    }

    public JavaFileObject getJavaFileForOutput(JavaFileManager.Location location,
                                    String className,
                                    Kind kind,
                                    FileObject sibling) throws IOException {
        ClassOutputBuffer buf = new ClassOutputBuffer(className);
        buffers.add(buf);
        return buf;
    }

    public javax.tools.FileObject getFileForInput(Location location, String packageName, String relativeName) throws IOException {
        throw new UnsupportedOperationException("Not supported yet."); // NOI18N
    }

    public javax.tools.FileObject getFileForOutput(Location location, String packageName, String relativeName, javax.tools.FileObject sibling) throws IOException {
        throw new UnsupportedOperationException("Not supported yet."); // NOI18N
    }

    public void flush() throws IOException {
    }

    public void close() throws IOException {
        throw new UnsupportedOperationException("Not supported yet."); // NOI18N
    }

    public int isSupportedOption(String option) {
        throw new UnsupportedOperationException("Not supported yet."); // NOI18N
    }
    
    private final static String EXT = ".visage"; // NOI18N
    static URI toURI(String name) {
        File file = new File(name);
        if (file.exists()) {
            return file.toURI();
        } else {
            try {
                final StringBuilder newUri = new StringBuilder();
                newUri.append("mfm:///"); // NOI18N
                newUri.append(name.replace('.', '/')); // NOI18N
                if(name.endsWith(EXT)) newUri.replace(newUri.length() - EXT.length(), newUri.length(), EXT);
                return URI.create(newUri.toString());
            } catch (Exception exp) {
                return URI.create("mfm:///org/visage/tools/script/visage_source"); // NOI18N
            }
        }
    }
}
