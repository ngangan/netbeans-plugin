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
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2007 Sun
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

package org.netbeans.modules.visage.source.classpath;


import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.NestingKind;
import javax.tools.JavaFileObject;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.NbBundle;
import org.openide.util.Utilities;

/** Creates various kinds of file objects 
 *
 * XXX - Rename to JavaFileObjects
 *
 * @author Petr Hrebejk
 */
public class FileObjects {
    
/*    public static final Comparator<String> SIMPLE_NAME_STRING_COMPARATOR = new SimpleNameStringComparator();
    public static final Comparator<JavaFileObject> SIMPLE_NAME_FILEOBJECT_COMPARATOR = new SimpleNameFileObjectComparator();
  */  
    
    public static final String VISAGE  = "visage"; //VisageDataLoader.FX_EXTENSION; // NOI18N
    public static final String JAVA  = "java"; // NOI18N
    public static final String CLASS = "class"; //ClassDataLoader.CLASS_EXTENSION; // NOI18N
    public static final String JAR   = "jar";  //NOI18N
    public static final String FILE  = "file"; //NOI18N
    public static final String ZIP   = "zip";  //NOI18N
    public static final String HTML  = "html"; //NOI18N
    public static final String SIG   = "sig";  //NOI18N
    public static final String RS    = "rs";   //NOI18N
    
    
    /** Creates a new instance of FileObjects */
    private FileObjects() {
    }
    
    // Public methods ----------------------------------------------------------
    
    
    
    /**
     * Creates {@link JavaFileObject} for a file inside an archive file. The archive file
     * is opened every time an input stream of this {@link JavaFileObject} is needed, it may
     * slow down the javac attribution.
     * @param zip an archive file
     * @param folder in the archive
     * @param name the base (simple name)
     * @return {@link JavaFileObject}, never returns null
     */
    public static JavaFileObject zipFileObject( File zipFile, String folder, String baseName, long mtime) {
        assert zipFile != null;                
        return new ZipFileObject( zipFile, folder, baseName, mtime);
    }
    
    /**
     * Creates {@link JavaFileObject} for a file inside an {@link ZipFile}. The returned {@link JavaFileObject}
     * uses an opened ZipFile. It's a fastes way to read the archive file content, but the opened {@link ZipFile}s
     * cannot be modified. So, this {@link JavaFileObject}s can be used only for platform classpath.
     * @param zip an archive file
     * @param folder in the archive
     * @param name the base (simple name)
     * @param offset the start of zip entry in the zip file
     * @return {@link JavaFileObject}, never returns null
     */
    public static JavaFileObject zipFileObject(ZipFile zipFile, String folder, String baseName, long mtime) {
        assert zipFile != null;
        return new CachedZipFileObject (zipFile, folder, baseName, mtime);
    }
    
    /**
     * Creates {@link JavaFileObject} for a regular {@link File}
     * @param file for which the {@link JavaFileObject} should be created
     * @pram root - the classpath root owning the file
     * @return {@link JavaFileObject}, never returns null
     */
    public static JavaFileObject fileFileObject( final File file, final File root) {
        return fileFileObject(file, root, null);
    }
    
    /**
     * Creates {@link JavaFileObject} for a regular {@link File}
     * @param file for which the {@link JavaFileObject} should be created
     * @param root - the classpath root owning the file
     * @param encoding - the file's encoding
     * @return {@link JavaFileObject}, never returns null
     */
    public static JavaFileObject fileFileObject( final File file, final File root, Charset encoding) {
        assert file != null;
        assert root != null;
        String[] pkgNamePair = getFolderAndBaseName(getRelativePath(root,file),File.separatorChar);
        return new RegularFileObject( file, convertFolder2Package(pkgNamePair[0], File.separatorChar), pkgNamePair[1], encoding);
    }
                
    /**
     * Creates {@link JavaFileObject} for a NetBeans {@link FileObject}
     * Any client which needs to create {@link JavaFileObject} for java
     * source file should use this factory method.
     * @param {@link FileObject} for which the {@link JavaFileObject} should be created
     * @param {@link FileObject} root owning the file
     * @return {@link JavaFileObject}, never returns null
     * @exception {@link IOException} may be thrown
     */
    public static SourceFileObject nbFileObject (final FileObject file, final FileObject root) throws IOException {
        return nbFileObject (file, root, false);
    }
    
    /**
     * Creates {@link JavaFileObject} for a NetBeans {@link FileObject}
     * Any client which needs to create {@link JavaFileObject} for java
     * source file should use this factory method.
     * @param {@link FileObject} for which the {@link JavaFileObject} should be created
     * @param {@link FileObject} root owning the file
     * @param renderNow if true the snap shot of the file is taken immediately
     * @return {@link JavaFileObject}, never returns null
     * @exception {@link IOException} may be thrown
     */
    public static SourceFileObject nbFileObject (final FileObject file, final FileObject root, boolean renderNow) throws IOException {
        assert file != null;
        if (!file.isValid() || file.isVirtual()) {
            throw new InvalidFileException (file);
        }
        return new SourceFileObject (file, root/*, renderNow*/); // XXX
    }
        
    public static String stripExtension( String fileName ) {        
        int dot = fileName.lastIndexOf("."); // NOI18N
        return (dot == -1 ? fileName : fileName.substring(0, dot));
    }    
    
    public static String getExtension (final String fileName) {
        int dot = fileName.lastIndexOf('.');
        return (dot == -1 || dot == fileName.length() -1 ) ? "" : fileName.substring(dot+1);    //NOI18N
    }
    
    
    /**
     * Returns the name of JavaFileObject, similar to
     * {@link java.io.File#getName}
     */
    public static String getName (final JavaFileObject fo, final boolean noExt) {
        assert fo != null;
        if (fo instanceof Base) {
            Base baseFileObject = (Base) fo;
            if (noExt) {
                return baseFileObject.getName();
            }
            else {                
                StringBuilder sb = new StringBuilder ();
                sb.append (baseFileObject.getName());
                sb.append('.'); //NOI18N
                sb.append(baseFileObject.getExt());
                return sb.toString();
            }
        }
        try {
            final URL url = fo.toUri().toURL();
            String path = url.getPath();
            int index1 = path.lastIndexOf('/'); // NOI18N
            int len;
            if (noExt) {
               final int index2 = path.lastIndexOf('.'); // NOI18N
               if (index2>index1) {
                   len = index2;
               }
               else {
                   len = path.length();
               }
            }
            else {
                len = path.length();
            }
            path = path.substring(index1+1,len);
            return path;
        } catch (MalformedURLException e) {
            return null;
        }        
    }
        
    
    /**
     * Returns the basename name without folder path
     *  @param file name, eg. obtained from {@link FileObjects#getPath} or {java.io.File.getPath}
     *  @return the base name
     *  @see #getBaseName(String,char)
     */
    public static String getBaseName( String fileName ) {
        return getBaseName(fileName, File.separatorChar);
    }
    
    /**
     * Returns the basename name without folder path. You can specify
     * the path separator since eg zip files uses '/' regardless of platform.
     *  @param file name, eg. obtained from {@link FileObjects#getPath} or {java.io.File.getPath}
     *  @param separator path separator
     *  @return the base name
     */
    public static String getBaseName( String fileName, char separator ) {
        return getFolderAndBaseName(fileName, separator)[1];
    }
    
    
    /**
     *Returns the folder (package name separated by original separators)
     *and base name.
     * @param path
     * @return array of 2 strings, 1st the folder 2nd the base name
     */
    public static String[] getFolderAndBaseName (final String fileName, final char separator) {
        final int i = fileName.lastIndexOf( separator );
        if ( i == -1 ) {
            return new String[] {"",fileName};  //NOI18N
        }
        else {
            return new String[] {
                fileName.substring(0,i),
                fileName.substring( i + 1 )
            };
        }
    }
                
    public static String getBinaryName (final File file, final File root) {
        assert file != null && root != null;
        String fileName = FileObjects.getRelativePath (root, file);
        int index = fileName.lastIndexOf('.');  //NOI18N
        if (index > 0) {
            fileName = fileName.substring(0,index);
        }        
        return fileName.replace(File.separatorChar,'.');   //NOI18N        
    }
    
    public static String getSimpleName( JavaFileObject fo ) {
        
        String name = getName(fo,true);
        int i = name.lastIndexOf( '$' ); // NOI18N
        if ( i == -1 ) {
            return name;
        }
        else {
            return name.substring( i + 1 );
        }        
    }
    
    public static String getSimpleName( String fileName ) {
        
        String name = getBaseName( fileName );
        
        int i = name.lastIndexOf( '$' ); // NOI18N
        if ( i == -1 ) {
            return name;
        }
        else {
            return name.substring( i + 1 );
        }
        
    }
    
    public static String convertPackage2Folder( String packageName ) {
        return packageName.replace( '.', '/' ); // NOI18N
    }    
    
    
    public static String convertFolder2Package (String packageName) {
        return convertFolder2Package (packageName, '/');    //NOI18N
    }
    
    public static String convertFolder2Package( String packageName, char folderSeparator ) {
        return packageName.replace( folderSeparator, '.' ); // NOI18N
    }
    
    
    public static String getRelativePath (final String packageName, final String relativeName) {
        StringBuilder relativePath = new StringBuilder ();
        relativePath.append(packageName.replace('.','/')); // NOI18N
        relativePath.append(relativeName);
        return relativePath.toString();
    }
    
    public static String[] getParentRelativePathAndName (final String className) {
        if (className.charAt(className.length()-1) == '.') { // NOI18N
            return null;
        }
        final int index = className.lastIndexOf('.'); // NOI18N
        if (index<0) {
            return new String[] {
                "",     //NOI18N
                className
            };
        }
        else {
            return new String[] {
                className.substring(0,index).replace('.','/'),      //NOI18N
                className.substring(index+1)
            };
        }
    }            
    
    
    /**
     * Determines {@link JavaFileObject.Kind} for given extension
     * @param extension
     * @return the found kind
     */ 
    public static JavaFileObject.Kind getKind (final String extension) {
        if (extension == null) {
            return JavaFileObject.Kind.OTHER;
        }
        String lcextension = extension.toLowerCase();
        if (FileObjects.VISAGE.equals(lcextension) || FileObjects.JAVA.equals(lcextension)) {
                return JavaFileObject.Kind.SOURCE;
        }
        if (FileObjects.CLASS.equals(lcextension) || FileObjects.SIG.equals(lcextension)) {
                return JavaFileObject.Kind.CLASS;
        }
        if (FileObjects.HTML.equals(lcextension)) {
                return JavaFileObject.Kind.HTML;
        }
        return JavaFileObject.Kind.OTHER;
    }
    
    public static void deleteRecursively (final File folder) {
        assert folder != null;        
        if (folder.isDirectory()) {
            File[] children = folder.listFiles();
            if (children != null) {
                for (File file : children) {
                    deleteRecursively(file);
                }
            }
        }
        folder.delete();
    }
    
    // Private methods ---------------------------------------------------------
    
    // Innerclasses ------------------------------------------------------------
    
    public static abstract class Base implements JavaFileObject {

        protected final JavaFileObject.Kind kind;
        protected final String pkgName;
        protected final String nameWithoutExt;
        protected final String ext;        
        
        protected Base (final String pkgName, final String name) {
            assert pkgName != null;
            assert name != null;
            this.pkgName = pkgName;
            String[] res = getNameExtPair(name);
            this.nameWithoutExt = res[0];
            this.ext = res[1];
            this.kind = FileObjects.getKind (this.ext);
        }
        
        public JavaFileObject.Kind getKind() {
            return this.kind;
        }
        
        public boolean isNameCompatible (String simplename, JavaFileObject.Kind k) {
            if (this.kind != k) {
                return false;
            }
	    return nameWithoutExt.equals(simplename);
	}        
        
        public NestingKind getNestingKind() {
            return null;
        }
        
        public Modifier getAccessLevel() {
            return null;
        }
    
        @Override
        public String toString() {
            return this.toUri().toString();
        }
        
        public String getPackage () {
            return this.pkgName;
        }
        
        public String getNameWithoutExtension () {
            return this.nameWithoutExt;
        }
        
        public String getName () {
            return this.nameWithoutExt + '.' + ext; // NOI18N
        }
        
        public String getExt () {
            return this.ext;
        }        
        
        private static String[] getNameExtPair (String name) {
            int index = name.lastIndexOf ('.'); // NOI18N
            String namenx;
            String ext;
            if (index <= 0) {
                namenx =name;
                ext = "";   //NOI18N
            }
            else {
                namenx = name.substring(0,index);
                if (index == name.length()-1) {
                    ext = "";
                }
                else {
                    ext = name.substring(index+1);
                }
            }
            return new String[] {
              namenx,
              ext
            };
        }
    }
    
    public static abstract class FileBase extends Base {
        
        protected final File f;
        
        protected FileBase (final File file, final String pkgName, final String name) {
            super (pkgName, name);
            assert file != null;
            assert file.equals(FileUtil.normalizeFile(file)) : "File: " + file.getAbsolutePath() + " Normalized File: " + FileUtil.normalizeFile(file).getAbsolutePath();  //NOI18N
            this.f = file;
        }
        
        public File getFile () {
            return this.f;
        }
    }
    
    
    public static class InvalidFileException extends IOException {
        
        public InvalidFileException () {
            super ();
        }
        
        public InvalidFileException (final FileObject fo) {
            super (NbBundle.getMessage(FileObjects.class,"FMT_InvalidFile",FileUtil.getFileDisplayName(fo))); // NOI18N
        }
    }
    
    
    public static String getRelativePath (final File root, final File fo) {
        final String rootPath = root.getAbsolutePath();
        final String foPath = fo.getAbsolutePath();
        assert foPath.startsWith(rootPath) : String.format("getRelativePath(%s, %s)", rootPath, foPath); // NOI18N
        int index = rootPath.length();
        if (rootPath.charAt(index-1)!=File.separatorChar) {
            index++;
        }            
        int foIndex = foPath.length();
        if (foIndex <= index) {
            return "";  //NOI18N
        }
        return foPath.substring(index);
    }           
    
    private static class RegularFileObject extends FileBase {
        
        private URI uriCache;
        private final Charset encoding;

	public RegularFileObject(final File f, final String packageName, final String baseName, Charset encoding) {
            super (f, packageName, baseName);
            this.encoding = encoding;
	}               

        public InputStream openInputStream() throws IOException {
	    return new BufferedInputStream (new FileInputStream(f));
	}

	public Reader openReader (boolean b) throws IOException {
	    throw new UnsupportedOperationException();
	}

	public OutputStream openOutputStream() throws IOException {
	    return new FileOutputStream(f);
	}

	public Writer openWriter() throws IOException {
            if (encoding != null) {
                return new OutputStreamWriter(new FileOutputStream(f), encoding);
            } else {
                return new OutputStreamWriter(new FileOutputStream(f));
            }
	}

	public @Override boolean isNameCompatible(String simplename, JavaFileObject.Kind kind) {
	    boolean res = super.isNameCompatible(simplename, kind);
            if (res) {
                return res;
            }
            else if (Utilities.isWindows()) {
                return nameWithoutExt.equalsIgnoreCase(simplename);
            }
            else {
                return false;
            }
	} 	   
        
        public URI toUri () {
            if (this.uriCache == null) {
                this.uriCache = f.toURI();
            }
            return this.uriCache;
        }

        public long getLastModified() {
	    return f.lastModified();
	}

	public boolean delete() {
	    return f.delete();
	}

	public CharSequence getCharContent(boolean ignoreEncodingErrors) throws IOException {
            
            char[] result;
            Reader in;
            
            if (encoding != null) {
                in = new InputStreamReader (new FileInputStream(this.f), encoding);
            } else {
                in = new InputStreamReader (new FileInputStream(this.f));
            }
            int red = 0;
            try {
                int len = (int)this.f.length();
                if (len == 0) len++; //len - red would be 0 while reading from the stream
                result = new char [len+1];
                int rv;	    
                while ((rv=in.read(result,red,len-red))>=0) {
                    red += rv;
                    //In case the filter enlarged the file
                    if (red == len) {
                        char[] _tmp = new char[2*len];
                        System.arraycopy(result, 0, _tmp, 0, len);
                        result = _tmp;
                        len = result.length;
                    }
                }
            } finally {
                in.close();
            }
            result[red++]='\n'; //NOI18N
            CharSequence buffer = CharBuffer.wrap (result, 0, red);
            return buffer;
	}

	@Override
	public boolean equals(Object other) {
	    if (!(other instanceof RegularFileObject))
		return false;
	    RegularFileObject o = (RegularFileObject) other;
	    return f.equals(o.f);
	}

	@Override
	public int hashCode() {
	    return f.hashCode();
	}
        
    }
    
    
    /** A subclass of FileObject representing zip entries.
     * XXX: What happens when the archive is deleted or rebuilt?
     */
    public abstract static class ZipFileBase extends Base {
        
        protected final long mtime;
        protected final String resName;
        
        public ZipFileBase (final String folderName, final String baseName, long mtime) {
            super (convertFolder2Package(folderName),baseName);
            this.mtime = mtime;
            if (folderName.length() == 0) {
                this.resName = baseName;
            }
            else {
                StringBuilder resName = new StringBuilder (folderName);
                resName.append('/');        //NOI18N
                resName.append(baseName);
                this.resName = resName.toString();
            }
        }
        
        public OutputStream openOutputStream() throws IOException {
	    throw new UnsupportedOperationException();
	}

	public Reader openReader(boolean b) throws IOException {
            if (this.getKind() == JavaFileObject.Kind.CLASS) {
                throw new UnsupportedOperationException();
            }
            else {
                return new InputStreamReader (openInputStream(),FileObjects.encodingName);
            }
	}

        public Writer openWriter() throws IOException {
	    throw new UnsupportedOperationException();
	}
        
        public long getLastModified() {
	    return mtime;
	}

	public boolean delete() {
	    throw new UnsupportedOperationException();
	}

	public CharBuffer getCharContent(boolean ignoreEncodingErrors) throws IOException {
	    Reader r = openReader(ignoreEncodingErrors);
            try {
                int red = 0, rv;

                int len = (int)this.getSize();
                char[] result = new char [len+1];
                while ((rv=r.read(result,red,len-red))>0 && (red=red+rv)<len);

                int j=0;
                for (int i=0; i<red;i++) {
                    if (result[i] =='\r') {                                          //NOI18N
                        if (i+1>=red || result[i+1]!='\n') {                         //NOI18N
                            result[j++] = '\n';                                      //NOI18N
                        }
                    }
                    else {
                        result[j++] = result[i];
                    }
                }
                result[j]='\n';                                                      //NOI18N
                return CharBuffer.wrap (result,0,j);
            } finally {
                r.close();
            }
	}
        
        public final URI toUri () {
            URI  zdirURI = this.getArchiveURI();
            try {
                //Optimistic try and see
                return new URI ("jar:"+zdirURI.toString()+"!/"+resName);  //NOI18N
            } catch (URISyntaxException e) {
                //Need to encode the resName part (slower)
                final StringBuilder sb = new StringBuilder ();
                final String[] elements = resName.split("/");                 //NOI18N
                try {
                    for (int i = 0; i< elements.length; i++) {
                        String element = elements[i];
                        element = URLEncoder.encode(element, "UTF-8");       //NOI18N
                        element = element.replace("+", "%20");               //NOI18N
                        sb.append(element);
                        if (i< elements.length - 1) {
                            sb.append('/'); // NOI18N
                        }
                    }
                    return new URI("jar:"+zdirURI.toString()+"!/"+sb.toString());    //NOI18N
                } catch (final UnsupportedEncodingException e2) {
                    final IllegalStateException ne = new IllegalStateException ();
                    ne.initCause(e2);
                    throw ne;
                }
                catch (final URISyntaxException e2) {
                    final IllegalStateException ne = new IllegalStateException ();
                    ne.initCause(e2);
                    throw ne;
                }
            }
        }
        
        @Override
	public int hashCode() {
	    return this.resName.hashCode();
	}                
        
	@Override
	public boolean equals(Object other) {
	    if (!(other instanceof ZipFileBase))
		return false;
	    ZipFileBase o = (ZipFileBase) other;
	    return getArchiveURI().equals(o.getArchiveURI()) && resName.equals(o.resName);
	}
        
        protected abstract URI getArchiveURI ();
        
        protected abstract long getSize() throws IOException;
        
    }
    
    private static class ZipFileObject extends ZipFileBase {
	

	/** The zipfile containing the entry.
	 */
	protected final File archiveFile;
        

        ZipFileObject(final File archiveFile, final String folderName, final String baseName, long mtime) {
            super (folderName,baseName,mtime);
            assert archiveFile != null : "archiveFile == null";   //NOI18N
	    this.archiveFile = archiveFile;
            
	}

        public InputStream openInputStream() throws IOException {            
            class ZipInputStream extends InputStream {

                private ZipFile zipfile;
                private InputStream delegate;

                /**
                 * Creates new ZipInputStream.
                 * When ZipInputStream is created it owns the given ZipFile
                 * and closes it when this InputStream is closed or IOException
                 * is thrown by the constructer.
                 */
                public ZipInputStream (ZipFile zf) throws IOException {
                    assert zf != null;
                    this.zipfile = zf;
                    try {
                        this.delegate = zf.getInputStream(new ZipEntry(resName));
                        if (this.delegate == null) {                        
                            throw new IOException();
                        }
                    } catch (IOException e) {
                        try {
                            this.zipfile.close();
                        } catch (IOException e2) {/*Outher exception is more important*/}
                        throw e;
                    }
                }

                public int read() throws IOException {
                    throw new java.lang.UnsupportedOperationException("Not supported yet."); // NOI18N
                }

                public int read(byte b[], int off, int len) throws IOException {
                    return delegate.read(b, off, len);
                }

                public int available() throws IOException {
                    return this.delegate.available();
                }

                public void close() throws IOException {
                    try {
                        this.delegate.close();
                    } finally {
                        this.zipfile.close();
                    }
                }


            };
            // long time = System.currentTimeMillis();
            ZipFile zf = new ZipFile (archiveFile);
            // System.out.println("ZF OPEN " + archiveFile.getPath() + " took: " + (System.currentTimeMillis() - time )+ "ms." ); // NOI18N
            return new BufferedInputStream (new ZipInputStream (zf));
	}
        
        public URI getArchiveURI () {
            return this.archiveFile.toURI();
        }
        
        protected long getSize () throws IOException {
            ZipFile zf = new ZipFile (archiveFile);
            try {
                ZipEntry ze = zf.getEntry(this.resName);
                return ze == null ? 0L : ze.getSize();
            } finally {
                zf.close();
            }
        }
    }
    
    private static class CachedZipFileObject extends ZipFileBase {
        
        private ZipFile zipFile;
        
        CachedZipFileObject(final ZipFile zipFile, final String folderName, final String baseName, long mtime) {
            super (folderName,baseName,mtime);
            assert zipFile != null : "archiveFile == null";   //NOI18N
	    this.zipFile = zipFile;            
	}
        
        public InputStream openInputStream() throws IOException {
            return new BufferedInputStream (this.zipFile.getInputStream(new ZipEntry (this.resName)));
	}
        
        public URI getArchiveURI () {
            return new File (this.zipFile.getName()).toURI();
        }
        
        protected long getSize() throws IOException {
            ZipEntry ze = this.zipFile.getEntry(this.resName);
            return ze == null ? 0L : ze.getSize();
        }
    }

/*    
    private static class SimpleNameStringComparator implements Comparator<String> {
        
        public int compare( String o1, String o2 ) {
            return getSimpleName( o1 ).compareTo( getSimpleName( o2 ) );
        }
                        
    }
    
    private static class SimpleNameFileObjectComparator implements Comparator<JavaFileObject> {
        
        public int compare( JavaFileObject o1, JavaFileObject o2 ) {
            
            String n1 = getSimpleName( o1 );
            String n2 = getSimpleName( o2 );
                        
            return n1.compareTo( n2 );
        }
                        
    }
  */  
    static final String encodingName = new OutputStreamWriter(new ByteArrayOutputStream()).getEncoding();            
    
}
