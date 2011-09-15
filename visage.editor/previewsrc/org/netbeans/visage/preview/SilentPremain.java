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
 * Portions Copyrighted 2007 Sun Microsystems, Inc.
 */


package org.netbeans.visage.preview;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.lang.instrument.ClassFileTransformer;
import java.lang.instrument.IllegalClassFormatException;
import java.lang.instrument.Instrumentation;
import java.lang.reflect.Method;
import java.security.ProtectionDomain;

/**
 *
 * @author Adam
 */
public class SilentPremain {
    static final int CONSTANT_Utf8 = 1;
    static final int CONSTANT_Integer = 3;
    static final int CONSTANT_Float = 4;
    static final int CONSTANT_Long = 5;
    static final int CONSTANT_Double = 6;
    static final int CONSTANT_Class = 7;
    static final int CONSTANT_String = 8;
    static final int CONSTANT_FieldRef = 9;
    static final int CONSTANT_MethodRef = 10;
    static final int CONSTANT_InterfaceMethodRef = 11;
    static final int CONSTANT_NameAndType = 12;

    static PrintStream out;

    public static void premain(String agentArgs, Instrumentation inst) {
        out = System.out;
        new Thread(new Runnable() {
            public void run() {
                try {
                    Thread.sleep(10000);
                    out.close();
                    Runtime.getRuntime().halt(1);
                } catch (InterruptedException ex) {
                    ex.printStackTrace();
                }
            }
        }, "Preview Timeout").start(); //NOI18N
        try {
            final Method setOut = System.class.getDeclaredMethod("setOut0", PrintStream.class); //NOI18N
            setOut.setAccessible(true);
            setOut.invoke(null, new PrintStream(new OutputStream() {

                public void write(int b) throws IOException {
                }
            }));
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
        inst.addTransformer(new ClassFileTransformer() {
            public byte[] transform(ClassLoader loader, String className, Class<?> classBeingRedefined, ProtectionDomain protectionDomain, byte[] classfileBuffer) throws IllegalClassFormatException {
                if ("java/awt/Window".equals(className)) try {
                    int showIndex = 0, codeIndex = 0;
                    DataInputStream in = new DataInputStream(new ByteArrayInputStream(classfileBuffer));
                    ByteArrayOutputStream newClass = new ByteArrayOutputStream();
                    DataOutputStream out = new DataOutputStream(newClass);
                    int magic = in.readInt(); out.writeInt(magic);
                    int minorVersion = in.readShort(); out.writeShort(minorVersion);
                    int majorVersion = in.readShort(); out.writeShort(majorVersion);
                    int cPoolSize = in.readUnsignedShort(); out.writeShort(cPoolSize + 6); //adding 6 records to constant pool
                    for (int i=1; i<cPoolSize; i++) {
                        byte type = in.readByte(); out.writeByte(type);
                        switch (type) {
                            case CONSTANT_Utf8: 
                                int len = in.readUnsignedShort(); out.writeShort(len);
                                byte buff[] = new byte[len];
                                in.readFully(buff); out.write(buff);
                                String s = new String(buff, "UTF-8");
//                                System.out.println(i+ ": "+s);
                                if ("show".equals(s)) showIndex = i;
                                else if ("Code".equals(s)) codeIndex = i;
                                break;
                            case CONSTANT_Integer: int ii = in.readInt(); out.writeInt(ii); break;
                            case CONSTANT_Float: float f = in.readFloat(); out.writeFloat(f); break;
                            case CONSTANT_Long: long l = in.readLong(); i++; out.writeLong(l); break;
                            case CONSTANT_Double: double d = in.readDouble(); i++; out.writeDouble(d); break;
                            case CONSTANT_Class: int us = in.readUnsignedShort(); out.writeShort(us);
                            break;
                            case CONSTANT_String: int index = in.readUnsignedShort(); out.writeShort(index); break;
                            case CONSTANT_FieldRef: int classIndex = in.readUnsignedShort(); out.writeShort(classIndex);
                                                    int natIndex = in.readUnsignedShort(); out.writeShort(natIndex); break;
                            case CONSTANT_MethodRef: classIndex = in.readUnsignedShort(); out.writeShort(classIndex);
                                                     natIndex = in.readUnsignedShort(); out.writeShort(natIndex);
                                                     break;
                            case CONSTANT_InterfaceMethodRef: classIndex = in.readUnsignedShort(); out.writeShort(classIndex);
                                                              natIndex = in.readUnsignedShort(); out.writeShort(natIndex); break;
                            case CONSTANT_NameAndType: int nameIndex = in.readUnsignedShort(); out.writeShort(nameIndex);
                                                       int descIndex = in.readUnsignedShort(); out.writeShort(descIndex);
                                                       break;
                            default: throw new IOException("unknown CP type");
                        }
                    }
                    
                    byte buff[] = "org/netbeans/visage/preview/ScreenShot".getBytes("UTF-8");
                    out.writeByte(CONSTANT_Utf8);
                    out.writeShort(buff.length);
                    out.write(buff);
                    buff = "screenShot".getBytes("UTF-8");
                    out.writeByte(CONSTANT_Utf8);
                    out.writeShort(buff.length);
                    out.write(buff);
                    buff = "(Ljava/awt/Window;)V".getBytes("UTF-8");
                    out.writeByte(CONSTANT_Utf8);
                    out.writeShort(buff.length);
                    out.write(buff);
                    out.writeByte(CONSTANT_Class);
                    out.writeShort(cPoolSize);
                    out.writeByte(CONSTANT_NameAndType);
                    out.writeShort(cPoolSize + 1);
                    out.writeShort(cPoolSize + 2);
                    out.writeByte(CONSTANT_MethodRef);
                    out.writeShort(cPoolSize + 3);
                    out.writeShort(cPoolSize + 4);
                    int shotMethodIndex = cPoolSize + 5;

                    int classAccess = in.readUnsignedShort(); out.writeShort(classAccess);
                    int classInfo = in.readUnsignedShort(); out.writeShort(classInfo);
                    int superClassIndex = in.readUnsignedShort(); out.writeShort(superClassIndex);
                    int countClasses = in.readUnsignedShort(); out.writeShort(countClasses);
                    for (int i=0; i<countClasses; i++) {
                        int classIndex = in.readUnsignedShort(); out.writeShort(classIndex);
                    }
                    int variableCount = in.readUnsignedShort(); out.writeShort(variableCount);
                    for (int i = 0; i < variableCount; i++) {
                        int access = in.readUnsignedShort(); out.writeShort(access);
                        int iName = in.readUnsignedShort(); out.writeShort(iName);
                        int iType = in.readUnsignedShort(); out.writeShort(iType);
                        int attrCount = in.readUnsignedShort(); out.writeShort(attrCount);
                        for (int j = 0; j < attrCount; j++) {
                            int attrName = in.readUnsignedShort(); out.writeShort(attrName);
                    	    int attrLen = in.readInt(); out.writeInt(attrLen);
                            buff = new byte[attrLen];
                    		in.readFully(buff); out.write(buff);
                	    }
                    }
                    int methodsCount = in.readUnsignedShort(); out.writeShort(methodsCount);
                    for (int i = 0; i < methodsCount; i++) {
                        int access = in.readUnsignedShort(); out.writeShort(access);
                        int iName = in.readUnsignedShort(); out.writeShort(iName);
                        int iType = in.readUnsignedShort(); out.writeShort(iType);
                        int attrCount = in.readUnsignedShort(); out.writeShort(attrCount);
                        for (int j = 0; j < attrCount; j++) {
                            int attrName = in.readUnsignedShort(); out.writeShort(attrName);
                    	    int attrLen = in.readInt(); out.writeInt(attrLen);
                            buff = new byte[attrLen];
                    		in.readFully(buff);
                            if (iName == showIndex && attrName == codeIndex) {
                                out.writeShort(1); //max stack
                                out.writeShort(1); //max local
                                out.writeInt(5); //code length
                                out.writeByte(42); //aload_0
                                out.writeByte(184); //invoke static
                                out.writeShort(shotMethodIndex); //method reference
                                out.writeByte(177); //return
                                out.writeShort(0); //no exceptions
                                out.writeShort(0); //no attributes
                            } else {
                                out.write(buff);
                            }
                	    }
                    }
                    int attrCount = in.readUnsignedShort(); out.writeShort(attrCount);
                    for (int j = 0; j < attrCount; j++) {
                        int attrName = in.readUnsignedShort(); out.writeShort(attrName);
                        int attrLen = in.readInt(); out.writeInt(attrLen);
                        buff = new byte[attrLen];
                        in.readFully(buff); out.write(buff);
                    }
                    return newClass.toByteArray();
                } catch (IOException e) {
                    e.printStackTrace();
                }
                return classfileBuffer;
            }
        });
    }
}
