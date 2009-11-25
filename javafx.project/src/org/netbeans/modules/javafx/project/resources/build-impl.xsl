<?xml version="1.0" encoding="UTF-8"?>
<!--
DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.

Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.


The contents of this file are subject to the terms of either the GNU
General Public License Version 2 only ("GPL") or the Common
Development and Distribution License("CDDL") (collectively, the
"License"). You may not use this file except in compliance with the
License. You can obtain a copy of the License at
http://www.netbeans.org/cddl-gplv2.html
or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
specific language governing permissions and limitations under the
License.  When distributing the software, include this License Header
Notice in each file and include the License file at
nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
particular file as subject to the "Classpath" exception as provided
by Sun in the GPL Version 2 section of the License file that
accompanied this code. If applicable, add the following below the
License Header, with the fields enclosed by brackets [] replaced by
your own identifying information:
"Portions Copyrighted [year] [name of copyright owner]"

Contributor(s):

The Original Software is NetBeans. The Initial Developer of the Original
Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
Microsystems, Inc. All Rights Reserved.

If you wish your version of this file to be governed by only the CDDL
or only the GPL Version 2, indicate your decision by adding
"[Contributor] elects to include this software in this distribution
under the [CDDL or GPL Version 2] license." If you do not indicate a
single choice of license, a recipient has the option to distribute
your version of this file under either the CDDL, the GPL Version 2 or
to extend the choice of license to its licensees as provided above.
However, if you add GPL Version 2 code and therefore, elected the GPL
Version 2 license, then the option applies only if the new code is
made subject to such option by the copyright holder.
-->
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:p="http://www.netbeans.org/ns/project/1"
                xmlns:xalan="http://xml.apache.org/xslt"
                xmlns:javafxproject1="http://www.netbeans.org/ns/javafx-project/1"
                xmlns:javafxproject2="http://www.netbeans.org/ns/javafx-project/2"
                xmlns:javafxproject3="http://www.netbeans.org/ns/javafx-project/3"
                xmlns:projdeps="http://www.netbeans.org/ns/ant-project-references/1"
                xmlns:projdeps2="http://www.netbeans.org/ns/ant-project-references/2"
                exclude-result-prefixes="xalan p projdeps projdeps2">
<xsl:comment> XXX should use namespaces for NB in-VM tasks from ant/browsetask and debuggerjavafx/ant (Ant 1.6.1 and higher only)</xsl:comment>
    <xsl:output method="xml" indent="yes" encoding="UTF-8" xalan:indent-amount="4"/>
    <xsl:template match="/">
        
        <xsl:comment><![CDATA[
*** GENERATED FROM project.xml - DO NOT EDIT  ***
***         EDIT ../build.xml INSTEAD         ***

For the purpose of easier reading the script
is divided into following sections:

  - initialization
  - compilation
  - jar
  - execution
  - debugging
  - javadoc
  - junit compilation
  - junit execution
  - junit debugging
  - applet
  - cleanup

        ]]></xsl:comment>
        
        <xsl:variable name="name" select="/p:project/p:configuration/javafxproject3:data/javafxproject3:name"/>
    <!-- Synch with build-impl.xsl: -->
        <xsl:variable name="codename" select="translate($name, ' ', '_')"/>
        <project name="{$codename}-impl">
            <xsl:attribute name="default">default</xsl:attribute>
            <xsl:attribute name="basedir">..</xsl:attribute>
            
            <target name="default">
                <xsl:attribute name="depends">jar</xsl:attribute>
                <xsl:attribute name="description">Build whole project.</xsl:attribute>
            </target>
            
    <xsl:comment>
                    ======================
                    INITIALIZATION SECTION
                    ======================
    </xsl:comment>
        <target name="-pre-init">
        <xsl:comment> Empty placeholder for easier customization.</xsl:comment>
        <xsl:comment> You can override this target in the ../build.xml file.</xsl:comment>
        </target>
        <target depends="-pre-init" name="-init-private">
            <macrodef name="property" uri="http://www.netbeans.org/ns/javafx-project/1">
                <attribute name="name"/>
                <attribute name="value"/>
                <sequential>
                    <property name="@{{name}}" value="${{@{{value}}}}"/>
                </sequential>
            </macrodef>
            <property file="nbproject/private/config.properties"/>
            <property file="nbproject/private/configs/${{config}}.properties"/>
            <property file="nbproject/private/private.properties"/>
        </target>
        <target depends="-pre-init,-init-private" name="-init-user">
            <property file="${{user.properties.file}}"/>
        </target>
        <target depends="-pre-init,-init-private,-init-user" name="-init-project">
            <property file="nbproject/configs/${{config}}.properties"/>
            <property file="nbproject/project.properties"/>
        </target>
        <target depends="-pre-init,-init-private,-init-user,-init-project" name="-do-init">
            <javafxproject1:property name="platform.fxhome" value="platforms.${{platform.active}}.fxhome"/>
            <javafxproject1:property name="platform.javadoc.tmp" value="platforms.${{platform.active}}.javadoc"/>
            <condition property="platform.javadoc" value="${{platform.home}}/bin/javadoc">
                <equals arg1="${{platform.javadoc.tmp}}" arg2="$${{platforms.${{platform.active}}.javadoc}}"/>
            </condition>
            <property name="platform.javadoc" value="${{platform.javadoc.tmp}}"/>
            <condition property="no.javadoc.preview">
                <and>
                    <isset property="javadoc.preview"/>
                    <isfalse value="${{javadoc.preview}}"/>
                </and>
            </condition>
            <property name="work.dir" value="${{basedir}}"/>
            <condition property="no.deps">
                <istrue value="${{no.dependencies}}"/>
            </condition>
            <condition property="codebase.arg" value="-appCodebase ${{codebase.url}}" else="">
                <isset property="codebase.url"/>
            </condition>
            <property file="${{user.properties.file}}/../config/preferences/org/apache/tools/ant/module.properties" prefix="ant.module"/>
            <condition property="verbose.arg" value="-v" else="">
                <or>
                    <equals arg1="${{ant.module.verbosity}}" arg2="3"/>
                    <equals arg1="${{ant.module.verbosity}}" arg2="4"/>
                </or>
            </condition>
            <condition property="draggable.arg" value="-draggable" else="">
                <istrue value="${{applet.draggable}}"/>
            </condition>
            <condition property="pack200.arg" value="-pack200" else="">
                <istrue value="${{jnlp.packEnabled}}"/>
            </condition>
            <condition property="sign.arg" value="-sign" else="">
                <istrue value="${{jnlp.signed}}"/>
            </condition>
            <property name="javadoc.preview" value="true"/>
            <property name="source.encoding" value="${{file.encoding}}"/>
            <condition property="binary.extension" value=".exe" else="">
                <os family="windows"/>
            </condition>
            <property name="javafx.profile" value="desktop"/>
            <condition property="midp.execution.trigger">
                <equals arg1="${{javafx.profile}}" arg2="mobile"/>
            </condition>
            <condition property="tv.execution.trigger">
                <equals arg1="${{javafx.profile}}" arg2="tv"/>
            </condition>
            <condition property="jnlp.execution.trigger">
                <and>
                    <equals arg1="${{javafx.profile}}" arg2="desktop"/>
                    <equals arg1="${{execution.target}}" arg2="jnlp"/>
                </and>    
            </condition>
            <condition property="applet.execution.trigger">
                <and>
                    <equals arg1="${{javafx.profile}}" arg2="desktop"/>
                    <equals arg1="${{execution.target}}" arg2="applet"/>
                    <isset property="netbeans.home"/>
                </and>    
            </condition>
            <condition property="standard.execution.trigger">
                <not>
                    <or>
                         <isset property="jnlp.execution.trigger"/>
                         <isset property="applet.execution.trigger"/>
                         <isset property="midp.execution.trigger"/>
                         <isset property="tv.execution.trigger"/>
                    </or>
                </not>
            </condition>
            <property name="run.jvmargs" value=""/>
            <available property="emulator.available" file="${{platform.fxhome}}/emulator/mobile/bin/emulator${{binary.extension}}"/>
            <available property="tvemulator.available" file="${{platform.fxhome}}/emulator/tv/bin/cvm${{binary.extension}}"/>
        </target>
        <target name="-post-init">
        <xsl:comment> Empty placeholder for easier customization.</xsl:comment>
        <xsl:comment> You can override this target in the ../build.xml file.</xsl:comment>
        </target>
        <target depends="-pre-init,-init-private,-init-user,-init-project,-do-init" name="-init-check">
            <fail unless="build.dir">Must set build.dir</fail>
            <fail unless="dist.dir">Must set dist.dir</fail>
            <fail unless="dist.javadoc.dir">Must set dist.javadoc.dir</fail>
        </target>
        <target depends="-pre-init,-init-private,-init-user,-init-project,-do-init,-post-init,-init-check" name="init"/>
    <xsl:comment>
                    ===================
                    COMPILATION SECTION
                    ===================
    </xsl:comment>
        
        <xsl:call-template name="deps.target">
            <xsl:with-param name="targetname" select="'deps-jar'"/>
            <xsl:with-param name="type" select="'jar'"/>
        </xsl:call-template>
            
        <target name="-pre-compile">
        <xsl:comment> Empty placeholder for easier customization.</xsl:comment>
        <xsl:comment> You can override this target in the ../build.xml file.</xsl:comment>
        </target>
        <target depends="init,deps-jar,-pre-compile" name="-do-compile">
            <exec executable="${{platform.fxhome}}/bin/javafxpackager${{binary.extension}}" failonerror="true">
                <arg value="-src"/>
                <arg>
                    <xsl:attribute name="value">
                        <xsl:call-template name="createPath">
                            <xsl:with-param name="roots" select="/p:project/p:configuration/javafxproject3:data/javafxproject3:source-roots"/>
                        </xsl:call-template>
                    </xsl:attribute>
                </arg>
                <arg value="-workdir"/>
                <arg file="${{build.dir}}"/>
                <arg value="-d"/>
                <arg file="${{dist.dir}}"/>
                <arg value="-appname"/>
                <arg value="${{application.title}}"/>
                <arg value="-appvendor"/>
                <arg value="${{application.vendor}}"/>
                <arg value="-appwidth"/>
                <arg value="${{applet.width}}"/>
                <arg value="-appheight"/>
                <arg value="${{applet.height}}"/>
                <arg value="-appclass"/>
                <arg value="${{main.class}}"/>
                <arg line="${{codebase.arg}}"/>
                <arg value="-encoding"/>
                <arg value="${{source.encoding}}"/>
                <arg value="-p"/>
                <arg value="${{javafx.profile}}"/>
                <arg value="${{verbose.arg}}"/>
                <arg value="${{draggable.arg}}"/>
                <arg value="${{pack200.arg}}"/>
                <arg value="${{sign.arg}}"/>
                <arg value="-cp"/>
                <arg path="${{javac.classpath}}"/>
            </exec>
        </target>
        <target name="-post-compile">
        <xsl:comment> Empty placeholder for easier customization.</xsl:comment>
        <xsl:comment> You can override this target in the ../build.xml file.</xsl:comment>
        </target>
        <target depends="init,deps-jar,-pre-compile,-do-compile,-post-compile" description="Compile project." name="compile"/>
    <xsl:comment>
                    ====================
                    JAR BUILDING SECTION
                    ====================
    </xsl:comment>
        <target depends="init,compile" description="Build." name="jar"/>
    <xsl:comment>
                    =================
                    EXECUTION SECTION
                    =================
    </xsl:comment>
        <target depends="init,compile,jar" if="standard.execution.trigger" description="Run a main class." name="standard-run">
            <property name="application.args" value=""/>
            <java fork="true" jvm="${{platform.fxhome}}/bin/javafx${{binary.extension}}" classpath="${{dist.dir}}/${{application.title}}.jar" classname="${{main.class}}" jvmargs="${{run.jvmargs}}" failonerror="true">
                <arg line="${{application.args}}"/>
            </java>
        </target>
        <target depends="jar" if="midp.execution.trigger" description="Start MIDP execution" name="midp-run">
            <fail unless="emulator.available" message="Current platform does not include mobile device emulator necessary for the execution."/>
            <property name="jad.file" location="${{dist.dir}}/${{application.title}}.jad"/>
            <property name="mobile.device" value="DefaultFxPhone1"/>
            <condition property="emulator.exec.arg" value="-Xjam:install=" else="-Xdescriptor:">
                <istrue value="${{jad.install}}"/>
            </condition>
            <exec executable="${{platform.fxhome}}/emulator/mobile/bin/emulator${{binary.extension}}" failonerror="true">
                <arg value="${{run.jvmargs}}"/>
                <arg value="${{emulator.exec.arg}}${{jad.file}}"/>
                <arg value="-Xdevice:${{mobile.device}}"/>
            </exec>
        </target>
        <target depends="jar" if="tv.execution.trigger" description="Start TV execution" name="tv-run">
            <fail unless="tvemulator.available" message="Current platform does not include tv emulator necessary for the execution."/>
            <property name="jar.file" location='${{dist.dir}}/${{application.title}}.jar'/>
            <exec executable="${{platform.fxhome}}/emulator/tv/bin/cvm${{binary.extension}}" failonerror="true">
                <arg value="-Dprism.verbose=true"/>
                <arg value="-Dprism.order=es1"/>
                <arg value="-Djava.library.path=${{platform.fxhome}}/emulator/tv/bin"/>
                <arg value="-Dsun.boot.library.path=${{platform.fxhome}}/emulator/tv/bin"/>
                <arg value="-Djavafx.toolkit=com.sun.javafx.tk.prism.PrismToolkit"/>
                <arg value="-Djava.security.policy=${{platform.fxhome}}/emulator/tv/lib/security/java_permissive.policy"/>
                <arg value="-Xbootclasspath/a:${{platform.fxhome}}/lib/tv/javafxrt-cdc.jar:${{jar.file}}"/>
                <arg value="com.sun.javafx.runtime.main.Main"/>
                <arg value="${{main.class}}"/>
                <arg value="${{run.jvmargs}}"/>
            </exec>
        </target>
        <target depends="init,jar" if="applet.execution.trigger" name="browser-run">
            <makeurl property="applet.local.url" file="${{dist.dir}}/${{application.title}}.html"/>
            <condition property="applet.url" value="${{codebase.url}}/${{application.title}}.html" else="${{applet.local.url}}">
                <isset property="codebase.url"/>
            </condition>
            <nbbrowse url="${{applet.url}}"/>
        </target>
        <target depends="jar"  if="jnlp.execution.trigger" description="Start javaws execution" name="jws-run">
            <condition property="javaws.home" value="/usr" else="${{java.home}}">
                <os family="mac"/>
            </condition>
            <exec executable="${{javaws.home}}/bin/javaws" failonerror="true">
                <env key="JAVAWS_VM_ARGS" value="${{run.jvmargs}}"/>
                <arg file="${{dist.dir}}/${{application.title}}.jnlp"/>
            </exec>
        </target>
        <target depends="init,compile,jar,standard-run,browser-run,jws-run,midp-run,tv-run" description="Run an application." name="run"/>
    <xsl:comment>
                    =================
                    DEBUGGING SECTION
                    =================
    </xsl:comment>
        <target depends="init" if="netbeans.home" name="-debug-start-debugger">
            <nbjpdastart addressproperty="javafx.address" name="${{application.title}}" transport="dt_socket">
                <classpath>
                    <path path="${{javac.classpath}}"/>
                </classpath>
                <sourcepath>
                    <path>
                      <xsl:attribute name="path">
                          <xsl:call-template name="createPath">
                              <xsl:with-param name="roots" select="/p:project/p:configuration/javafxproject3:data/javafxproject3:source-roots"/>
                          </xsl:call-template>
                      </xsl:attribute>
                    </path>
                </sourcepath>
            </nbjpdastart>
        </target>
        <target depends="init" if="netbeans.home" name="-debug-start-debugger-stepinto">
            <nbjpdastart addressproperty="javafx.address" name="${{application.title}}" stopclassname="${{main.class}}" transport="dt_socket">
                <classpath>
                    <path path="${{javac.classpath}}"/>
                </classpath>
                <sourcepath>
                    <path>
                      <xsl:attribute name="path">
                          <xsl:call-template name="createPath">
                              <xsl:with-param name="roots" select="/p:project/p:configuration/javafxproject3:data/javafxproject3:source-roots"/>
                          </xsl:call-template>
                      </xsl:attribute>
                    </path>
                </sourcepath>
            </nbjpdastart>
        </target>
        <target depends="init,compile" if="standard.execution.trigger" name="-debug-start-debuggee">
            <property name="application.args" value=""/>
            <java fork="true" jvm="${{platform.fxhome}}/bin/javafx${{binary.extension}}" classpath="${{dist.dir}}/${{application.title}}.jar" classname="${{main.class}}">
                <jvmarg value="-Xrunjdwp:transport=dt_socket,address=${{javafx.address}}"/>
                <jvmarg line="${{run.jvmargs}}"/>
                <syspropertyset>
                    <propertyref prefix="run-sys-prop."/>
                    <mapper from="run-sys-prop.*" to="*" type="glob"/>
                </syspropertyset>
                <arg line="${{application.args}}"/>
            </java>
        </target>
        <target name="-debug-midp-debuggee" if="midp.execution.trigger">
            <fail unless="emulator.available" message="Current platform does not include mobile device emulator necessary for the debugging."/>
            <property name="jad.file" location="${{dist.dir}}/${{application.title}}.jad"/>
            <condition property="emulator.exec.arg" value="-Xjam:install=" else="-Xdescriptor:">
                <istrue value="${{jad.install}}"/>
            </condition>
             <exec executable="${{platform.fxhome}}/emulator/mobile/bin/emulator${{binary.extension}}">
                <arg value="${{run.jvmargs}}"/>
                <arg value="${{emulator.exec.arg}}${{jad.file}}"/>
                <arg value="-Xdebug"/>
                <arg value="-Xrunjdwp:transport=dt_socket,address=${{javafx.address}},server=n"/>
            </exec>
        </target>
        <target name="-debug-tv-debuggee" if="tv.execution.trigger">
            <fail message="Current platform does not support tv emulator debugging."/>
        </target>
        <target if="jnlp.execution.trigger" name="-debug-javaws-debuggee">
            <condition property="javaws.home" value="/usr" else="${{java.home}}">
                <os family="mac"/>
            </condition>
            <exec executable="${{javaws.home}}/bin/javaws">
                <env key="JAVAWS_VM_ARGS" value="-Xdebug -Xnoagent -Djava.compiler=none -Xrunjdwp:transport=dt_socket,address=${{javafx.address}} ${{run.jvmargs}}"/>
                <arg file="${{dist.dir}}/${{application.title}}.jnlp"/>
            </exec>
        </target>
        <target depends="init,compile,-debug-start-debugger,-debug-start-debuggee,-debug-javaws-debuggee,-debug-midp-debuggee" description="Debug project in IDE." if="netbeans.home" name="debug"/>
        <target depends="init,compile,-debug-start-debugger-stepinto,-debug-start-debuggee,-debug-javaws-debuggee,-debug-midp-debuggee" if="netbeans.home" name="debug-stepinto"/>
    <xsl:comment>
                    ===============
                    JAVADOC SECTION
                    ===============
    </xsl:comment>
        <target depends="init" name="-javadoc-build">
            <mkdir dir="${{dist.javadoc.dir}}"/>
            <javadoc author="${{javadoc.author}}" classpath="${{javac.classpath}}" destdir="${{dist.javadoc.dir}}" executable="${{platform.fxhome}}/bin/javafxdoc${{binary.extension}}" failonerror="true" private="${{javadoc.private}}" version="${{javadoc.version}}" useexternalfile="true"  encoding="${{source.encoding}}">
                <xsl:for-each select="/p:project/p:configuration/javafxproject3:data/javafxproject3:source-roots/javafxproject3:root">
                    <fileset includes="**/*.fx">
                        <xsl:attribute name="dir">
                          <xsl:text>${</xsl:text>
                          <xsl:value-of select="@id"/>
                          <xsl:text>}</xsl:text>
                        </xsl:attribute>
                    </fileset>
                </xsl:for-each>						
            </javadoc>
            <condition property="javadoc.available">
                <and>
                    <isset property="netbeans.home"/>
                    <available file="${{dist.javadoc.dir}}/index.html"/>
                </and>
            </condition>
        </target>
        <target depends="init,-javadoc-build" if="javadoc.available" name="-javadoc-browse" unless="no.javadoc.preview">
            <nbbrowse file="${{dist.javadoc.dir}}/index.html"/>
        </target>
        <target depends="init,-javadoc-build,-javadoc-browse" description="Build Javadoc." name="javadoc"/>
    <xsl:comment>
                    ===============
                    CLEANUP SECTION
                    ===============
    </xsl:comment>
        <target depends="init" name="deps-clean" unless="no.deps"/>
        <target depends="init" name="-do-clean">
            <delete dir="${{build.dir}}"/>
            <delete dir="${{dist.dir}}"/>
        </target>
        <target name="-post-clean">
        <xsl:comment> Empty placeholder for easier customization.</xsl:comment>
        <xsl:comment> You can override this target in the ../build.xml file.</xsl:comment>
        </target>
        <target depends="init,deps-clean,-do-clean,-post-clean" description="Clean build products." name="clean"/>
    </project>
    </xsl:template>


    <!---
    Generic template to build subdependencies of a certain type.
    Feel free to copy into other modules.
    @param targetname required name of target to generate
    @param type artifact-type from project.xml to filter on; optional, if not specified, uses
                all references, and looks for clean targets rather than build targets
    @return an Ant target which builds (or cleans) all known subprojects
    -->
    <xsl:template name="deps.target">
        <xsl:param name="targetname"/>
        <xsl:param name="type"/>
        <target name="{$targetname}">
            <xsl:attribute name="depends">init</xsl:attribute>
            <xsl:attribute name="unless">no.deps</xsl:attribute>
            
            <xsl:variable name="references2" select="/p:project/p:configuration/projdeps2:references"/>
            <xsl:for-each select="$references2/projdeps2:reference[not($type) or projdeps2:artifact-type = $type]">
                <xsl:variable name="subproj" select="projdeps2:foreign-project"/>
                <xsl:variable name="subtarget">
                    <xsl:choose>
                        <xsl:when test="$type">
                            <xsl:value-of select="projdeps2:target"/>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:value-of select="projdeps2:clean-target"/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:variable>
                <xsl:variable name="script" select="projdeps2:script"/>
                <xsl:choose>
                    <xsl:when test="projdeps2:properties">
                        <ant target="{$subtarget}" inheritall="false" antfile="{$script}">
                            <xsl:for-each select="projdeps2:properties/projdeps2:property">
                                <property name="{@name}" value="{.}"/>
                            </xsl:for-each>
                        </ant>
                    </xsl:when>
                    <xsl:otherwise>
                        <ant target="{$subtarget}" inheritall="false" antfile="{$script}"/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:for-each>
            
            <xsl:variable name="references" select="/p:project/p:configuration/projdeps:references"/>
            <xsl:for-each select="$references/projdeps:reference[not($type) or projdeps:artifact-type = $type]">
                <xsl:variable name="subproj" select="projdeps:foreign-project"/>
                <xsl:variable name="subtarget">
                    <xsl:choose>
                        <xsl:when test="$type">
                            <xsl:value-of select="projdeps:target"/>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:value-of select="projdeps:clean-target"/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:variable>
                <xsl:variable name="script" select="projdeps:script"/>
                <ant target="{$subtarget}" inheritall="false" antfile="${{project.{$subproj}}}/{$script}"/>
            </xsl:for-each>
            
        </target>
    </xsl:template>


    <xsl:template name="createPath">
        <xsl:param name="roots"/>
        <xsl:for-each select="$roots/javafxproject3:root">
            <xsl:if test="position() != 1">
                <xsl:text>;</xsl:text>
            </xsl:if>
            <xsl:text>${</xsl:text>
            <xsl:value-of select="@id"/>
            <xsl:text>}</xsl:text>
        </xsl:for-each>						
    </xsl:template>

</xsl:stylesheet>
