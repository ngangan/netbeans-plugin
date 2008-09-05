#!/bin/bash

#Get or renew the Netbeans repo
###########################
cd $WORKSPACE
if [ -d $WORKSPACE/main ]; then
    cd $WORKSPACE/main
    hg pull
else
    hg clone http://hg.netbeans.org/main $WORKSPACE/main
fi

#Get contrib repository
###########################

cd  $WORKSPACE/main
rm -rf contrib
rm -rf nbbuild
hg up -C
hg clone http://hg.netbeans.org/javafx contrib

##########################
#Start the build 
###########################

cd $WORKSPACE/main/nbbuild

cat > user.build.properties  <<EOF
nb.cluster.javafx=\\
       contrib/javafx.debug,\\
       contrib/javafx.editor,\\
       contrib/javafx.kit,\\
       contrib/javafx.lexer,\\
       contrib/javafx.lib,\\
       contrib/javafx.project,\\
       contrib/javafx.platform,\\
       contrib/javafx.source,\\
       contrib/javafx.userlib,\\
       contrib/javafx.profiler,\\
       contrib/javafx.bestpractices,\\
       contrib/javafx.weather2,\\
       contrib/javafx.sdk.win,\\
       contrib/javafx.sdk.mac,\\
       contrib/javafx.sdk.lin,\\
       contrib/javafx.sdk.sol,\\
       contrib/api.debugger.javafx,\\
       contrib/debugger.javafx,\\
       contrib/debugger.javafx.ant,\\
       contrib/debugger.javafx.projects,\\
       contrib/debugger.javafx.ui,\\
       contrib/javafx.palette,\\
       contrib/javafx.navigation

javafx-sdk.win.url=${SDK_URL}/windows-i586/bundles/javafx_sdk-1_0-dev-windows-i586.zip
javafx-sdk.mac.url=${SDK_URL}/macosx-universal/bundles/javafx_sdk-1_0-dev-macosx-universal.zip
javafx-sdk.lin.url=${SDK_URL}/linux-i586/bundles/javafx_sdk-1_0-dev-linux-i586.zip
javafx-sdk.sol.url=${SDK_URL}/solaris-sparc/bundles/javafx_sdk-1_0-dev-solaris-sparc.zip
jfxcompiler.jar.url=${SDK_URL}/windows-i586/javafx-sdk-image/javafx-sdk1.0dev/lib/javafxc.jar
jfxdoc.jar.url=${OPEN_COMPILER_URL}/lib/javafxdoc.jara
jfxdoc.jar.url=http://openjfx.java.sun.com/hudson/job/openjfx-compiler/lastSuccessfulBuild/artifact/openjfx-compiler/dist/lib/javafxdoc.jar
nb.cluster.javafx.dir=javafx2

EOF

# Get Netbeans binary
########################## 
rm -rf nbms/* netbeans
unzip -o /net/smetiste.czech/space/builds/netbeans/trunk/latest_daily/zip/netbeans-*-*[0-9].zip

ant init  build-one-cluster -Done.cluster.name=nb.cluster.javafx -Dnb.cluster.javafx-hasNoDependencies=true -Dverify.checkout=false -Dscan.binaries=true

ant build-nbms -Dmoduleconfig=javafx -Dnb.cluster.javafx-hasNoDependencies=true -Dverify.checkout=false -Dscan.binaries=true

#Build rip clusters
########################## 

cd $WORKSPACE/main/contrib
ant zip-clusters

#Compress results
##########################

cd $WORKSPACE/main/nbbuild
mkdir -p nbms/compress
zip nbms/compress/${BUILD_ID}.zip nbms/javafx2/*.nbm
tar cvf - nbms/javafx/*.nbm |  gzip  > nbms/compress/${BUILD_ID}.tar.gz
ln -s $WORKSPACE/main/nbbuild/nbms/compress/${BUILD_ID}.zip nbms/compress/netbeans-6.5-javafx-nbms.zip
