#########################################################################
# This script is purposed to set correct URL of JavaFX SDK
#
#  (c) Martin Ryzl, SUN Microsystems, Oct 2008
#########################################################################

#!/bin/bash
echo "Regular builds using latest successful builds"

export SDK_URL=http://getjfx.sfbay.sun.com/hudson/view/JavaFX%20Franca/job/franca
export JDK_FILENAME=javafx_sdk-1_1
export JDK_DIRNAME=javafx-sdk1.1
export PRODUCTION_SUITE_URL=http://getjfx.sfbay.sun.com/hudson/view/Production%20Suite/job/JavaFX_Production_Suite_Trunk/lastSuccessfulBuild/label=windows-i586/artifact/installer/win/build/javafx-fxd-netbeans-support-1.1-windows-i586.zip
export SDK_WIN_URL=${SDK_URL}/label=windows-i586/lastSuccessfulBuild/artifact/build/windows-i586/release/bundles/${JDK_FILENAME}-windows-i586.zip
export SDK_MAC_URL=${SDK_URL}/label=macosx-universal/lastSuccessfulBuild/artifact/build/macosx-universal/release/bundles/${JDK_FILENAME}-macosx-universal.zip
export SDK_LIN_URL=${SDK_URL}/label=linux-i586/lastSuccessfulBuild/artifact/build/linux-i586/release/bundles/${JDK_FILENAME}-linux-i586.zip	
export SDK_SOL_URL=${SDK_URL}/label=solaris-sparc/lastSuccessfulBuild/artifact/build/solaris-sparc/release/bundles/${JDK_FILENAME}-solaris-sparc.zip	
export COMPILER_URL=${SDK_URL}/label=windows-i586/lastSuccessfulBuild/artifact/build/windows-i586/release/javafx-sdk-image/${JDK_DIRNAME}/lib/shared/javafxc.jar
export JFXDOC_URL=${SDK_URL}/label=windows-i586/lastSuccessfulBuild/artifact/build/windows-i586/release/javafx-sdk-image/${JDK_DIRNAME}/lib/shared/javafxdoc.jar

sh -x $WORKSPACE/main/contrib/javafx-nb-plugin-build.sh
