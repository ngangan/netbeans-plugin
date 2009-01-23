#########################################################################
# This script is purposed to set correct URL of JavaFX SDK
#
#  (c) Martin Ryzl, SUN Microsystems, Oct 2008
#########################################################################

#!/bin/bash
echo "Promoted JavaFX SDK 1.1 build b03, Production Suite b29"

export SDK_URL=http://jre.sfbay.sun.com/java/re/javafx/1.1/promoted/fcs/b03/
export JDK_FILENAME=javafx_sdk-1_1
export JDK_DIRNAME=javafx-sdk1.1
export PRODUCTION_SUITE_URL=http://getjfx.sfbay.sun.com/hudson/job/JavaFX_1.0_Production_Suite/29/label=windows-i586/artifact/installer/win/build/javafx-fxd-netbeans-support-1.1-windows-i586.zip
export SDK_WIN_URL=${SDK_URL}/bundles/windows-i586/${JDK_FILENAME}-windows-i586.zip
export SDK_MAC_URL=${SDK_URL}/bundles/macosx-universal/${JDK_FILENAME}-macosx-universal.zip
export SDK_LIN_URL=${SDK_URL}/bundles/linux-i586/${JDK_FILENAME}-linux-i586.zip
export SDK_SOL_URL=${SDK_URL}/bundles/solaris-sparc/${JDK_FILENAME}-solaris-sparc.zip
export COMPILER_URL=${SDK_URL}/binaries/windows-i586/${JDK_DIRNAME}/lib/shared/javafxc.jar
export JFXDOC_URL=${SDK_URL}/binaries/windows-i586/${JDK_DIRNAME}/lib/shared/javafxdoc.jar

sh -x $WORKSPACE/main/contrib/javafx-nb-plugin-build.sh
