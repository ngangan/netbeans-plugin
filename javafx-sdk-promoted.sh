#########################################################################
# This script is purposed to set correct URL of JavaFX SDK
#
#  (c) Martin Ryzl, SUN Microsystems, Oct 2008
#########################################################################

#!/bin/bash
<<<<<<< local
# Variables export
#for i in `export | awk  '{print $3}'` ; do
#	export "$i"
#done
export
#export SDK_URL=http://getjfx.sfbay.sun.com/hudson/job/javafx1.0/741/
#sh -x $WORKSPACE/main/contrib/javafx-nb-plugin-build.sh
=======
echo "Promoted JavaFX SDK build b48 (774)"
export SDK_URL=http://getjfx.sfbay.sun.com/hudson/job/javafx1.0/774/
export PRODUCTION_SUITE_URL=http://getjfx.sfbay.sun.com/hudson/job/JavaFX_Production_Suite_Trunk/434/
sh -x $WORKSPACE/main/contrib/javafx-nb-plugin-build.sh
>>>>>>> other
