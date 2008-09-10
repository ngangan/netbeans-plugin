#########################################################################
# This script is purposed to test JavaFX Netbeans
# Plugin builds. 
#
#  (c) Eugenia Shuiskaja, SUN Microsystems, Sep 2008
#########################################################################

#!/bin/bash

#Check variables

export

#Start tests
cd $WORKSPACE/main/
rm -rf xtest
mkdir xtest 
wget http://www.netbeans.org/download/xtest/xtest-distribution.zip
unzip -o xtest-distribution.zip
wget ${TEST_URL}/xtest-tools.zip
unzip -o xtest-tools.zip

#create config.properties
cd $WORKSPACE/main/contrib/javafx.kit/test/config/
cat > config.properties <<  EOF

javafx.sdk.version=reprise
xtest.mail.mailhost=localhost
xtest.mail.results.to=${MAILTO}
xtest.mail.failed.to=${FAILMAILTO}
EOF

rm -rf $WORKSPACE/main/contrib/javafx.kit/test/results/*

cd $WORKSPACE/main/contrib/javafx.kit/test
ant
