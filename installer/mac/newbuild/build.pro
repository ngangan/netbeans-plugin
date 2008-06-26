<?xml version="1.0" encoding="UTF-8"?>

<project name="Mac Installer Properties" basedir="." >
   
    <property name="translatedfiles.src" value="${basedir}/../../../src"/>
        
    <property name="install.dir" value="/Applications/NetBeans"/>
    
    <!-- Base IDE properties   -->       
    <property name="baseide.version" value="6.1"/>
    <property name="mpkg.name_nb" value="NetBeans IDE 6.1"/> 
    <property name="appname" value="NetBeans 6.1"/> 
    <property name="app.name" value="${install.dir}/${appname}.app"/>
    <property name="nbClusterDir" value="nb6.1"/>      

    <property name="appversion" value="6.1"/>
    
    <!-- Tomcat properties   -->    
    <property name="tomcat.install.dir" value="${install.dir}/apache-tomcat-6.0.16"/>
    <property name="tomcat.version" value="6.0.16"/>
    <property name="tomcat_location" value="${binary_cache_host}/tomcat/apache-tomcat-6.0.16.zip"/> 
            
    <!-- GlassFish properties   -->   
    <property name="glassfish.install.dir" value="${install.dir}/glassfish-v2ur2"/>
    <property name="glassfish.version" value="v2ur2"/>
    <property name="glassfish_location"    value="${gf_builds_host}/java/re/glassfish/9.1_02/promoted/fcs/latest/images/mac/glassfish-image-SNAPSHOT.jar"/>
    <property name="glassfish_location_ml" value="${gf_builds_host}/java/re/glassfish/9.1_02/promoted/fcs/latest/l10n/mac/glassfish-image-SNAPSHOT-ml.jar"/>
    
    <!-- Open ESB Properties-->    
    <property name="openesb.install.dir" value="${glassfish.install.dir}/addons"/>
    <property name="openesb.version" value="v2"/>
    <property name="openesb_location" value="${openesb_builds_host}/kits/ojc/openesb_as9_ur2/latest/installers/jbi_components_installer.jar"/>
    <!--property name="openesb_core_source" value="${openesb_builds_host}/kits/openesb/main/latest/CORE/jbi-core-installer.jar"/-->                  

    <property name="dmg.prefix.name" value="${prefix}"/>                  

    <!-- JavaFX Properties -->
    <property name="javafx_location" value="${javafx_builds_host}/hudson/job/JavaFX_NB_Plugin_NB61_daily/ws/main/nbbuild/netbeans/javafx//*zip*/javafx.zip"/>

    <property name="mpkg.name_nb_mysql" value="NetBeans IDE with MySQL"/> 
    <property name="mysql_10.5.pkg.name" value="mysql-5.0.51a-osx10.5-x86"/>
    <property name="mysql_10.4.pkg.name" value="mysql-5.0.51a-osx10.4-i686"/>
    <property name="mysql_10.5.dmg.name" value="netbeans-6.1-mysql-macosx10.5-x86"/>
    <property name="mysql_10.4.dmg.name" value="netbeans-6.1-mysql-macosx10.4-x86"/>
    <property name="mysql_startup.pkg.name" value="MySQLStartupItem"/>
    <property name="mysql_prefPane.name" value="MySQL.prefPane"/>
    <property name="mysql_connector.name" value="mysql-connector-java-5.1.6-bin"/>
    <property name="mysql_10.5.location" value="${binary_cache_host}/mysql/${mysql_10.5.pkg.name}.dmg"/>
    <property name="mysql_10.4.location" value="${binary_cache_host}/mysql/${mysql_10.4.pkg.name}.dmg"/>
    <property name="mysql_prefPane.location" value="${binary_cache_host}/mysql/MySQL.prefPane-leopardfix.zip"/>
    <property name="mysql_connector.location" value="${binary_cache_host}/mysql/${mysql_connector.name}.jar"/>   
    <property name="mysql_license.name" value="NB_GF_MySQL.txt"/>   
    <property name="mysql_readme.name" value="NB_GF_MySQL_Bundle_Thirdparty_license_readme.txt"/>   

</project>