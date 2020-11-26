@GOTO start

:add
 @set X=%X%;%1
 @GOTO :eof

:start
@set X=.\dist\ProM-Framework-6.10.110.jar
@set X=%X%;.\dist\ProM-Contexts-6.10.62.jar
@set X=%X%;.\dist\ProM-Models-6.10.40.jar
@set X=%X%;.\dist\ProM-Plugins-6.9.70.jar

@for /R .\lib %%I IN ("*.jar") DO @call :add .\lib\%%~nI.jar

@java^
 -Xmx4G^
 -classpath "%X%"^
 -Djava.library.path=.//lib^
 -Djava.system.class.loader=org.processmining.framework.util.ProMClassLoader^
 -Djava.awt.headless=true^
 -Djava.util.Arrays.useLegacyMergeSort=true^
 org.processmining.contexts.cli.CLI^
 %1 %2

set X=
