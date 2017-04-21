# install tomcat
# if [[ $UID != 0 ]]; then
#     echo "Please run this script with sudo:"
#     echo "sudo $0 $*"
#     exit 1
# fi

tomcatVersion=apache-tomcat-8.5.2
zipName=$tomcatVersion.zip
url="https://archive.apache.org/dist/tomcat/tomcat-8/v8.5.2/bin/$zipName"
tmpDir="/tmp"
outputDir="/home/marek/software/tomcat"

binDir=$outputDir/bin
tmpZip=$tmpDir/$zipName
userConfiguration=$outputDir/conf/tomcat-users.xml


wget $url -O $tmpDir/$zipName
echo "Downloaded $url"
unzip $tmpZip -d $tmpDir
echo "unzipped $tmpZip to $tmpDir"
mkdir $outputDir
mv $tmpDir/$tomcatVersion/* $outputDir
chmod 777 $binDir/*.sh
ln -s $binDir/startup.sh /usr/bin/tomcatup
ln -s $binDir/shutdown.sh /usr/bin/tomcatdown
rm -f $userConfiguration
cp ~/Dropbox/RInstal/scripts/tomcat-users-xml $userConfiguration
echo "Set permission"
rm -f $tmpZip
echo "Removed $tmpZip"