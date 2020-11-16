set -e
dotnet clean -c Release
rm -f Mom/bin/Release/*.nupkg
dotnet pack -c Release Mom
dotnet nuget push Mom/bin/Release/Mom.*.nupkg --source https://www.myget.org/F/pragmatrix --api-key $MYGETAPIKEY
