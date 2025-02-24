set -ex
dotnet tool restore
dotnet clean -c Release
rm -f Mom/bin/Release/*.nupkg

# 20250224 Moved to dotnet paket pack after attempting to require only FSharp.Core >= 6.0.3 and trying to add an appropriate nuspec file, which screwed things up completely.
dotnet build -c Release Mom
dotnet paket pack Mom/bin/Release
# dotnet pack -c Release Mom

dotnet nuget push Mom/bin/Release/Mom.*.nupkg --source https://www.myget.org/F/pragmatrix --api-key $MYGETAPIKEY
