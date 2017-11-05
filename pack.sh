set -e
MSBuild.exe Mom.sln /t:Clean /p:Configuration=Release
MSBuild.exe Mom.sln /t:Mom /p:Configuration=Release
mkdir -p tmp
rm -f tmp/*.nupkg
(cd Mom && paket pack ../tmp)
paket push --url https://www.myget.org/F/pragmatrix --endpoint /api/v2/package --api-key $MYGETAPIKEY tmp/*.nupkg
