# compile projects
dotnet build --configuration Release

# build docker image
docker rmi lambda-cek-webui
docker build -t lambda-cek-webui -f src-webui/Dockerfile .

# run container
# docker run -d -p 8080:8080 --name lambda-cek-webui lambda-cek-webui
