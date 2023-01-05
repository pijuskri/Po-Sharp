#sudo apt update

sudo apt-get update -y && \
  sudo apt-get install -y gcc make curl dos2unix lsb-release wget software-properties-common gnupg

sudo chmod +x llvm.sh
dos2unix llvm.sh
sudo ./llvm.sh 15
sudo mv /usr/bin/llc-15 /usr/bin/llc
# sudo mv /usr/bin/clang-15 /usr/bin/clang

sudo apt install openjdk-17-jdk openjdk-17-jre

sudo chmod +x build-llvm.sh
dos2unix build-llvm.sh

#./gradlew app:run $args --no-daemon