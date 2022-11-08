# antoniosbarotsis/posharp-veritas
# Packages all project dependencies

FROM openjdk:17-jdk-slim

RUN apt-get update -y && \
  apt-get install -y gcc make curl dos2unix lsb-release wget software-properties-common gnupg

COPY llvm.sh llvm.sh
RUN chmod +x llvm.sh
RUN dos2unix llvm.sh
RUN ./llvm.sh 15

RUN mv /usr/bin/llc-15 /usr/bin/llc

COPY *.gradle gradle.* gradlew ./
COPY gradle/ ./gradle/
