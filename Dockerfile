# antoniosbarotsis/posharp-veritas
# Packages all dependencies needed to run the tests

FROM openjdk:17-jdk-slim

RUN apt-get update && apt-get install -y \
  make \
  llvm \
  gcc \
  curl \
  nano \
  && apt-get clean \
  && rm -rf /var/cache/apt/archives /var/lib/apt/lists/*

# Cache dependencies hopefully
COPY *.gradle gradle.* gradlew ./
COPY gradle/ ./gradle/

# Make the build fail so the dependencies get resolved
RUN ./gradlew clean build --no-daemon 2>&1 || true
