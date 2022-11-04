plugins {
    scala
    application
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.scala-lang:scala-library:2.13.10")
    implementation("com.google.guava:guava:31.1-jre")
    implementation("com.lihaoyi:fastparse_2.13:2.3.3")
}
