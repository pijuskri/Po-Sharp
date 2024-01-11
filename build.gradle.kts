plugins {
    scala
    application
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.scala-lang:scala3-library_3:3.3.1")
    implementation("com.google.guava:guava:31.1-jre")
    implementation("com.lihaoyi:fastparse_3:3.0.2")
    //implementation("co.blocke:scala-reflection_3:2.0.0")
}
