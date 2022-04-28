plugins {
    scala
    application
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.scala-lang:scala-library:2.13.6")
    implementation("com.google.guava:guava:30.1.1-jre")
    implementation("com.lihaoyi:fastparse_2.13:2.3.3")
}

application {
    mainClass.set("scala.Main")
}