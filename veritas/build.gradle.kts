plugins {
    scala
    application
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.scala-lang:scala3-library_3:3.3.1")
    implementation("org.reflections:reflections:0.10.2")
    implementation(project(":app"))
}

application {
    mainClass.set("core.Veritas")
}

task("runTests", JavaExec::class) {
    mainClass.set("core.Veritas")
    classpath = sourceSets["main"].runtimeClasspath

    shouldRunAfter(":app:build")
}

task("runCoverage", JavaExec::class) {
    mainClass.set("core.Veritas")
    classpath = sourceSets["main"].runtimeClasspath

    // If `extraArgs` exists, prepend it with "coverage" and set args to the result
    // else just set args to "coverage"
    if (project.hasProperty("extraArgs")) {
        val tmp = project.properties["extraArgs"].toString().split(",")

        args(listOf("coverage") + tmp)
    } else {
        args("coverage")
    }
}