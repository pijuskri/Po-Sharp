plugins {
    scala
    application
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.scala-lang:scala-library:2.13.8")
    implementation("org.reflections:reflections:0.10.2")
    implementation("org.scala-lang:scala-reflect:2.13.8")
    implementation(project(":app"))
}

application {
    mainClass.set("core.Veritas")
}

task("runTests", JavaExec::class) {
    mainClass.set("core.Veritas")
    classpath = sourceSets["main"].runtimeClasspath

    shouldRunAfter(":app:build")
    finalizedBy("run")
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