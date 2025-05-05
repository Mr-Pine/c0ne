plugins {
    id("java")
    application
}

group = "edu.kit.kastel.logic"
version = "1.0-SNAPSHOT"

application {
    mainModule = "edu.kit.kastel.vads.compiler"
    mainClass = "edu.kit.kastel.vads.compiler.Main"
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.jspecify:jspecify:1.0.0")
    testImplementation(platform("org.junit:junit-bom:5.10.0"))
    testImplementation("org.junit.jupiter:junit-jupiter")
}

java {
    toolchain.languageVersion = JavaLanguageVersion.of(24)
}

tasks.test {
    useJUnitPlatform()
}