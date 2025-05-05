plugins {
    id("java")
    kotlin("jvm") version "2.1.20"
    application
}

group = "de.mr_pine"
version = "1.0-SNAPSHOT"

application {
    mainClass = "de.mr_pine.c0ne.MainKt"
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.jspecify:jspecify:1.0.0")
    implementation("com.github.ajalt.clikt:clikt:5.0.3")
    testImplementation(kotlin("test"))
    testImplementation(platform("org.junit:junit-bom:5.10.0"))
    testImplementation("org.junit.jupiter:junit-jupiter")
}

tasks.test {
    useJUnitPlatform()
}

java {
    toolchain.languageVersion = JavaLanguageVersion.of(23)
}