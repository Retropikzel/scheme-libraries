pipeline {
    agent {
        dockerfile {
            label 'docker-x86_64'
            filename 'Dockerfile.jenkins'
            args '--user=root --privileged -v /var/run/docker.sock:/var/run/docker.sock'
            reuseNode true
        }
    }

    options {
        disableConcurrentBuilds()
        buildDiscarder(logRotator(numToKeepStr: '10', artifactNumToKeepStr: '10'))
    }

    parameters {
        string(name: 'R6RS_SCHEMES', defaultValue: 'capyscheme chezscheme guile ikarus ironscheme loko mosh racket sagittarius ypsilon', description: '')
        string(name: 'R7RS_SCHEMES', defaultValue: 'capyscheme chibi chicken cyclone foment gauche gambit guile kawa larceny loko meevax mit-scheme mosh racket sagittarius skint stklos tr7 ypsilon', description: '')
        string(name: 'LIBRARIES', defaultValue: 'ctrf mouth string url-encoding', description: '')
    }

    stages {
        stage('R6RS tests') {
            steps {
                script {
                    params.LIBRARIES.split().each { LIBRARY ->
                        stage("${LIBRARY}") {
                            params.R6RS_SCHEMES.split().each { SCHEME ->
                                stage("${SCHEME}") {
                                        catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                        sh "timeout 600 make SCHEME=${SCHEME} LIBRARY=${LIBRARY} RNRS=r6rs run-test-docker"
                                        archiveArtifacts(artifacts: "*.json", allowEmptyArchive: true, fingerprint: true)
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        stage('R7RS tests') {
            steps {
                script {
                    params.LIBRARIES.split().each { LIBRARY ->
                        stage("${LIBRARY}") {
                            params.R7RS_SCHEMES.split().each { SCHEME ->
                                stage("${SCHEME}") {
                                        catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                        sh "timeout 600 make SCHEME=${SCHEME} LIBRARY=${LIBRARY} RNRS=r7rs run-test-docker"
                                        archiveArtifacts(artifacts: "*.json", allowEmptyArchive: true, fingerprint: true)
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    post {
        success {
            publishCtrfResults: "*.json"
        }
    }
}
