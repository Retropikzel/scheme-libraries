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
        string(name: 'LIBRARIES', defaultValue: 'ctrf mouth string url-encoding', description: '')
    }

    stages {
        stage('R6RS tests') {
            steps {
                script {
                    def implementations = sh(script: 'compile-scheme --list-r6rs-except ironscheme larceny', returnStdout: true).split()
                    params.LIBRARIES.split().each { LIBRARY ->
                        stage("${LIBRARY}") {
                            parallel implementations.collectEntries { SCHEME ->
                                [(SCHEME): {
                                    stage("${SCHEME}") {
                                        catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                            sh "timeout 600 make SCHEME=${SCHEME} LIBRARY=${LIBRARY} RNRS=r6rs run-test-docker"
                                        }
                                    }
                                }]
                            }
                        }
                    }
                }
            }
        }
        stage('R7RS tests') {
            steps {
                script {
                    def implementations = sh(script: 'compile-scheme --list-r7rs-except capyscheme cyclone foment gambit meevax skint larceny tr7', returnStdout: true).split()
                    params.LIBRARIES.split().each { LIBRARY ->
                        stage("${LIBRARY}") {
                            parallel implementations.collectEntries { SCHEME ->
                                [(SCHEME): {
                                    stage("${SCHEME}") {
                                        catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                            sh "timeout 600 make SCHEME=${SCHEME} LIBRARY=${LIBRARY} RNRS=r7rs run-test-docker"
                                        }
                                    }
                                }]
                            }
                        }
                    }
                }
            }
        }
    }
}
