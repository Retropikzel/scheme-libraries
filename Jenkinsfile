pipeline {
    agent {
        dockerfile {
            label 'agent1'
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
        string(name: 'LIBRARIES', defaultValue: 'ctrf', description: '')
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
                                            sh "make SCHEME=${SCHEME} DOCKER_QUIET="" test-r6rs-docker"
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
                    def implementations = sh(script: 'compile-scheme --list-r7rs except', returnStdout: true).split()
                    params.LIBRARIES.split().each { LIBRARY ->
                        stage("${LIBRARY}") {
                            parallel implementations.collectEntries { SCHEME ->
                                [(SCHEME): {
                                    stage("${SCHEME}") {
                                        catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                            sh "make SCHEME=${SCHEME} DOCKER_QUIET="" test-r7rs-docker"
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
