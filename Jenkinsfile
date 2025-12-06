pipeline {
    agent {
        dockerfile {
            label 'docker-x86_64'
            image 'Dockerfile.jenkins'
            args '--user=root --privileged -v /var/run/docker.sock:/var/run/docker.sock'
            reuseNode true
        }
    }

    options {
        disableConcurrentBuilds()
        buildDiscarder(logRotator(numToKeepStr: '10', artifactNumToKeepStr: '10'))
    }

    parameters {
        string(name: 'LIBRARIES', defaultValue: 'cgi', description: '')
    }

    stages {
        stage('Tests') {
            steps {
                script {
                    def implementations = sh(script: 'compile-r7rs --list-r7rs-schemes', returnStdout: true).split()

                    params.LIBRARIES.split().each { LIBRARY ->
                        stage("${LIBRARY}") {
                            parallel implementations.collectEntries { SCHEME ->
                                [(SCHEME): {
                                    stage("${SCHEME}") {
                                        catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                            sh "make SCHEME=${SCHEME} clean test-docker"
                                            archiveArtifacts artifacts: 'tmp/*/*.log', fingerprint: true
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
