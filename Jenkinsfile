pipeline {
    agent {
        dockerfile {
            label 'docker-x86_64'
            filename 'Dockerfile.jenkins'
            args '-t --user=root --privileged -v /var/run/docker.sock:/var/run/docker.sock'
            reuseNode true
        }
    }

    triggers {
      GenericTrigger(
        genericVariables: [[key: 'ref', value: '$.ref']],
        causeString: 'Triggered on $ref',
        printContributedVariables: true,
        printPostContent: true,
        silentResponse: false,
        shouldNotFlatten: false,
        regexpFilterText: '$ref',
        regexpFilterExpression: 'refs/heads/' + BRANCH_NAME
      )
    }

    options {
        disableConcurrentBuilds()
        buildDiscarder(logRotator(numToKeepStr: '10', artifactNumToKeepStr: '10'))
    }

    environment {
        R6RS_SCHEMES='capyscheme chezscheme guile ikarus ironscheme loko mosh racket sagittarius ypsilon'
        R7RS_SCHEMES='capyscheme chibi chicken cyclone foment gauche gambit kawa loko meevax mosh racket sagittarius skint stklos tr7 ypsilon'
        LIBRARIES='tap junit debug lambda-utils ctrf mouth string url-encoding leb128 hardware-info'
    }

    stages {
        stage('Parallel') {
            parallel {
                stage('R6RS tests') {
                    steps {
                        script {
                            env.LIBRARIES.split().each { LIBRARY ->
                                stage("${LIBRARY}") {
                                    env.R6RS_SCHEMES.split().each { SCHEME ->
                                        stage("${SCHEME}") {
                                            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                                sh "timeout 600 make SCHEME=${SCHEME} LIBRARY=${LIBRARY} RNRS=r6rs test-docker"
                                                junit ".tmp/*/*/*.xml"
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
                            env.LIBRARIES.split().each { LIBRARY ->
                                stage("${LIBRARY}") {
                                    env.R7RS_SCHEMES.split().each { SCHEME ->
                                        stage("${SCHEME}") {
                                            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                                sh "timeout 600 make SCHEME=${SCHEME} LIBRARY=${LIBRARY} RNRS=r7rs test-docker"
                                                junit ".tmp/*/*/*.xml"
                                            }
                                        }
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
        always {
            cleanWs()
        }
    }
}
