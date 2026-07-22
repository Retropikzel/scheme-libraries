pipeline {
    agent {
        label 'docker-x86_64'
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
        BUILDS="chibi"
    }

    stages {
        stage('Builds') {
            steps {
                script {
                    env.BUILDS.split().each { build ->
                        stage("${build}") {
                            agent {
                                docker {
                                    image "schemers/chibi:head"
                                    reuseNode true
                                    args '--user=root'
                                }
                            }
                            steps {
                                sh "apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev"
                                sh "git clone https://github.com/ashinn/chibi-scheme.git --depth=1"
                                sh "make -j8 -C chibi-scheme"
                                sh "make -j8 -C chibi-scheme install"
                                sh "snow-chibi install retropikzel.compile-r7rs"
                                catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                    sh "make SCHEME=chibi LIBRARY=tap all install test"
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
            cleanWs()
        }
    }
}
