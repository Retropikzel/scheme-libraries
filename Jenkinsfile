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

    stages {
        stage('chibi') {
            agent {
                docker {
                    image "schemers/chibi:head"
                    reuseNode true
                    args '--user=root'
                }
            }
            steps {
                script {
                    '''
                    apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev
                    git clone https://github.com/ashinn/chibi-scheme.git --depth=1
                    make -j8 -C chibi-scheme
                    make -j8 -C chibi-scheme install
                    snow-chibi install retropikzel.compile-r7rs
                    make SCHEME=chibi LIBRARY=tap all install test
                    '''.split("\n").each { cmd ->
                        stage("wip") {
                            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                sh "${cmd}"
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
