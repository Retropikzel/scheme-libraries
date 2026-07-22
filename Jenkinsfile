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
        stage('Makings') {
            agent {
                docker {
                    image "schemers/chibi:head"
                    reuseNode true
                    args '--user=root'
                }
            }
            steps {
                script {
                    files = findFiles(glob: 'makings/*.mks')
                    files.each { file ->
                        stage("${file}") {
                            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                sh "ls"
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
