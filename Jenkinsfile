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
                    def config = readYAML file: 'builds.yaml'
                    config.stages.each { item ->
                        stage("${item.name}") {
                            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                sh "${item.cmd}"
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
