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
        stage('Build') {
            steps {
                script {
                    def config = readYaml("buildconfig.yaml")
                    config.schemes.each { scheme ->
                        stage("${scheme}") {
                            agent {
                                docker {
                                    image "schemers/${scheme}:${scheme.docker-tag}"
                                }
                            }
                            environment {
                                COMPILE_R7RS="${scheme}"
                                SCHEME="${scheme}"
                            }
                            scheme.stages.each { stage ->
                                stage("${stage}") {
                                    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                        sh "${stage.cmd}"
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
