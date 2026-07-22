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
                    def builds = readYaml file: 'buildconfig.yaml'
                    builds.each { build ->
                        stage("${build.name}") {
                            agent {
                                docker {
                                    image "${build.image}"
                                }
                            }
                            build.stages.each { stage ->
                                stage("${stage.name}") {
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
