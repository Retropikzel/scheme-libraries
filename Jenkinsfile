pipeline {
  agent any
  options {
    disableConcurrentBuilds()
    buildDiscarder(logRotator(numToKeepStr: '10', artifactNumToKeepStr: '10'))
  }
  stages {
    stage('chibi') {
      agent {
        docker {
          image 'schemers/chibi:head'
          reuseNode true
          args '--user=root'
        }
      }
      steps {
        script {
          stage('init') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev'
              sh 'git clone https://github.com/ashinn/chibi-scheme.git --depth=1'
              sh 'make -j8 -C chibi-scheme'
              sh 'make -j8 -C chibi-scheme install'
              sh 'snow-chibi install retropikzel.compile-r7rs'
            }
          }
          stage('tap') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'COMPILE_R7RS=chibi compile-r7rs '
            }
          }
        }
      }
    }
  }
}
