pipeline {
  agent {
      label 'agent1'
  }
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
              sh 'git clone https://github.com/ashinn/chibi-scheme.git --depth=1 || true'
              sh 'make -j8 -C chibi-scheme'
              sh 'make -j8 -C chibi-scheme install'
              sh 'snow-chibi install retropikzel.compile-r7rs'
            }
          }
          stage('tap') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'snow-chibi install --impls=chibi srfi.64'
              sh 'snow-chibi install --impls=chibi retropikzel.tap'
              sh 'COMPILE_R7RS=chibi compile-r7rs -o tap-test-program retropikzel/tap/test.scm'
              sh './tap-test-program'
            }
          }
        }
      }
    }
    stage('sagittarius') {
      agent {
        docker {
          image 'schemers/sagittarius:head'
          reuseNode true
          args '--user=root'
        }
      }
      steps {
        script {
          stage('init') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev'
              sh 'git clone https://github.com/ashinn/chibi-scheme.git --depth=1 || true'
              sh 'make -j8 -C chibi-scheme'
              sh 'make -j8 -C chibi-scheme install'
              sh 'snow-chibi install retropikzel.compile-r7rs'
            }
          }
          stage('tap') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'snow-chibi install --impls=sagittarius srfi.64'
              sh 'snow-chibi install --impls=sagittarius retropikzel.tap'
              sh 'COMPILE_R7RS=sagittarius compile-r7rs -o tap-test-program retropikzel/tap/test.scm'
              sh './tap-test-program'
            }
          }
        }
      }
    }
  }
}
