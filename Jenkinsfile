pipeline {
  agent {
    label 'docker-x86_64'
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
          def stages = get_chibi_stages()
          parallel stages
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
          def stages = get_sagittarius_stages()
          parallel stages
        }
      }
    }
  }
}

def get_chibi_stages() {
  def stages = []
          stages['init'] = stage('init') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev'
              sh 'git clone https://github.com/ashinn/chibi-scheme.git --depth=1 || true'
              sh 'make -j8 -C chibi-scheme'
              sh 'make -j8 -C chibi-scheme install'
              sh 'snow-chibi install retropikzel.compile-r7rs'
            }
          }
          stages['tap'] = stage('tap') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=chibi LIBRARY=tap all install test'
            }
          }
          stages['debug'] = stage('debug') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=chibi LIBRARY=debug all install test'
            }
          }
  return stages
}

def get_sagittarius_stages() {
  def stages = []
          stages['init'] = stage('init') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev'
              sh 'git clone https://github.com/ashinn/chibi-scheme.git --depth=1 || true'
              sh 'make -j8 -C chibi-scheme'
              sh 'make -j8 -C chibi-scheme install'
              sh 'snow-chibi install retropikzel.compile-r7rs'
            }
          }
          stages['tap'] = stage('tap') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=sagittarius LIBRARY=tap all install test'
            }
          }
          stages['debug'] = stage('debug') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=sagittarius LIBRARY=debug all install test'
            }
          }
  return stages
}

