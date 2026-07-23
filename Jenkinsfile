pipeline {
  agent {
    label 'docker-x86_64'
  }
  options {
    disableConcurrentBuilds()
    buildDiscarder(logRotator(numToKeepStr: '10', artifactNumToKeepStr: '10'))
  }
  stages {
    stage('capyscheme') {
      agent {
        docker {
          image 'schemers/capyscheme:head'
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
              sh 'make SCHEME=capyscheme LIBRARY=tap all install test'
            }
          }
          stage('junit') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=capyscheme LIBRARY=junit all install test'
            }
          }
          stage('ctrf') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=capyscheme LIBRARY=ctrf all install test'
            }
          }
          stage('mouth') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=capyscheme LIBRARY=mouth all install test'
            }
          }
          stage('string') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=capyscheme LIBRARY=string all install test'
            }
          }
          stage('url-encoding') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=capyscheme LIBRARY=url-encoding all install test'
            }
          }
          stage('debug') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=capyscheme LIBRARY=debug all install test'
            }
          }
          stage('leb128') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=capyscheme LIBRARY=leb128 all install test'
            }
          }
          stage('hardware-info') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=capyscheme LIBRARY=hardware-info all install test'
            }
          }
          stage('lambda-utils') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=capyscheme LIBRARY=lambda-utils all install test'
            }
          }
        }
      }
    }
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
              sh 'make SCHEME=chibi LIBRARY=tap all install test'
            }
          }
          stage('junit') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=chibi LIBRARY=junit all install test'
            }
          }
          stage('ctrf') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=chibi LIBRARY=ctrf all install test'
            }
          }
          stage('mouth') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=chibi LIBRARY=mouth all install test'
            }
          }
          stage('string') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=chibi LIBRARY=string all install test'
            }
          }
          stage('url-encoding') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=chibi LIBRARY=url-encoding all install test'
            }
          }
          stage('debug') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=chibi LIBRARY=debug all install test'
            }
          }
          stage('leb128') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=chibi LIBRARY=leb128 all install test'
            }
          }
          stage('hardware-info') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=chibi LIBRARY=hardware-info all install test'
            }
          }
          stage('lambda-utils') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=chibi LIBRARY=lambda-utils all install test'
            }
          }
        }
      }
    }
    stage('chicken') {
      agent {
        docker {
          image 'schemers/chicken:head'
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
              sh 'make SCHEME=chicken LIBRARY=tap all install test'
            }
          }
          stage('junit') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=chicken LIBRARY=junit all install test'
            }
          }
          stage('ctrf') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=chicken LIBRARY=ctrf all install test'
            }
          }
          stage('mouth') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=chicken LIBRARY=mouth all install test'
            }
          }
          stage('string') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=chicken LIBRARY=string all install test'
            }
          }
          stage('url-encoding') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=chicken LIBRARY=url-encoding all install test'
            }
          }
          stage('debug') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=chicken LIBRARY=debug all install test'
            }
          }
          stage('leb128') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=chicken LIBRARY=leb128 all install test'
            }
          }
          stage('hardware-info') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=chicken LIBRARY=hardware-info all install test'
            }
          }
          stage('lambda-utils') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=chicken LIBRARY=lambda-utils all install test'
            }
          }
        }
      }
    }
    stage('cyclone') {
      agent {
        docker {
          image 'schemers/cyclone:head'
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
              sh 'make SCHEME=cyclone LIBRARY=tap all install test'
            }
          }
          stage('junit') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=cyclone LIBRARY=junit all install test'
            }
          }
          stage('ctrf') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=cyclone LIBRARY=ctrf all install test'
            }
          }
          stage('mouth') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=cyclone LIBRARY=mouth all install test'
            }
          }
          stage('string') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=cyclone LIBRARY=string all install test'
            }
          }
          stage('url-encoding') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=cyclone LIBRARY=url-encoding all install test'
            }
          }
          stage('debug') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=cyclone LIBRARY=debug all install test'
            }
          }
          stage('leb128') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=cyclone LIBRARY=leb128 all install test'
            }
          }
          stage('hardware-info') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=cyclone LIBRARY=hardware-info all install test'
            }
          }
          stage('lambda-utils') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=cyclone LIBRARY=lambda-utils all install test'
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
              sh 'make SCHEME=sagittarius LIBRARY=tap all install test'
            }
          }
          stage('junit') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=sagittarius LIBRARY=junit all install test'
            }
          }
          stage('ctrf') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=sagittarius LIBRARY=ctrf all install test'
            }
          }
          stage('mouth') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=sagittarius LIBRARY=mouth all install test'
            }
          }
          stage('string') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=sagittarius LIBRARY=string all install test'
            }
          }
          stage('url-encoding') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=sagittarius LIBRARY=url-encoding all install test'
            }
          }
          stage('debug') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=sagittarius LIBRARY=debug all install test'
            }
          }
          stage('leb128') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=sagittarius LIBRARY=leb128 all install test'
            }
          }
          stage('hardware-info') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=sagittarius LIBRARY=hardware-info all install test'
            }
          }
          stage('lambda-utils') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=sagittarius LIBRARY=lambda-utils all install test'
            }
          }
        }
      }
    }
    stage('foment') {
      agent {
        docker {
          image 'schemers/foment:head'
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
              sh 'make SCHEME=foment LIBRARY=tap all install test'
            }
          }
          stage('junit') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=foment LIBRARY=junit all install test'
            }
          }
          stage('ctrf') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=foment LIBRARY=ctrf all install test'
            }
          }
          stage('mouth') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=foment LIBRARY=mouth all install test'
            }
          }
          stage('string') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=foment LIBRARY=string all install test'
            }
          }
          stage('url-encoding') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=foment LIBRARY=url-encoding all install test'
            }
          }
          stage('debug') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=foment LIBRARY=debug all install test'
            }
          }
          stage('leb128') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=foment LIBRARY=leb128 all install test'
            }
          }
          stage('hardware-info') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=foment LIBRARY=hardware-info all install test'
            }
          }
          stage('lambda-utils') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=foment LIBRARY=lambda-utils all install test'
            }
          }
        }
      }
    }
    stage('gauche') {
      agent {
        docker {
          image 'schemers/gauche:head'
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
              sh 'make SCHEME=gauche LIBRARY=tap all install test'
            }
          }
          stage('junit') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=gauche LIBRARY=junit all install test'
            }
          }
          stage('ctrf') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=gauche LIBRARY=ctrf all install test'
            }
          }
          stage('mouth') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=gauche LIBRARY=mouth all install test'
            }
          }
          stage('string') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=gauche LIBRARY=string all install test'
            }
          }
          stage('url-encoding') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=gauche LIBRARY=url-encoding all install test'
            }
          }
          stage('debug') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=gauche LIBRARY=debug all install test'
            }
          }
          stage('leb128') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=gauche LIBRARY=leb128 all install test'
            }
          }
          stage('hardware-info') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=gauche LIBRARY=hardware-info all install test'
            }
          }
          stage('lambda-utils') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=gauche LIBRARY=lambda-utils all install test'
            }
          }
        }
      }
    }
    stage('kawa') {
      agent {
        docker {
          image 'schemers/kawa:head'
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
              sh 'make SCHEME=kawa LIBRARY=tap all install test'
            }
          }
          stage('junit') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=kawa LIBRARY=junit all install test'
            }
          }
          stage('ctrf') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=kawa LIBRARY=ctrf all install test'
            }
          }
          stage('mouth') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=kawa LIBRARY=mouth all install test'
            }
          }
          stage('string') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=kawa LIBRARY=string all install test'
            }
          }
          stage('url-encoding') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=kawa LIBRARY=url-encoding all install test'
            }
          }
          stage('debug') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=kawa LIBRARY=debug all install test'
            }
          }
          stage('leb128') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=kawa LIBRARY=leb128 all install test'
            }
          }
          stage('hardware-info') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=kawa LIBRARY=hardware-info all install test'
            }
          }
          stage('lambda-utils') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=kawa LIBRARY=lambda-utils all install test'
            }
          }
        }
      }
    }
    stage('loko') {
      agent {
        docker {
          image 'schemers/loko:head'
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
              sh 'make SCHEME=loko LIBRARY=tap all install test'
            }
          }
          stage('junit') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=loko LIBRARY=junit all install test'
            }
          }
          stage('ctrf') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=loko LIBRARY=ctrf all install test'
            }
          }
          stage('mouth') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=loko LIBRARY=mouth all install test'
            }
          }
          stage('string') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=loko LIBRARY=string all install test'
            }
          }
          stage('url-encoding') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=loko LIBRARY=url-encoding all install test'
            }
          }
          stage('debug') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=loko LIBRARY=debug all install test'
            }
          }
          stage('leb128') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=loko LIBRARY=leb128 all install test'
            }
          }
          stage('hardware-info') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=loko LIBRARY=hardware-info all install test'
            }
          }
          stage('lambda-utils') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=loko LIBRARY=lambda-utils all install test'
            }
          }
        }
      }
    }
    stage('meevax') {
      agent {
        docker {
          image 'schemers/meevax:head'
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
              sh 'make SCHEME=meevax LIBRARY=tap all install test'
            }
          }
          stage('junit') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=meevax LIBRARY=junit all install test'
            }
          }
          stage('ctrf') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=meevax LIBRARY=ctrf all install test'
            }
          }
          stage('mouth') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=meevax LIBRARY=mouth all install test'
            }
          }
          stage('string') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=meevax LIBRARY=string all install test'
            }
          }
          stage('url-encoding') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=meevax LIBRARY=url-encoding all install test'
            }
          }
          stage('debug') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=meevax LIBRARY=debug all install test'
            }
          }
          stage('leb128') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=meevax LIBRARY=leb128 all install test'
            }
          }
          stage('hardware-info') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=meevax LIBRARY=hardware-info all install test'
            }
          }
          stage('lambda-utils') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=meevax LIBRARY=lambda-utils all install test'
            }
          }
        }
      }
    }
    stage('mit-scheme') {
      agent {
        docker {
          image 'schemers/mit-scheme:head'
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
              sh 'make SCHEME=mit-scheme LIBRARY=tap all install test'
            }
          }
          stage('junit') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=mit-scheme LIBRARY=junit all install test'
            }
          }
          stage('ctrf') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=mit-scheme LIBRARY=ctrf all install test'
            }
          }
          stage('mouth') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=mit-scheme LIBRARY=mouth all install test'
            }
          }
          stage('string') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=mit-scheme LIBRARY=string all install test'
            }
          }
          stage('url-encoding') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=mit-scheme LIBRARY=url-encoding all install test'
            }
          }
          stage('debug') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=mit-scheme LIBRARY=debug all install test'
            }
          }
          stage('leb128') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=mit-scheme LIBRARY=leb128 all install test'
            }
          }
          stage('hardware-info') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=mit-scheme LIBRARY=hardware-info all install test'
            }
          }
          stage('lambda-utils') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=mit-scheme LIBRARY=lambda-utils all install test'
            }
          }
        }
      }
    }
    stage('mosh') {
      agent {
        docker {
          image 'schemers/mosh:head'
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
              sh 'make SCHEME=mosh LIBRARY=tap all install test'
            }
          }
          stage('junit') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=mosh LIBRARY=junit all install test'
            }
          }
          stage('ctrf') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=mosh LIBRARY=ctrf all install test'
            }
          }
          stage('mouth') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=mosh LIBRARY=mouth all install test'
            }
          }
          stage('string') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=mosh LIBRARY=string all install test'
            }
          }
          stage('url-encoding') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=mosh LIBRARY=url-encoding all install test'
            }
          }
          stage('debug') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=mosh LIBRARY=debug all install test'
            }
          }
          stage('leb128') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=mosh LIBRARY=leb128 all install test'
            }
          }
          stage('hardware-info') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=mosh LIBRARY=hardware-info all install test'
            }
          }
          stage('lambda-utils') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=mosh LIBRARY=lambda-utils all install test'
            }
          }
        }
      }
    }
    stage('racket') {
      agent {
        docker {
          image 'schemers/racket:head'
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
              sh 'make SCHEME=racket LIBRARY=tap all install test'
            }
          }
          stage('junit') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=racket LIBRARY=junit all install test'
            }
          }
          stage('ctrf') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=racket LIBRARY=ctrf all install test'
            }
          }
          stage('mouth') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=racket LIBRARY=mouth all install test'
            }
          }
          stage('string') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=racket LIBRARY=string all install test'
            }
          }
          stage('url-encoding') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=racket LIBRARY=url-encoding all install test'
            }
          }
          stage('debug') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=racket LIBRARY=debug all install test'
            }
          }
          stage('leb128') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=racket LIBRARY=leb128 all install test'
            }
          }
          stage('hardware-info') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=racket LIBRARY=hardware-info all install test'
            }
          }
          stage('lambda-utils') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=racket LIBRARY=lambda-utils all install test'
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
              sh 'make SCHEME=sagittarius LIBRARY=tap all install test'
            }
          }
          stage('junit') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=sagittarius LIBRARY=junit all install test'
            }
          }
          stage('ctrf') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=sagittarius LIBRARY=ctrf all install test'
            }
          }
          stage('mouth') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=sagittarius LIBRARY=mouth all install test'
            }
          }
          stage('string') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=sagittarius LIBRARY=string all install test'
            }
          }
          stage('url-encoding') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=sagittarius LIBRARY=url-encoding all install test'
            }
          }
          stage('debug') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=sagittarius LIBRARY=debug all install test'
            }
          }
          stage('leb128') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=sagittarius LIBRARY=leb128 all install test'
            }
          }
          stage('hardware-info') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=sagittarius LIBRARY=hardware-info all install test'
            }
          }
          stage('lambda-utils') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=sagittarius LIBRARY=lambda-utils all install test'
            }
          }
        }
      }
    }
    stage('skint') {
      agent {
        docker {
          image 'schemers/skint:head'
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
              sh 'make SCHEME=skint LIBRARY=tap all install test'
            }
          }
          stage('junit') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=skint LIBRARY=junit all install test'
            }
          }
          stage('ctrf') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=skint LIBRARY=ctrf all install test'
            }
          }
          stage('mouth') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=skint LIBRARY=mouth all install test'
            }
          }
          stage('string') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=skint LIBRARY=string all install test'
            }
          }
          stage('url-encoding') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=skint LIBRARY=url-encoding all install test'
            }
          }
          stage('debug') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=skint LIBRARY=debug all install test'
            }
          }
          stage('leb128') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=skint LIBRARY=leb128 all install test'
            }
          }
          stage('hardware-info') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=skint LIBRARY=hardware-info all install test'
            }
          }
          stage('lambda-utils') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=skint LIBRARY=lambda-utils all install test'
            }
          }
        }
      }
    }
    stage('stklos') {
      agent {
        docker {
          image 'schemers/stklos:head'
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
              sh 'make SCHEME=stklos LIBRARY=tap all install test'
            }
          }
          stage('junit') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=stklos LIBRARY=junit all install test'
            }
          }
          stage('ctrf') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=stklos LIBRARY=ctrf all install test'
            }
          }
          stage('mouth') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=stklos LIBRARY=mouth all install test'
            }
          }
          stage('string') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=stklos LIBRARY=string all install test'
            }
          }
          stage('url-encoding') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=stklos LIBRARY=url-encoding all install test'
            }
          }
          stage('debug') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=stklos LIBRARY=debug all install test'
            }
          }
          stage('leb128') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=stklos LIBRARY=leb128 all install test'
            }
          }
          stage('hardware-info') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=stklos LIBRARY=hardware-info all install test'
            }
          }
          stage('lambda-utils') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=stklos LIBRARY=lambda-utils all install test'
            }
          }
        }
      }
    }
    stage('tr7') {
      agent {
        docker {
          image 'schemers/tr7:head'
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
              sh 'make SCHEME=tr7 LIBRARY=tap all install test'
            }
          }
          stage('junit') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=tr7 LIBRARY=junit all install test'
            }
          }
          stage('ctrf') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=tr7 LIBRARY=ctrf all install test'
            }
          }
          stage('mouth') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=tr7 LIBRARY=mouth all install test'
            }
          }
          stage('string') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=tr7 LIBRARY=string all install test'
            }
          }
          stage('url-encoding') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=tr7 LIBRARY=url-encoding all install test'
            }
          }
          stage('debug') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=tr7 LIBRARY=debug all install test'
            }
          }
          stage('leb128') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=tr7 LIBRARY=leb128 all install test'
            }
          }
          stage('hardware-info') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=tr7 LIBRARY=hardware-info all install test'
            }
          }
          stage('lambda-utils') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=tr7 LIBRARY=lambda-utils all install test'
            }
          }
        }
      }
    }
    stage('ypsilon') {
      agent {
        docker {
          image 'schemers/ypsilon:head'
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
              sh 'make SCHEME=ypsilon LIBRARY=tap all install test'
            }
          }
          stage('junit') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=ypsilon LIBRARY=junit all install test'
            }
          }
          stage('ctrf') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=ypsilon LIBRARY=ctrf all install test'
            }
          }
          stage('mouth') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=ypsilon LIBRARY=mouth all install test'
            }
          }
          stage('string') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=ypsilon LIBRARY=string all install test'
            }
          }
          stage('url-encoding') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=ypsilon LIBRARY=url-encoding all install test'
            }
          }
          stage('debug') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=ypsilon LIBRARY=debug all install test'
            }
          }
          stage('leb128') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=ypsilon LIBRARY=leb128 all install test'
            }
          }
          stage('hardware-info') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=ypsilon LIBRARY=hardware-info all install test'
            }
          }
          stage('lambda-utils') {
            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
              sh 'make SCHEME=ypsilon LIBRARY=lambda-utils all install test'
            }
          }
        }
      }
    }
  }
}
