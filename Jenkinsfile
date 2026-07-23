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
          get_capyscheme_stages()
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
          get_chibi_stages()
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
          get_chicken_stages()
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
          get_foment_stages()
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
          get_gauche_stages()
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
          get_kawa_stages()
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
          get_loko_stages()
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
          get_meevax_stages()
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
          get_mit_scheme_stages()
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
          get_mosh_stages()
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
          get_racket_stages()
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
          get_sagittarius_stages()
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
          get_skint_stages()
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
          get_stklos_stages()
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
          get_tr7_stages()
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
          get_ypsilon_stages()
        }
      }
    }
  }
}

def get_capyscheme_stages() {
  def stages = []
  stages.plus(stage('init') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev"
      sh "git clone https://github.com/ashinn/chibi-scheme.git --depth=1 || true"
      sh "make -j8 -C chibi-scheme"
      sh "make -j8 -C chibi-scheme install"
      sh "snow-chibi install --always-yes retropikzel.compile-r7rs"
    }
  })
  stages.plus(stage('tap') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=capyscheme LIBRARY=tap all install test"
    }
  })
  stages.plus(stage('mouth') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=capyscheme LIBRARY=mouth all install test"
    }
  })
  stages.plus(stage('debug') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=capyscheme LIBRARY=debug all install test"
    }
  })
  stages.plus(stage('hardware-info') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=capyscheme LIBRARY=hardware-info all install test"
    }
  })
  stages.plus(stage('lambda-utils') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=capyscheme LIBRARY=lambda-utils all install test"
    }
  })
  return stages
}

def get_chibi_stages() {
  def stages = []
  stages.plus(stage('init') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev"
      sh "git clone https://github.com/ashinn/chibi-scheme.git --depth=1 || true"
      sh "make -j8 -C chibi-scheme"
      sh "make -j8 -C chibi-scheme install"
      sh "snow-chibi install --always-yes retropikzel.compile-r7rs"
    }
  })
  stages.plus(stage('tap') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=chibi LIBRARY=tap all install test"
    }
  })
  stages.plus(stage('mouth') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=chibi LIBRARY=mouth all install test"
    }
  })
  stages.plus(stage('debug') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=chibi LIBRARY=debug all install test"
    }
  })
  stages.plus(stage('hardware-info') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=chibi LIBRARY=hardware-info all install test"
    }
  })
  stages.plus(stage('lambda-utils') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=chibi LIBRARY=lambda-utils all install test"
    }
  })
  return stages
}

def get_chicken_stages() {
  def stages = []
  stages.plus(stage('init') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev"
      sh "git clone https://github.com/ashinn/chibi-scheme.git --depth=1 || true"
      sh "make -j8 -C chibi-scheme"
      sh "make -j8 -C chibi-scheme install"
      sh "snow-chibi install --always-yes retropikzel.compile-r7rs"
    }
  })
  stages.plus(stage('tap') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=chicken LIBRARY=tap all install test"
    }
  })
  stages.plus(stage('mouth') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=chicken LIBRARY=mouth all install test"
    }
  })
  stages.plus(stage('debug') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=chicken LIBRARY=debug all install test"
    }
  })
  stages.plus(stage('hardware-info') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=chicken LIBRARY=hardware-info all install test"
    }
  })
  stages.plus(stage('lambda-utils') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=chicken LIBRARY=lambda-utils all install test"
    }
  })
  return stages
}

def get_foment_stages() {
  def stages = []
  stages.plus(stage('init') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev"
      sh "git clone https://github.com/ashinn/chibi-scheme.git --depth=1 || true"
      sh "make -j8 -C chibi-scheme"
      sh "make -j8 -C chibi-scheme install"
      sh "snow-chibi install --always-yes retropikzel.compile-r7rs"
    }
  })
  stages.plus(stage('tap') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=foment LIBRARY=tap all install test"
    }
  })
  stages.plus(stage('mouth') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=foment LIBRARY=mouth all install test"
    }
  })
  stages.plus(stage('debug') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=foment LIBRARY=debug all install test"
    }
  })
  stages.plus(stage('hardware-info') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=foment LIBRARY=hardware-info all install test"
    }
  })
  stages.plus(stage('lambda-utils') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=foment LIBRARY=lambda-utils all install test"
    }
  })
  return stages
}

def get_gauche_stages() {
  def stages = []
  stages.plus(stage('init') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev"
      sh "git clone https://github.com/ashinn/chibi-scheme.git --depth=1 || true"
      sh "make -j8 -C chibi-scheme"
      sh "make -j8 -C chibi-scheme install"
      sh "snow-chibi install --always-yes retropikzel.compile-r7rs"
    }
  })
  stages.plus(stage('tap') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=gauche LIBRARY=tap all install test"
    }
  })
  stages.plus(stage('mouth') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=gauche LIBRARY=mouth all install test"
    }
  })
  stages.plus(stage('debug') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=gauche LIBRARY=debug all install test"
    }
  })
  stages.plus(stage('hardware-info') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=gauche LIBRARY=hardware-info all install test"
    }
  })
  stages.plus(stage('lambda-utils') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=gauche LIBRARY=lambda-utils all install test"
    }
  })
  return stages
}

def get_kawa_stages() {
  def stages = []
  stages.plus(stage('init') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev"
      sh "git clone https://github.com/ashinn/chibi-scheme.git --depth=1 || true"
      sh "make -j8 -C chibi-scheme"
      sh "make -j8 -C chibi-scheme install"
      sh "snow-chibi install --always-yes retropikzel.compile-r7rs"
    }
  })
  stages.plus(stage('tap') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=kawa LIBRARY=tap all install test"
    }
  })
  stages.plus(stage('mouth') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=kawa LIBRARY=mouth all install test"
    }
  })
  stages.plus(stage('debug') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=kawa LIBRARY=debug all install test"
    }
  })
  stages.plus(stage('hardware-info') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=kawa LIBRARY=hardware-info all install test"
    }
  })
  stages.plus(stage('lambda-utils') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=kawa LIBRARY=lambda-utils all install test"
    }
  })
  return stages
}

def get_loko_stages() {
  def stages = []
  stages.plus(stage('init') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev"
      sh "git clone https://github.com/ashinn/chibi-scheme.git --depth=1 || true"
      sh "make -j8 -C chibi-scheme"
      sh "make -j8 -C chibi-scheme install"
      sh "snow-chibi install --always-yes retropikzel.compile-r7rs"
    }
  })
  stages.plus(stage('tap') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=loko LIBRARY=tap all install test"
    }
  })
  stages.plus(stage('mouth') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=loko LIBRARY=mouth all install test"
    }
  })
  stages.plus(stage('debug') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=loko LIBRARY=debug all install test"
    }
  })
  stages.plus(stage('hardware-info') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=loko LIBRARY=hardware-info all install test"
    }
  })
  stages.plus(stage('lambda-utils') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=loko LIBRARY=lambda-utils all install test"
    }
  })
  return stages
}

def get_meevax_stages() {
  def stages = []
  stages.plus(stage('init') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev"
      sh "git clone https://github.com/ashinn/chibi-scheme.git --depth=1 || true"
      sh "make -j8 -C chibi-scheme"
      sh "make -j8 -C chibi-scheme install"
      sh "snow-chibi install --always-yes retropikzel.compile-r7rs"
    }
  })
  stages.plus(stage('tap') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=meevax LIBRARY=tap all install test"
    }
  })
  stages.plus(stage('mouth') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=meevax LIBRARY=mouth all install test"
    }
  })
  stages.plus(stage('debug') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=meevax LIBRARY=debug all install test"
    }
  })
  stages.plus(stage('hardware-info') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=meevax LIBRARY=hardware-info all install test"
    }
  })
  stages.plus(stage('lambda-utils') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=meevax LIBRARY=lambda-utils all install test"
    }
  })
  return stages
}

def get_mit_scheme_stages() {
  def stages = []
  stages.plus(stage('init') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev"
      sh "git clone https://github.com/ashinn/chibi-scheme.git --depth=1 || true"
      sh "make -j8 -C chibi-scheme"
      sh "make -j8 -C chibi-scheme install"
      sh "snow-chibi install --always-yes retropikzel.compile-r7rs"
    }
  })
  stages.plus(stage('tap') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=mit-scheme LIBRARY=tap all install test"
    }
  })
  stages.plus(stage('mouth') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=mit-scheme LIBRARY=mouth all install test"
    }
  })
  stages.plus(stage('debug') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=mit-scheme LIBRARY=debug all install test"
    }
  })
  stages.plus(stage('hardware-info') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=mit-scheme LIBRARY=hardware-info all install test"
    }
  })
  stages.plus(stage('lambda-utils') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=mit-scheme LIBRARY=lambda-utils all install test"
    }
  })
  return stages
}

def get_mosh_stages() {
  def stages = []
  stages.plus(stage('init') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev"
      sh "git clone https://github.com/ashinn/chibi-scheme.git --depth=1 || true"
      sh "make -j8 -C chibi-scheme"
      sh "make -j8 -C chibi-scheme install"
      sh "snow-chibi install --always-yes retropikzel.compile-r7rs"
    }
  })
  stages.plus(stage('tap') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=mosh LIBRARY=tap all install test"
    }
  })
  stages.plus(stage('mouth') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=mosh LIBRARY=mouth all install test"
    }
  })
  stages.plus(stage('debug') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=mosh LIBRARY=debug all install test"
    }
  })
  stages.plus(stage('hardware-info') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=mosh LIBRARY=hardware-info all install test"
    }
  })
  stages.plus(stage('lambda-utils') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=mosh LIBRARY=lambda-utils all install test"
    }
  })
  return stages
}

def get_racket_stages() {
  def stages = []
  stages.plus(stage('init') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev"
      sh "git clone https://github.com/ashinn/chibi-scheme.git --depth=1 || true"
      sh "make -j8 -C chibi-scheme"
      sh "make -j8 -C chibi-scheme install"
      sh "snow-chibi install --always-yes retropikzel.compile-r7rs"
    }
  })
  stages.plus(stage('tap') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=racket LIBRARY=tap all install test"
    }
  })
  stages.plus(stage('mouth') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=racket LIBRARY=mouth all install test"
    }
  })
  stages.plus(stage('debug') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=racket LIBRARY=debug all install test"
    }
  })
  stages.plus(stage('hardware-info') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=racket LIBRARY=hardware-info all install test"
    }
  })
  stages.plus(stage('lambda-utils') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=racket LIBRARY=lambda-utils all install test"
    }
  })
  return stages
}

def get_sagittarius_stages() {
  def stages = []
  stages.plus(stage('init') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev"
      sh "git clone https://github.com/ashinn/chibi-scheme.git --depth=1 || true"
      sh "make -j8 -C chibi-scheme"
      sh "make -j8 -C chibi-scheme install"
      sh "snow-chibi install --always-yes retropikzel.compile-r7rs"
    }
  })
  stages.plus(stage('tap') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=sagittarius LIBRARY=tap all install test"
    }
  })
  stages.plus(stage('mouth') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=sagittarius LIBRARY=mouth all install test"
    }
  })
  stages.plus(stage('debug') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=sagittarius LIBRARY=debug all install test"
    }
  })
  stages.plus(stage('hardware-info') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=sagittarius LIBRARY=hardware-info all install test"
    }
  })
  stages.plus(stage('lambda-utils') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=sagittarius LIBRARY=lambda-utils all install test"
    }
  })
  return stages
}

def get_skint_stages() {
  def stages = []
  stages.plus(stage('init') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev"
      sh "git clone https://github.com/ashinn/chibi-scheme.git --depth=1 || true"
      sh "make -j8 -C chibi-scheme"
      sh "make -j8 -C chibi-scheme install"
      sh "snow-chibi install --always-yes retropikzel.compile-r7rs"
    }
  })
  stages.plus(stage('tap') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=skint LIBRARY=tap all install test"
    }
  })
  stages.plus(stage('mouth') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=skint LIBRARY=mouth all install test"
    }
  })
  stages.plus(stage('debug') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=skint LIBRARY=debug all install test"
    }
  })
  stages.plus(stage('hardware-info') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=skint LIBRARY=hardware-info all install test"
    }
  })
  stages.plus(stage('lambda-utils') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=skint LIBRARY=lambda-utils all install test"
    }
  })
  return stages
}

def get_stklos_stages() {
  def stages = []
  stages.plus(stage('init') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev"
      sh "git clone https://github.com/ashinn/chibi-scheme.git --depth=1 || true"
      sh "make -j8 -C chibi-scheme"
      sh "make -j8 -C chibi-scheme install"
      sh "snow-chibi install --always-yes retropikzel.compile-r7rs"
    }
  })
  stages.plus(stage('tap') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=stklos LIBRARY=tap all install test"
    }
  })
  stages.plus(stage('mouth') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=stklos LIBRARY=mouth all install test"
    }
  })
  stages.plus(stage('debug') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=stklos LIBRARY=debug all install test"
    }
  })
  stages.plus(stage('hardware-info') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=stklos LIBRARY=hardware-info all install test"
    }
  })
  stages.plus(stage('lambda-utils') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=stklos LIBRARY=lambda-utils all install test"
    }
  })
  return stages
}

def get_tr7_stages() {
  def stages = []
  stages.plus(stage('init') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev"
      sh "git clone https://github.com/ashinn/chibi-scheme.git --depth=1 || true"
      sh "make -j8 -C chibi-scheme"
      sh "make -j8 -C chibi-scheme install"
      sh "snow-chibi install --always-yes retropikzel.compile-r7rs"
    }
  })
  stages.plus(stage('tap') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=tr7 LIBRARY=tap all install test"
    }
  })
  stages.plus(stage('mouth') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=tr7 LIBRARY=mouth all install test"
    }
  })
  stages.plus(stage('debug') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=tr7 LIBRARY=debug all install test"
    }
  })
  stages.plus(stage('hardware-info') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=tr7 LIBRARY=hardware-info all install test"
    }
  })
  stages.plus(stage('lambda-utils') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=tr7 LIBRARY=lambda-utils all install test"
    }
  })
  return stages
}

def get_ypsilon_stages() {
  def stages = []
  stages.plus(stage('init') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "apt-get update && apt-get install -y git ca-certificates gcc make libffi-dev"
      sh "git clone https://github.com/ashinn/chibi-scheme.git --depth=1 || true"
      sh "make -j8 -C chibi-scheme"
      sh "make -j8 -C chibi-scheme install"
      sh "snow-chibi install --always-yes retropikzel.compile-r7rs"
    }
  })
  stages.plus(stage('tap') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=ypsilon LIBRARY=tap all install test"
    }
  })
  stages.plus(stage('mouth') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=ypsilon LIBRARY=mouth all install test"
    }
  })
  stages.plus(stage('debug') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=ypsilon LIBRARY=debug all install test"
    }
  })
  stages.plus(stage('hardware-info') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=ypsilon LIBRARY=hardware-info all install test"
    }
  })
  stages.plus(stage('lambda-utils') {
    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
      sh "make SCHEME=ypsilon LIBRARY=lambda-utils all install test"
    }
  })
  return stages
}

