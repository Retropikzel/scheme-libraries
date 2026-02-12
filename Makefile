SCHEME=chibi
RNRS=r7rs
LIBRARY=ctrf
AUTHOR=retropikzel

LIBRARY_FILE=retropikzel/${LIBRARY}.sld
VERSION=$(shell cat retropikzel/${LIBRARY}/VERSION)
DESCRIPTION=$(shell head -n1 retropikzel/${LIBRARY}/README.md)
README=retropikzel/${LIBRARY}/README.html
TESTFILE=retropikzel/${LIBRARY}/test.scm

PKG=${AUTHOR}-${LIBRARY}-${VERSION}.tgz
MOUTHPKG=${AUTHOR}-mouth-$(shell cat retropikzel/mouth/VERSION).tgz

DOCKERIMG=${SCHEME}:head
ifeq "${SCHEME}" "chicken"
DOCKERIMG="chicken:5"
endif
DOCKER_TAG=scheme-library-test-${SCHEME}

all: build

build: retropikzel/${LIBRARY}/LICENSE retropikzel/${LIBRARY}/VERSION retropikzel/${LIBRARY}/README.md
	echo "<pre>$$(cat retropikzel/${LIBRARY}/README.md)</pre>" > ${README}
	snow-chibi package --version=${VERSION} --authors=${AUTHOR} --doc=${README} --description="${DESCRIPTION}" ${LIBRARY_FILE}

install:
	snow-chibi install --impls=${SCHEME} ${SNOW_CHIBI_ARGS} ${PKG}

uninstall:
	-snow-chibi remove --impls=${SCHEME} ${PKG}

init-venv: build
	@rm -rf venv
	@scheme-venv ${SCHEME} ${RNRS} venv
	@echo "(import (scheme base) (scheme write) (scheme read) (scheme char) (scheme file) (scheme process-context) (retropikzel mouth) (srfi 64) (retropikzel ctrf) (retropikzel ${LIBRARY}))" > venv/test.scm
	@echo "(test-runner-current (ctrf-runner))" >> venv/test.scm
	@printf "#!r6rs\n(import (rnrs) (srfi :64) (srfi :98) (retropikzel mouth) (retropikzel ${LIBRARY}))" > venv/test.sps
	@cat ${TESTFILE} >> venv/test.scm
	@cat ${TESTFILE} >> venv/test.sps
	@if [ "${RNRS}" = "r7rs" ]; then ./venv/bin/snow-chibi install retropikzel.mouth; fi
	@if [ "${RNRS}" = "r7rs" ]; then ./venv/bin/snow-chibi install --always-yes retropikzel.ctrf; fi
	@if [ "${RNRS}" = "r6rs" ]; then if [ -d ../foreign-c ]; then cp -r ../foreign-c/foreign venv/lib/; fi; fi
	@if [ "${RNRS}" = "r6rs" ]; then if [ -d ../foreign-c-srfis ]; then cp -r ../foreign-c-srfis/srfi venv/lib/; fi; fi
	@if [ "${RNRS}" = "r6rs" ]; then cp -r retropikzel venv/lib/; fi
	@if [ "${SCHEME}" = "chezscheme" ]; then ./venv/bin/akku install akku-r7rs chez-srfi; fi
	@if [ "${SCHEME}" = "ikarus" ]; then ./venv/bin/akku install akku-r7rs chez-srfi; fi
	@if [ "${SCHEME}" = "ironscheme" ]; then ./venv/bin/akku install akku-r7rs chez-srfi; fi
	@if [ "${SCHEME}" = "racket" ]; then ./venv/bin/akku install akku-r7rs chez-srfi; fi
	@if [ "${RNRS}" = "r6rs" ]; then ./venv/bin/akku install; fi
	@if [ "${SCHEME}" = "chicken" ]; then ./venv/bin/snow-chibi install --always-yes srfi.64; fi
	@if [ "${SCHEME}-${RNRS}" = "mosh-r7rs" ]; then ./venv/bin/snow-chibi install --always-yes srfi.64; fi
	@if [ "${RNRS}" = "r7rs" ]; then ./venv/bin/snow-chibi install ${PKG}; fi

run-test: init-venv
	if [ "${RNRS}" = "r6rs" ]; then ./venv/bin/scheme-compile venv/test.sps; fi
	if [ "${RNRS}" = "r7rs" ]; then VENV_CSC_ARGS="-L -lcurl" ./venv/bin/scheme-compile venv/test.scm; fi
	./venv/test

clean:
	git clean -X -f
