TMPDIR=.tmp/${SCHEME}

.SILENT: build install test-r6rs test-r6rs-docker test-r7rs test-r7rs-docker \
	clean ${TMPDIR}
.PHONY: ${TMPDIR}

SCHEME=chibi
LIBRARY=ctrf
AUTHOR=retropikzel

LIBRARY_FILE=retropikzel/${LIBRARY}.sld
VERSION=$(shell cat retropikzel/${LIBRARY}/VERSION)
DESCRIPTION=$(shell head -n1 retropikzel/${LIBRARY}/README.md)
README=retropikzel/${LIBRARY}/README.html
TESTFILE=retropikzel/${LIBRARY}/test.scm

PKG=${AUTHOR}-${LIBRARY}-${VERSION}.tgz

DOCKERIMG=${SCHEME}:head
ifeq "${SCHEME}" "chicken"
DOCKERIMG="chicken:5"
endif
DOCKER_TAG=scheme-library-test-${SCHEME}

DOCKER_QUIET="--quiet"

all: build

build: retropikzel/${LIBRARY}/LICENSE retropikzel/${LIBRARY}/VERSION retropikzel/${LIBRARY}/README.md
	echo "<pre>$$(cat retropikzel/${LIBRARY}/README.md)</pre>" > ${README}
	snow-chibi package --version=${VERSION} --authors=${AUTHOR} --doc=${README} --description="${DESCRIPTION}" ${LIBRARY_FILE}

install:
	snow-chibi install --impls=${SCHEME} ${SNOW_CHIBI_ARGS} ${PKG}

uninstall:
	-snow-chibi remove --impls=${SCHEME} ${PKG}

${TMPDIR}:
	mkdir -p ${TMPDIR}
	mkdir -p ${TMPDIR}/retropikzel
	cp -r retropikzel/${LIBRARY} ${TMPDIR}/retropikzel/
	cp -r retropikzel/${LIBRARY}.s* ${TMPDIR}/retropikzel/
	if [ -d srfi ]; then cp -r srfi ${TMPDIR}/; fi

test-r6rs: ${TMPDIR}
	cd ${TMPDIR} && printf "#!r6rs\n(import (rnrs base) (rnrs control) (rnrs io simple) (rnrs files) (rnrs programs) (srfi :64) (srfi :180) (retropikzel ${LIBRARY}))\n" > test-r6rs.sps
	cat ${TESTFILE} >> ${TMPDIR}/test-r6rs.sps
	cd ${TMPDIR} && akku install chez-srfi akku-r7rs
	cd ${TMPDIR} && COMPILE_R7RS=${SCHEME} timeout 120 compile-scheme -I .akku/lib -o test-r6rs test-r6rs.sps
	cd ${TMPDIR} && timeout 120 ./test-r6rs

test-r6rs-docker: ${TMPDIR}
	echo "Building docker image..."
	docker build --build-arg IMAGE=${DOCKERIMG} --build-arg SCHEME=${SCHEME} --tag=${DOCKER_TAG} -f Dockerfile.test ${DOCKER_QUIET} .
	docker run -t ${DOCKER_TAG} sh -c "make SCHEME=${SCHEME} SNOW_CHIBI_ARGS=--always-yes LIBRARY=${LIBRARY} test-r6rs"

test-r7rs: ${TMPDIR}
	cd ${TMPDIR} && echo "(import (scheme base) (scheme write) (scheme read) (scheme char) (scheme file) (scheme process-context) (srfi 64) (srfi 180) (retropikzel ${LIBRARY}))" > test-r7rs.scm
	cat ${TESTFILE} >> ${TMPDIR}/test-r7rs.scm
	cd ${TMPDIR} && COMPILE_R7RS=${SCHEME} timeout 120 compile-scheme -I . -o test-r7rs test-r7rs.scm
	cd ${TMPDIR} && timeout 120 ./test-r7rs

test-r7rs-docker: ${TMPDIR}
	echo "Building docker image..."
	docker build --build-arg IMAGE=${DOCKERIMG} --build-arg SCHEME=${SCHEME} --tag=${DOCKER_TAG} -f Dockerfile.test ${DOCKER_QUIET} .
	docker run -t ${DOCKER_TAG} sh -c "make SCHEME=${SCHEME} SNOW_CHIBI_ARGS=--always-yes LIBRARY=${LIBRARY} build install test-r7rs"

clean:
	git clean -X -f
