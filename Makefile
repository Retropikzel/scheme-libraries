TMPDIR=.tmp/${SCHEME}

.SILENT: build install test-r6rs test-r6rs-docker test-r7rs test-r7rs-docker \
	clean ${TMPDIR}
.PHONY: ${TMPDIR}

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
	@echo "(import (scheme base) (scheme write) (scheme read) (scheme char) (scheme file) (scheme process-context) (srfi 64) (retropikzel ${LIBRARY}))" > venv/test.scm
	@printf "#!r6rs\n(import (rnrs) (srfi :64) (srfi :98) (retropikzel ${LIBRARY}))" > venv/test.sps
	@cat ${TESTFILE} >> venv/test.scm
	@cat ${TESTFILE} >> venv/test.sps
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

${TMPDIR}:
	mkdir -p ${TMPDIR}
	mkdir -p ${TMPDIR}/retropikzel
	cp -r retropikzel/${LIBRARY} ${TMPDIR}/retropikzel/
	cp -r retropikzel/${LIBRARY}.s* ${TMPDIR}/retropikzel/
	if [ -d srfi ]; then cp -r srfi ${TMPDIR}/; fi

test-r6rs: ${TMPDIR}
	cd ${TMPDIR} && printf "#!r6rs\n(import (rnrs base) (rnrs control) (rnrs io simple) (rnrs files) (rnrs programs) (srfi :64) (retropikzel ${LIBRARY}))\n" > test-r6rs.sps
	cat ${TESTFILE} >> ${TMPDIR}/test-r6rs.sps
	cd ${TMPDIR} && snow-chibi install --impls=${SCHEME} --install-source-dir=. --install-library-dir=. --always-yes srfi.180
	cd ${TMPDIR} && akku install chez-srfi akku-r7rs srfi.180
	cd ${TMPDIR} && COMPILE_R7RS=${SCHEME} timeout 120 compile-scheme -I .akku/lib -o test-r6rs test-r6rs.sps
	cd ${TMPDIR} && timeout 120 ./test-r6rs

test-r6rs-docker: ${TMPDIR}
	echo "Building docker image..."
	docker build --build-arg IMAGE=${DOCKERIMG} --build-arg SCHEME=${SCHEME} --tag=${DOCKER_TAG} -f Dockerfile.test .
	docker run -p 3001:3001 -t ${DOCKER_TAG} sh -c "lighttpd -f fcgi-lighttpd.conf && make SCHEME=${SCHEME} SNOW_CHIBI_ARGS=--always-yes LIBRARY=${LIBRARY} test-r6rs"

test-r7rs: ${TMPDIR}
	cd ${TMPDIR} && echo "(import (scheme base) (scheme write) (scheme read) (scheme char) (scheme file) (scheme process-context) (srfi 64) (retropikzel ${LIBRARY}))" > test-r7rs.scm
	cat ${TESTFILE} >> ${TMPDIR}/test-r7rs.scm
	cd ${TMPDIR} && COMPILE_R7RS=${SCHEME} timeout 120 compile-scheme -I . -o test-r7rs test-r7rs.scm
	cd ${TMPDIR} && timeout 120 ./test-r7rs

test-r7rs-docker: ${TMPDIR}
	echo "Building docker image..."
	docker build --build-arg IMAGE=${DOCKERIMG} --build-arg SCHEME=${SCHEME} --tag=${DOCKER_TAG} -f Dockerfile.test .
	docker run -p 3001:3001 -t ${DOCKER_TAG} sh -c "lighttpd -f fcgi-lighttpd.conf && make SCHEME=${SCHEME} SNOW_CHIBI_ARGS=--always-yes LIBRARY=${LIBRARY} build install test-r7rs"

clean:
	git clean -X -f
