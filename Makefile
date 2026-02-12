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

all: build

build: retropikzel/${LIBRARY}/LICENSE retropikzel/${LIBRARY}/VERSION retropikzel/${LIBRARY}/README.md
	echo "<pre>$$(cat retropikzel/${LIBRARY}/README.md)</pre>" > ${README}
	snow-chibi package --version=${VERSION} --authors=${AUTHOR} --doc=${README} --description="${DESCRIPTION}" ${LIBRARY_FILE}

install:
	snow-chibi install --impls=${SCHEME} ${SNOW_CHIBI_ARGS} ${PKG}

uninstall:
	-snow-chibi remove --impls=${SCHEME} ${PKG}

run-test-venv: build
	rm -rf venv
	scheme-venv ${SCHEME} ${RNRS} venv
	echo "(import (scheme base) (scheme write) (scheme read) (scheme char) (scheme file) (scheme process-context) (retropikzel mouth) (srfi 64) (retropikzel ctrf) (retropikzel ${LIBRARY}))" > venv/test.scm
	echo "(test-runner-current (ctrf-runner))" >> venv/test.scm
	printf "#!r6rs\n(import (rnrs) (srfi :64) (srfi :98) (retropikzel mouth) (retropikzel ${LIBRARY}))" > venv/test.sps
	cat ${TESTFILE} >> venv/test.scm
	cat ${TESTFILE} >> venv/test.sps
	if [ "${RNRS}" = "r7rs" ]; then ./venv/bin/snow-chibi install --always-yes srfi.64; fi
	if [ "${RNRS}" = "r7rs" ]; then ./venv/bin/snow-chibi install retropikzel.mouth; fi
	if [ "${RNRS}" = "r7rs" ]; then printf "1\n1\n" | ./venv/bin/snow-chibi install retropikzel.ctrf; fi
	if [ "${RNRS}" = "r6rs" ]; then cp -r retropikzel venv/lib/; fi
	if [ "${SCHEME}" = "chezscheme" ]; then ./venv/bin/akku install akku-r7rs chez-srfi; fi
	if [ "${SCHEME}" = "ikarus" ]; then ./venv/bin/akku install akku-r7rs chez-srfi; fi
	if [ "${SCHEME}" = "ironscheme" ]; then ./venv/bin/akku install akku-r7rs chez-srfi; fi
	if [ "${SCHEME}" = "racket" ]; then ./venv/bin/akku install akku-r7rs chez-srfi; fi
	if [ "${RNRS}" = "r6rs" ]; then ./venv/bin/akku install; fi
	if [ "${SCHEME}-${RNRS}" = "mosh-r7rs" ]; then ./venv/bin/snow-chibi install --always-yes srfi.64; fi
	if [ "${RNRS}" = "r7rs" ]; then ./venv/bin/snow-chibi install ${PKG}; fi
	if [ "${RNRS}" = "r6rs" ]; then ./venv/bin/scheme-compile venv/test.sps; fi
	if [ "${RNRS}" = "r7rs" ]; then CSC_OPTIONS="-L -lcurl" ./venv/bin/scheme-compile venv/test.scm; fi
	./venv/test

run-test-system: build
	printf "#!r6rs\n(import (rnrs) (srfi :64) (srfi :98) (retropikzel mouth) (retropikzel ${LIBRARY}))" > run-test.sps
	echo "(test-runner-current (ctrf-runner))" >> run-test.sps
	cat ${TESTFILE} >> run-test.sps
	echo "(import (scheme base) (scheme write) (scheme read) (scheme char) (scheme file) (scheme process-context) (retropikzel mouth) (srfi 64) (retropikzel ctrf) (retropikzel ${LIBRARY}))" > run-test.scm
	echo "(test-runner-current (ctrf-runner))" >> run-test.scm
	cat ${TESTFILE} >> run-test.scm
	if [ "${RNRS}" = "r7rs" ]; then snow-chibi install --always-yes srfi.64; fi
	if [ "${RNRS}" = "r7rs" ]; then snow-chibi install retropikzel.mouth; fi
	if [ "${RNRS}" = "r7rs" ]; then printf "1\n1\n" | snow-chibi install retropikzel.ctrf; fi
	if [ "${SCHEME}" = "chezscheme" ]; then akku install akku-r7rs chez-srfi; fi
	if [ "${SCHEME}" = "ikarus" ]; then akku install akku-r7rs chez-srfi; fi
	if [ "${SCHEME}" = "ironscheme" ]; then akku install akku-r7rs chez-srfi; fi
	if [ "${SCHEME}" = "racket" ]; then akku install akku-r7rs chez-srfi; fi
	if [ "${RNRS}" = "r6rs" ]; then akku install; fi
	if [ "${SCHEME}-${RNRS}" = "mosh-r7rs" ]; then snow-chibi install --always-yes srfi.64; fi
	if [ "${RNRS}" = "r7rs" ]; then snow-chibi install ${PKG}; fi
	if [ "${RNRS}" = "r6rs" ]; then COMPILE_SCHEME=${SCHEME} compile-scheme run-test.sps; fi
	if [ "${RNRS}" = "r7rs" ]; then COMPILE_SCHEME=${SCHEME} CSC_OPTIONS="-L -lcurl" compile-scheme run-test.scm; fi
	./run-test

run-test-docker:
	docker build --build-arg IMAGE=${DOCKERIMG} -f Dockerfile.test --tag=scheme-libraries-${SCHEME}-${RNRS} .
	docker run -v "${PWD}:/workdir" -w /workdir scheme-libraries-${SCHEME}-${RNRS} sh -c "make SCHEME=${SCHEME} RNRS=${RNRS} LIBRARY=${LIBRARY} run-test-system ; chmod 755 *.json"

clean:
	git clean -X -f
