SCHEME=chibi
RNRS=r7rs
LIBRARY=ctrf
VENV=venv-${SCHEME}-${RNRS}-${LIBRARY}
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

logs:
	mkdir -p logs

snow:
	snow-chibi install --impls=generic --skip-tests?=1 --always-yes --install-source-dir=snow --install-library-dir=snow retropikzel.ctrf || true
	snow-chibi install --impls=generic --skip-tests?=1 --always-yes --install-source-dir=snow --install-library-dir=snow srfi.64 || true

${VENV}:
	scheme-venv ${SCHEME} ${RNRS} ${VENV}
	if [ "${RNRS}" = "r7rs" ]; then ${VENV}/bin/snow-chibi install --always-yes srfi.64; fi
	if [ "${RNRS}" = "r7rs" ]; then ${VENV}/bin/snow-chibi install --always-yes retropikzel.mouth; fi
	if [ "${RNRS}" = "r7rs" ]; then ${VENV}/bin/snow-chibi install --always-yes retropikzel.ctrf; fi
	if [ "${SCHEME}-${RNRS}" = "mosh-r7rs" ]; then ${VENV}/bin/snow-chibi install --always-yes srfi.64; fi
	if [ "${SCHEME}" = "chezscheme" ]; then ${VENV}/bin/akku install akku-r7rs chez-srfi; fi
	if [ "${SCHEME}" = "ikarus" ]; then ${VENV}/bin/akku install akku-r7rs chez-srfi; fi
	if [ "${SCHEME}" = "ironscheme" ]; then ${VENV}/bin/akku install akku-r7rs chez-srfi; fi
	if [ "${SCHEME}" = "racket" ]; then ${VENV}/bin/akku install akku-r7rs chez-srfi; fi

run-test-venv: ${VENV} logs build
	echo "(import (scheme base) (scheme write) (scheme read) (scheme char) (scheme file) (scheme process-context) (retropikzel mouth) (srfi 64) (retropikzel ctrf) (retropikzel ${LIBRARY}))" > ${VENV}/test.scm
	echo "(test-runner-current (ctrf-runner))" >> ${VENV}/test.scm
	printf "#!r6rs\n(import (rnrs) (srfi :64) (srfi :98) (retropikzel mouth) (retropikzel ${LIBRARY}))" > ${VENV}/test.sps
	cat ${TESTFILE} >> ${VENV}/test.scm
	cat ${TESTFILE} >> ${VENV}/test.sps
	if [ "${RNRS}" = "r6rs" ]; then cp -r retropikzel ${VENV}/lib/; fi
	if [ "${RNRS}" = "r6rs" ]; then ${VENV}/bin/akku install; fi
	if [ "${RNRS}" = "r7rs" ]; then ${VENV}/bin/snow-chibi install ${PKG}; fi
	if [ "${RNRS}" = "r6rs" ]; then ${VENV}/bin/scheme-compile ${VENV}/test.sps; fi
	if [ "${RNRS}" = "r7rs" ]; then CSC_OPTIONS="-L -lcurl" ${VENV}/bin/scheme-compile ${VENV}/test.scm; fi
	cd ${VENV} && ./test
	mv ${VENV}/*.json logs/ || true

run-test-system: logs snow build
	printf "#!r6rs\n(import (rnrs) (srfi :64) (srfi :98) (retropikzel mouth) (retropikzel ctrf) (retropikzel ${LIBRARY}))" > run-test.sps
	echo "(test-runner-current (ctrf-runner))" >> run-test.sps
	cat ${TESTFILE} >> run-test.sps
	echo "(import (scheme base) (scheme write) (scheme read) (scheme char) (scheme file) (scheme process-context) (retropikzel mouth) (srfi 64) (retropikzel ctrf) (retropikzel ${LIBRARY}))" > run-test.scm
	echo "(test-runner-current (ctrf-runner))" >> run-test.scm
	cat ${TESTFILE} >> run-test.scm
	if [ "${RNRS}" = "r6rs" ]; then akku install akku-r7rs; fi
	if [ "${RNRS}" = "r7rs" ]; then snow-chibi install --impls=${SCHEME} --always-yes srfi.64; fi
	if [ "${RNRS}" = "r7rs" ]; then snow-chibi install --impls=${SCHEME} --always-yes retropikzel.mouth; fi
	if [ "${RNRS}" = "r7rs" ]; then snow-chibi install --impls=${SCHEME} --always-yes retropikzel.ctrf; fi
	if [ "${RNRS}" = "r7rs" ]; then snow-chibi install --impls=${SCHEME} ${PKG}; fi
	rm -rf run-test
	if [ "${RNRS}" = "r6rs" ]; then COMPILE_R7RS=${SCHEME} compile-scheme -I .akku/lib run-test.sps; fi
	if [ "${RNRS}" = "r7rs" ]; then COMPILE_R7RS=${SCHEME} CSC_OPTIONS="-L -lcurl" compile-scheme run-test.scm; fi
	./run-test
	mv *.json logs/ || true

run-test-docker:
	docker build --build-arg IMAGE=${DOCKERIMG} -f Dockerfile.test --tag=scheme-libraries-${SCHEME} .
	docker run -v "${PWD}/logs:/workdir/logs" -w /workdir scheme-libraries-${SCHEME} sh -c "make SCHEME=${SCHEME} RNRS=${RNRS} LIBRARY=${LIBRARY} run-test-system"

retropikzel/wasm/plus.wasm: retropikzel/wasm/plus.c
	emcc -o retropikzel/wasm/plus.js retropikzel/wasm/plus.c

clean:
	git clean -X -f
