SCHEME=chibi
RNRS=r7rs
LIBRARY=ctrf
VENV=venv-${SCHEME}-${RNRS}-${LIBRARY}
AUTHOR=retropikzel
PKG=${AUTHOR}-${LIBRARY}-${VERSION}.tgz

LIBRARY_FILE=retropikzel/${LIBRARY}.sld
VERSION=$(shell cat retropikzel/${LIBRARY}/VERSION)
DESCRIPTION=$(shell head -n1 retropikzel/${LIBRARY}/README.md)
README=retropikzel/${LIBRARY}/README.html
TESTFILE=retropikzel/${LIBRARY}/test.scm
TEST_DEPENDS=srfi.64 retropikzel.mouth retropikzel.ctrf

all: build

build: retropikzel/${LIBRARY}/LICENSE retropikzel/${LIBRARY}/VERSION retropikzel/${LIBRARY}/README.md
	echo "<pre>$$(cat retropikzel/${LIBRARY}/README.md)</pre>" > ${README}
	snow-chibi package --version=${VERSION} --authors=${AUTHOR} --doc=${README} --description="${DESCRIPTION}" ${LIBRARY_FILE}

index:
	snow-chibi index ${PKG}

install: index
	snow-chibi install --impls=${SCHEME} --always-yes retropikzel.${LIBRARY}

uninstall:
	snow-chibi remove --impls=${SCHEME} retropikzel.${LIBRARY}

test: logs build index
	mkdir -p logs
	# tmpdir
	mkdir -p .tmp
	# r6rs testfiles
	printf "#!r6rs\n(import (rnrs) (srfi :64) (srfi :98) (retropikzel mouth) (retropikzel ctrf) (retropikzel ${LIBRARY}))" > .tmp/test.sps
	echo "(test-runner-current (ctrf-runner))" >> .tmp/test.sps
	cat ${TESTFILE} >> .tmp/test.sps
	# r7rs testfiles
	echo "(import (scheme base) (scheme write) (scheme read) (scheme char) (scheme file) (scheme process-context) (retropikzel mouth) (srfi 64) (retropikzel ctrf) (retropikzel ${LIBRARY}))" > .tmp/test.scm
	echo "(test-runner-current (ctrf-runner))" >> .tmp/test.scm
	cat ${TESTFILE} >> .tmp/test.scm
	# r6rs
	if [ "${RNRS}" = "r6rs" ]; then \
		cd .tmp \
		&& snow-chibi install --impls=${SCHEME} --skip-tests?=1 --always-yes --install-source-dir=. --install-library-dir=. ${TEST_DEPENDS} retropikzel.${LIBRARY} \
		&& akku install akku-r7rs 2> /dev/null \
		&& COMPILE_R7RS=${SCHEME} compile-r7rs -I .akku/lib -o test test.sps; \
	fi
	# r7rs
	if [ "${RNRS}" = "r7rs" ]; then \
		cd .tmp \
		&& snow-chibi install --impls=${SCHEME} retropikzel.${LIBRARY} \
		&& COMPILE_R7RS=${SCHEME} CSC_OPTIONS="-L -lcurl" compile-r7rs -o test test.scm; \
	fi
	cd .tmp && ./test
	mv .tmp/*.json logs/ || true

test-docker:
	docker build --build-arg IMAGE=${IMAGE} --build-arg SCHEME=${SCHEME} -f Dockerfile.test --tag=${SCHEME}-testing .
	docker run -v "${PWD}/logs:/workdir/logs" -w /workdir ${SCHEME}-testing sh -c "make SCHEME=${SCHEME} RNRS=${RNRS} LIBRARY=${LIBRARY} test"

retropikzel/wasm/plus.wasm: retropikzel/wasm/plus.c
	emcc -o retropikzel/wasm/plus.js retropikzel/wasm/plus.c

clean:
	git clean -X -f
