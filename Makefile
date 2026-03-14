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

SFX=scm
SNOW=snow-chibi --impls=${SCHEME} install --skip-tests?=1 --always-yes
LIB_PATHS=
ifeq "${RNRS}" "r6rs"
SNOW=snow-chibi --impls=${SCHEME} install --skip-tests?=1 --always-yes --install-source-dir=. --install-library-dir=.
SFX=sps
LIB_PATHS=-I .akku/lib
endif

all: build

build: retropikzel/${LIBRARY}/LICENSE retropikzel/${LIBRARY}/VERSION retropikzel/${LIBRARY}/README.md
	echo "<pre>$$(cat retropikzel/${LIBRARY}/README.md)</pre>" > ${README}
	snow-chibi package --always-yes --version=${VERSION} --authors=${AUTHOR} --doc=${README} --description="${DESCRIPTION}" ${LIBRARY_FILE}

index:
	snow-chibi index ${PKG}

install: index
	snow-chibi install --impls=${SCHEME} --always-yes retropikzel.${LIBRARY}

uninstall:
	snow-chibi remove --impls=${SCHEME} retropikzel.${LIBRARY}

logs:
	mkdir -p logs

test: logs build index
	rm -rf .tmp
	mkdir -p .tmp
	cat test-headers.${SFX} ${TESTFILE} | sed 's/LIBRARY/${LIBRARY}/' > .tmp/test.${SFX}
	cd .tmp && ${SNOW} ${TEST_DEPENDS} retropikzel.${LIBRARY}
	cd .tmp && akku install akku-r7rs
	cd .tmp && COMPILE_R7RS=${SCHEME} CSC_OPTIONS="-L -lcurl" compile-r7rs ${LIB_PATHS} -o test test.${SFX};
	cd .tmp && ./test
	mv .tmp/*.json logs/ || true

test-docker: logs
	docker build --build-arg IMAGE=${IMAGE} --build-arg SCHEME=${SCHEME} -f Dockerfile.test --tag=${SCHEME}-testing .
	docker run -v "${PWD}/logs:/workdir/logs" -w /workdir ${SCHEME}-testing sh -c "make SCHEME=${SCHEME} RNRS=${RNRS} LIBRARY=${LIBRARY} test"

retropikzel/wasm/plus.wasm: retropikzel/wasm/plus.c
	emcc -o retropikzel/wasm/plus.js retropikzel/wasm/plus.c

clean:
	git clean -X -f
