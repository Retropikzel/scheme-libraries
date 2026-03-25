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

SFX=scm
SNOW=snow-chibi --impls=${SCHEME} install --always-yes
LIB_PATHS=
ifeq "${RNRS}" "r6rs"
SNOW=snow-chibi --impls=${SCHEME} install --always-yes --install-source-dir=. --install-library-dir=.
SFX=sps
LIB_PATHS=-I .akku/lib
endif

all: build

build: retropikzel/${LIBRARY}/LICENSE retropikzel/${LIBRARY}/VERSION retropikzel/${LIBRARY}/README.md
	echo "<pre>$$(cat retropikzel/${LIBRARY}/README.md)</pre>" > ${README}
	snow-chibi package --always-yes --version=${VERSION} --authors=${AUTHOR} --doc=${README} --description="${DESCRIPTION}" ${LIBRARY_FILE}

install:
	snow-chibi install --impls=${SCHEME} --always-yes ${PKG}

test: logs build
	rm -rf .tmp
	mkdir -p .tmp
	cp ${PKG} .tmp/
	cat test-headers.${SFX} ${TESTFILE} | sed 's/LIBRARY/${LIBRARY}/' > .tmp/test.${SFX}
	cd .tmp && SNOW_PACKAGES="srfi.64 retropikzel.mouth" \
		APT_PACKAGES="libcurl4-openssl-dev" \
		COMPILE_R7RS=${SCHEME} \
		CSC_OPIONS="-L -lcurl" \
		test-r7rs test.${SFX} ${PKG}

retropikzel/wasm/plus.wasm: retropikzel/wasm/plus.c
	emcc -o retropikzel/wasm/plus.js retropikzel/wasm/plus.c

clean:
	git clean -X -f
