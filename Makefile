SCHEME=chibi
RNRS=r7rs
LIBRARY=ctrf
VENV=venv-${SCHEME}-${RNRS}-${LIBRARY}
AUTHOR=retropikzel
PKG=${AUTHOR}-${LIBRARY}-${VERSION}.tgz

LIBRARY_FILE=retropikzel/${LIBRARY}.sld
TESTFILE=retropikzel/${LIBRARY}/test.scm
VERSION != cat retropikzel/${LIBRARY}/VERSION
DESCRIPTION != head -n1 retropikzel/${LIBRARY}/README.md
README=retropikzel/${LIBRARY}/README.html

SFX=scm
ifeq "${RNRS}" "r6rs"
SFX=sps
endif

DOCKER_TAG=head


all: package

package: retropikzel/${LIBRARY}/LICENSE retropikzel/${LIBRARY}/VERSION retropikzel/${LIBRARY}/README.md
	echo "<pre>$$(cat retropikzel/${LIBRARY}/README.md)</pre>" > ${README}
	snow-chibi package \
		--always-yes \
		--version=${VERSION} \
		--authors=${AUTHOR} \
		--doc=${README} \
		--description="${DESCRIPTION}" \
		${LIBRARY_FILE}

install:
	snow-chibi install --impls=${SCHEME} --always-yes ${PKG}

testfiles: package ${TESTFILE}
	rm -rf .tmp
	mkdir -p .tmp
	cp ${PKG} .tmp/
	cp -r retropikzel .tmp/
	cat test-headers.${SFX} ${TESTFILE} | sed 's/LIBRARY/${LIBRARY}/' > .tmp/test.${SFX}
	cat ${TESTFILE} >> .tmp/test.${SFX}

test: testfiles
	cd .tmp && COMPILE_R7RS=${SCHEME} CSC_OPIONS="-L -lcurl" compile-r7rs -o test-program -I . test.${SFX}
	cd .tmp && ./test-program

test-docker: package testfiles
	cd .tmp && \
		SNOW_PACKAGES="srfi.64 srfi.145 srfi.180 retropikzel.mouth r6rs.bytevectors ${PKG}" \
		APT_PACKAGES="libcurl4-openssl-dev" \
		AKKU_PACKAGES="akku-r7rs" \
		DOCKER_TAG=${DOCKER_TAG} \
		COMPILE_R7RS=${SCHEME} \
		TEST_R7RS_DEBUG=1 \
		CSC_OPIONS="-L -lcurl" \
		test-r7rs test.${SFX}

retropikzel/wasm/plus.wat: retropikzel/wasm/plus.c
	emcc -o retropikzel/wasm/plus.js retropikzel/wasm/plus.c
	wasm-dis retropikzel/wasm/plus.wasm > retropikzel/wasm/plus.wat

clean:
	git clean -X -f
