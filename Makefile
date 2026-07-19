.SILENT:
SCHEME=chibi
RNRS=r7rs
LIBRARY=ctrf
DOCKER_TAG=latest

AUTHOR=retropikzel
PKG=${AUTHOR}-${LIBRARY}-${VERSION}.tgz
tmpdir=.tmp/${SCHEME}/${LIBRARY}

LIBRARY_FILE=retropikzel/${LIBRARY}.sld
TESTFILE=retropikzel/${LIBRARY}/test.scm
VERSION != cat retropikzel/${LIBRARY}/VERSION
DESCRIPTION != head -n1 retropikzel/${LIBRARY}/README.md
README=retropikzel/${LIBRARY}/README.html

AKKU_PACKAGES=""
SFX=scm
ifeq "${RNRS}" "r6rs"
SFX=sps
AKKU_PACKAGES="akku-r7rs"
endif

ifeq "${SCHEME}" "capyscheme"
DOCKER_TAG=head
endif
ifeq "${SCHEME}" "chibi"
DOCKER_TAG=head
endif
ifeq "${SCHEME}" "chicken"
DOCKER_TAG=head
endif
ifeq "${SCHEME}" "gauche"
DOCKER_TAG=head
endif



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

${PKG}: package

install:
	snow-chibi install --impls=${SCHEME} ${PKG}

testfiles: ${PKG} ${TESTFILE}
	rm -rf ${tmpdir}
	mkdir -p ${tmpdir}
	cp ${PKG} ${tmpdir}
	cat test-headers.${SFX} | sed 's/LIBRARY/${LIBRARY}/' > ${tmpdir}/test.${SFX}
	cat ${TESTFILE} >> ${tmpdir}/test.${SFX}

test: testfiles
	cd ${tmpdir} && COMPILE_R7RS=${SCHEME} CSC_OPIONS="-L -lcurl" compile-r7rs -o test-program test.${SFX}
	cd ${tmpdir} && ./test-program

test-docker: testfiles
	SNOW_PACKAGES="srfi.19 srfi.64 srfi.180 ${PKG}" \
	APT_PACKAGES="libcurl4-openssl-dev" \
	AKKU_PACKAGES="${AKKU_PACKAGES}" \
	DOCKER_TAG=${DOCKER_TAG} \
	COMPILE_R7RS=${SCHEME} \
	CSC_OPIONS="-L -lcurl" \
		test-r7rs -o ${tmpdir}/test-program ${tmpdir}/test.${SFX}

retropikzel/wasm/plus.wat: retropikzel/wasm/plus.c
	emcc -o retropikzel/wasm/plus.js retropikzel/wasm/plus.c
	wasm-dis retropikzel/wasm/plus.wasm > retropikzel/wasm/plus.wat

clean:
	git clean -X -f
