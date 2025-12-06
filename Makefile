TMPDIR=.tmp/${SCHEME}

.SILENT: build install test-r7rs test-r7rs-docker clean ${TMPDIR}
.PHONY: ${TMPDIR}

SCHEME=chibi
LIBRARY=cgi
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
	cp ${TESTFILE} ${TMPDIR}/
	mkdir -p ${TMPDIR}/retropikzel
	cp -r retropikzel/${LIBRARY} ${TMPDIR}/retropikzel/
	cp -r retropikzel/${LIBRARY}.s* ${TMPDIR}/retropikzel/

test-r7rs: ${TMPDIR}
	echo "Hello"
	cd ${TMPDIR} && COMPILE_R7RS=${SCHEME} compile-scheme -I . -o test-r7rs test.scm
	cd ${TMPDIR} && ./test-r7rs

test-r7rs-docker: ${TMPDIR}
	echo "Building docker image..."
	docker build --build-arg IMAGE=${DOCKERIMG} --build-arg SCHEME=${SCHEME} --tag=scheme-library-test-${SCHEME} -f Dockerfile.test --quiet . 2> ${TMPDIR}/docker.log || cat ${TMPDIR}/docker.log
	docker run -v "${PWD}:/workdir" -w /workdir -t scheme-library-test-${SCHEME} \
		sh -c "make SCHEME=${SCHEME} SNOW_CHIBI_ARGS=--always-yes LIBRARY=${LIBRARY} build install test-r7rs; chmod -R 755 ${TMPDIR}"

clean:
	git clean -X -f
