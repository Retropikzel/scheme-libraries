.SILENT: build install test test-docker clean ${TMPDIR}
SCHEME=chibi
LIBRARY=cgi
AUTHOR=retropikzel

LIBRARY_FILE=retropikzel/${LIBRARY}.sld
VERSION=$(shell cat retropikzel/${LIBRARY}/VERSION)
DESCRIPTION=$(shell head -n1 retropikzel/${LIBRARY}/README.md)
README=retropikzel/${LIBRARY}/README.html
TESTFILE=retropikzel/${LIBRARY}/test.scm

PKG=${AUTHOR}-${LIBRARY}-${VERSION}.tgz
TMPDIR=tmp/${SCHEME}

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
	@mkdir -p ${TMPDIR}
	@cp ${TESTFILE} ${TMPDIR}/
	@mkdir -p ${TMPDIR}/retropikzel
	@cp -r retropikzel/${LIBRARY} ${TMPDIR}/retropikzel/
	@cp -r retropikzel/${LIBRARY}.s* ${TMPDIR}/retropikzel/

test: ${TMPDIR}
	echo "Hello"
	cd ${TMPDIR} && COMPILE_R7RS=${SCHEME} compile-r7rs -I . -o test test.scm
	cd ${TMPDIR} && ./test

test-docker: ${TMPDIR}
	docker build --build-arg IMAGE=${DOCKERIMG} --build-arg SCHEME=${SCHEME} --tag=scheme-library-test-${SCHEME} -f Dockerfile.test . 2> ${TMPDIR}/docker.log || cat ${TMPDIR}/docker.log
	docker run -v "${PWD}:/workdir" -w /workdir -t scheme-library-test-${SCHEME} \
		sh -c "make SCHEME=${SCHEME} SNOW_CHIBI_ARGS=--always-yes build install test; chmod -R 755 ${TMPDIR}"

clean:
	find . -name "README.html" -delete
	find . -name "*.log" -delete
	rm -rf ${TMPDIR}
	rm -rf *.tgz

clean-all:
	rm -rf tmp
