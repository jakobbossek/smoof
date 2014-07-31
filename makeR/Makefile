R	:= R --no-save --no-restore
RSCRIPT	:= Rscript
DELETE	:= rm -fR
PKGNAME := $(shell Rscript ./makeR/get-pkg-name)
VERSION := $(shell Rscript ./makeR/get-pkg-version)
TARGZ   := $(PKGNAME)_$(VERSION).tar.gz

.SILENT:
.PHONEY: clean roxygenize package windows install dependencies test check

usage:
	echo "Available targets:"
	echo ""
	echo " clean          - Clean everything up"
	echo " roxygenize     - roxygenize in-place"
	echo " package        - build source package"
	echo " install        - install the package"
	echo " dependencies   - install package dependencies, including suggests"
	echo " test           - run unit tests"
	echo " check          - run R CMD check on the package"
	echo " check-rev-dep  - run a reverse dependency check against packages on CRAN"
	echo " check-rd-files - run Rd2pdf on each doc file to track hard-to-spot doc/latex errors"
	echo " htmlhelp       - build static html documentation"
	echo " winbuilder     - ask for email and build on winbuilder"

clean:
	echo  "Cleaning up ..."
	${DELETE} src/*.o src/*.so *.tar.gz
	${DELETE} html
	${DELETE} staticdocs
	${DELETE} *.Rcheck
	${DELETE} .RData .Rhistory

roxygenize: clean
	echo "Roxygenizing package ..."
	${RSCRIPT} ./makeR/roxygenize

package: roxygenize
	echo "Building package file $(TARGZ)"
	${R} CMD build .

install: package
	echo "Installing package $(TARGZ)"
	${R} CMD INSTALL --install-tests $(TARGZ)

test: install
	echo "Testing package $(TARGZ)"
	${RSCRIPT} ./test_all.R

check: package
	echo "Running R CMD check ..."
	${R} CMD check $(TARGZ)

dependecies:
	${RSCRIPT} ./makeR/dependencies

check-rev-dep: install
	echo "Running reverse dependency checks for CRAN ..."
	${RSCRIPT} ./makeR/check-rev-dep

htmlhelp: install
	echo "Generating html docs..."
	mkdir staticdocs
	${DELETE} /tmp/pkgdocs
	mkdir /tmp/pkgdocs
	mv README.md README.xxx
	${RSCRIPT} ./makeR/generate-html-docs
	mv README.xxx README.md
	${DELETE} Rplots*.pdf
	git checkout gh-pages
	git pull
	${DELETE} man
	mv /tmp/pkgdocs man
	git add man
	git commit -am "new html help"
	git push origin gh-pages
	git checkout master

winbuilder: roxygenize
	echo "Building via winbuilder"
	${RSCRIPT} ./makeR/winbuilder

