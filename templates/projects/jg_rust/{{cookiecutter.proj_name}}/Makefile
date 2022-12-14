SHELL			:= /usr/local/bin/bash
LOGLEVEL		:= WARNING

TOP				:= ./src
MAIN            := {{cookiecutter.entry_point}}
BUILD           := ./target

# Testing variables:
TEST_TARGET		?= ${TOP}
TEST_PAT		:=
TESTDIRS        :=

# Clean variables:
LOGS			!= find ${TOP} -name '*log.*'

# Documentation variables:
doc_target		?= "html"
SPHINXOPTS		?=
SPHINXBUILD		?= sphinx-build
DOCSOURCEDIR    = docs
DOCBUILDDIR     = dist/docs


# If defined, use these overrides
ifneq (${pat}, )
	TEST_PAT = -k ${pat}
endif

ifneq (${fpat}, )
	TEST_FILE_PAT := ${fpat}
endif


.PHONY: help Makefile all lint clean browse test dtest

all: verbose

# Documentation ###############################################################
# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(DOCSOURCEDIR)" "$(DOCBUILDDIR)" $(SPHINXOPTS) $(O)
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
# run with `make sphinx doc_target=html` =clean etc

sphinx: Makefile
	@$(SPHINXBUILD) -M ${doc_target} "$(DOCSOURCEDIR)" "$(DOCBUILDDIR)" $(SPHINXOPTS) $(O)

browse:
	open "$(DOCBUILDDIR)/html/index.html"

docs: sphinx browse
	cargo doc


rusthelp:
	python -mwebbrowser "https://doc.rust-lang.org/book/title-page.html"

# Building ####################################################################
debug:
	cargo build

release:
	cargo build --release

run:
	cargo run

# Linting #####################################################################
lint:
	@echo "TODO: Linting"
	cargo check

# Testing #####################################################################
test:
	@echo "TODO: Testing"
	cargo test

dtest: ${TESTDIRS}
	@echo "Tested: "
	@for entry in ${TESTDIRS}; do echo $$entry ; done

$(TESTDIRS):
	@echo "--------------------"
	@echo "Target: ${TOP}/$@"
	@echo "TODO: Dtest"
	@echo "Target: ${TOP}/$@"

# Cleaning ####################################################################
clean:
	@echo "Cleaning"
ifeq (${LOGS}, )
	@echo "No Logs to delete"
else
	-rm ${LOGS}
endif
	-rm -rf ./dist
	-rm -rf ${BUILD}
