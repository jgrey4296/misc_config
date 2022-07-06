SHELL		:= /usr/local/bin/bash
LOGLEVEL	:= WARNING

RUST_TOP	:= ./rust_src
PY_TOP		:= {{cookiecutter.proj_name}}
BUILD       := ./build

# Testing variables:
TEST_TARGET		?= ${PY_TOP}
TEST_PAT		:=
TESTDIRS        :=

# Clean variables:
LOGS			!= find ${PY_TOP} -name '*log.*'

# Documentation variables:
doc_target		?= "html"
SPHINXOPTS		?=
SPHINXBUILD		?= sphinx-build
DOCSOURCEDIR    = docs
DOCBUILDDIR     = dist/docs

# Extracts the name of the produced rust binary
NAME != awk 'BEGIN {FS=" "} /name/ {print $$3; exit}' Cargo.toml

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
rust_debug:
	cargo build
	cp ${BUILD}/debug/lib${NAME}.dylib ${BUILD}/${NAME}.so

rust_release:
	cargo build --release
	cp ${BUILD}/release/lib${NAME}.dylib ${BUILD}/${NAME}.so

editlib: rust_debug
	pip install -e .

install: rust_release
	pip install --use-feature=in-tree-build --src ${BUILD}/pip_temp -U .

wheel: rust_release
	pip wheel --use-feature=in-tree-build -w ${BUILD}/wheel --use-pep517 --src ${BUILD}/pip_temp .

srcbuild: rust_debug
	pip install --use-feature=in-tree-build -t ${BUILD}/pip_src --src ${BUILD}/pip_temp -U .

uninstall:
	pip uninstall -y {{cookiecutter.proj_name}}

requirements:
	pip freeze --all --exclude-editable -r requirements.txt > requirements.txt

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
	@echo "Target: ${PY_TOP}/$@"
	@echo "TODO: Dtest"
	@echo "Target: ${PY_TOP}/$@"

# Cleaning ####################################################################
clean:
	@echo "Cleaning"
ifeq (${LOGS}, )
	@echo "No Logs to delete"
else
	-rm ${LOGS}
endif
	-rm -rf ${BUILD}
