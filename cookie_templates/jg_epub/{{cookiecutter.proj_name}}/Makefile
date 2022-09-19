TITLE={{cookiecutter.proj_name}}

all: clean build

build:
	zip ./build/src -r ./src
	ebook-convert ./build/src.zip ./dist/$TITLE.zip

clean:
	-rm -r ./build
	-rm -r ./dist
	mkdir build
	mkdir dist
