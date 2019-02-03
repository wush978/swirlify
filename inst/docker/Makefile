DOCKERFILE_IN = $(shell find . -name Dockerfile.in | sort)
DOCKERFILE = $(DOCKERFILE_IN:.in=)

all : $(DOCKERFILE)

%/Dockerfile : %/Dockerfile.in install.docker useradd.docker
	cat $< > $@
	cat install.docker >> $@
	cat useradd.docker >> $@
