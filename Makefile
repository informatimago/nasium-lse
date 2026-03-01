# -*- mode:makefile-gmake;coding:utf-8 -*-
# Top-level Makefile for NASIUM-LSE.
#
# See doc/installation.txt for installation instructions.
# See src/README for compilation instructions and current status.

SRC_DIR := src
MAKE_SRC := $(MAKE) -C $(SRC_DIR)

.PHONY: help all cli cl-cli server documentation html-doc macosx install clean variables test fuzz

help:
	@printf "Targets: all cli cl-cli server documentation html-doc macosx test clean variables install fuzz\n"

all:
	$(MAKE_SRC) all

cli:
	$(MAKE_SRC) cli

cl-cli:
	$(MAKE_SRC) cl-cli

server:
	$(MAKE_SRC) server

documentation: html-doc
html-doc:
	$(MAKE_SRC) html-doc

macosx:
	$(MAKE_SRC) macosx

install:
	$(MAKE_SRC) install

variables:
	$(MAKE_SRC) variables

clean:
	$(MAKE_SRC) clean

fuzz:
	$(MAKE_SRC) fuzz

test:
	$(MAKE_SRC) test
