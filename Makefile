# Copyright (C) 2007-2015 Nicolas Lamirault <nicolas.lamirault@gmail.com>
#
# cletris users are granted the rights to distribute and use this software
# as governed by the terms of the MIT License :
# http://www.opensource.org/licenses/mit-license.php

APP = cletris

SHELL = /bin/bash

VERSION=$(shell \
        grep "version" cletris.asd \
	|awk -F'"' '{print $$2}')


NO_COLOR=\033[0m
OK_COLOR=\033[32;01m
ERROR_COLOR=\033[31;01m
WARN_COLOR=\033[33;01m

ROSWELL_HOME=$(HOME)/.roswell
ROSWELL_BRANCH = master


all: help

help:
	@echo -e "$(OK_COLOR)==== $(APP) [$(VERSION)] ====$(NO_COLOR)"
	@echo -e "$(WARN_COLOR)- init$(NO_COLOR)   : install tools$(NO_COLOR)"
	@echo -e "$(WARN_COLOR)- deps$(NO_COLOR)   : install dependencies$(NO_COLOR)"
	@echo -e "$(WARN_COLOR)- test$(NO_COLOR)   : launch unit tests$(NO_COLOR)"
	@echo -e "$(WARN_COLOR)- binary$(NO_COLOR) : build executable$(NO_COLOR)"

.PHONY: init
init:
	@echo -e "$(OK_COLOR)[climon] Install dependencies$(NO_COLOR)"
	@curl -L https://raw.githubusercontent.com/snmsts/roswell/$(ROSWELL_BRANCH)/scripts/install-for-ci.sh | sh
	@ros install qlot

.PHONY: deps
deps:
	@qlot install :climon
	@ln -sf `pwd`/*.asd  quicklisp/local-projects/

.PHONY: test
test:
	@echo -e "$(OK_COLOR)[climon] Launch unit tests$(NO_COLOR)"
	@qlot exec run-prove climon-test.asd

.PHONY: binary
binary:
	@echo -e "$(OK_COLOR)[climon] Build binary$(NO_COLOR)"
	@ros build roswell/climon.ros
