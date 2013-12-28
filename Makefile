#!/usr/bin/make -f

PACKAGE_NAME := rudel-cursor-sharing
PACKAGE_VERSION := 1.0.0alpha1
TAR := gtar

all: elpa

elpa: $(PACKAGE_NAME)-$(PACKAGE_VERSION).tar

$(PACKAGE_NAME)-$(PACKAGE_VERSION).tar: rudel-cursor-sharing.el rudel-obby-cursor.el rudel-operation-hooks.el $(PACKAGE_NAME)-pkg.el
	$(TAR) --transform='s::$(PACKAGE_NAME)-$(PACKAGE_VERSION)/:' \
	    -cf $@ $^

clean:
	rm $(PACKAGE_NAME)-*.tar

.PHONY: all clean elpa
