PYTHON=python

help:
	@echo 'Commonly used make targets:'
	@echo '  tests              - run all tests in the automatic test suite'
	@echo '  all-version-tests - run all tests against many hg versions'
	@echo '  tests-%s           - run all tests in the specified hg version'

all: help

.PHONY: tests

tests:
	@echo "Path to crew repo is $(CREW) - set this with CREW= if needed."
	cd tests && $(PYTHON) $(CREW)/tests/run-tests.py $(TESTFLAGS)

test-%:
	@echo "Path to crew repo is $(CREW) - set this with CREW= if needed."
	cd tests && $(PYTHON) $(CREW)/tests/run-tests.py $(TESTFLAGS) $@

tests-%:
	@echo "Path to crew repo is $(CREW) - set this with CREW= if needed."
	hg -R $(CREW) checkout $$(echo $@ | sed s/tests-//) && \
	(cd $(CREW) ; $(MAKE) clean ) && \
	cd tests && $(PYTHON) $(CREW)/tests/run-tests.py $(TESTFLAGS)

all-version-tests: tests-1.7.5 tests-1.8.1 tests-1.9.3 tests-2.0.2 tests-2.9 \
                   tests-3.1.1 tests-3.2 tests-3.3 tests-3.4 tests-3.5 \
                   tests-3.6 tests-3.7 tests-3.8 tests-3.9 tests-4.0 \
                   tests-4.1 tests-4.2 tests-@
