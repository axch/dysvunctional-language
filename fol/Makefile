ARCH = $(shell uname -m)

ifeq ($(ARCH), x86_64)
  HEAP = 100000
else
  HEAP = 6000
endif

test:
	mit-scheme --compiler --heap $(HEAP) --stack 2000 --batch-mode --no-init-file \
	  --eval '(set! load/suppress-loading-message? #t)' \
	  --eval '(begin (load "load") (load "test/load") ((access run-tests-and-exit fol-environment)))'

.PHONY: test
