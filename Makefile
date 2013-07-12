clean:
	@find . -regextype posix-extended -regex ".*\.(bci|bin|com|ext)$$" -delete

test:
	make -C fol test
	make -C vl test
	make -C dvl test

.PHONY: clean
