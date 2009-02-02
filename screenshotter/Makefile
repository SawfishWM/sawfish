all:

clean:
	rm -f *~

distclean: clean

dist: clean
	rm -rf screenshotter screenshotter.tar.gz
	rm -f .*.sw?
	mkdir screenshotter
	find . -maxdepth 1 -type f -print0 | xargs -i -0 cp '{}' screenshotter
	tar cfvz screenshotter.tar.gz screenshotter

