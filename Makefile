all:
	mkdir -p ebin
	erlc -o ebin src/liga.erl
	cp src/liga.app.src ebin/liga.app

clean:
	rm -fr ebin

test: all
	erlc -o ebin test/liga_tests.erl

