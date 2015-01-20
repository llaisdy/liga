all:
	mkdir -p ebin
	erlc -o ebin src/liga.erl
	erlc -o ebin src/liga_writer.erl
	cp src/liga.app.src ebin/liga.app

calibration: all
	erlc -o ebin calibration/calibration.erl
	erlc -o ebin calibration/data_server.erl

clean:
	rm -fr ebin

test: all
	erlc -o ebin test/liga_test_utils.erl
	erlc -o ebin test/liga_tests.erl
	erlc -o ebin test/liga_writer_tests.erl

