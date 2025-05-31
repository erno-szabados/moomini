SRC_DIR = src
TEST_DIR = test
BIN_DIR = bin
GM2FLAGS = -fiso

GM2 = gm2-14

all: test

$(BIN_DIR)/Utf8.o: $(SRC_DIR)/Utf8.mod $(SRC_DIR)/Utf8.def | $(BIN_DIR)
	$(GM2) $(GM2FLAGS) -I$(SRC_DIR) -c $(SRC_DIR)/Utf8.mod -o $(BIN_DIR)/Utf8.o

$(BIN_DIR)/TestUtf8: $(BIN_DIR)/Utf8.o | $(BIN_DIR)
	$(GM2) $(GM2FLAGS) -I$(SRC_DIR) $(TEST_DIR)/TestUtf8.mod $(BIN_DIR)/Utf8.o -o $(BIN_DIR)/TestUtf8

$(BIN_DIR)/TestUtf8FileWrite: $(BIN_DIR)/Utf8.o | $(BIN_DIR)
	$(GM2) $(GM2FLAGS) -I$(SRC_DIR) $(TEST_DIR)/TestUtf8FileWrite.mod $(BIN_DIR)/Utf8.o -o $(BIN_DIR)/TestUtf8FileWrite

test: testUtf8 testUtf8FileWrite

testUtf8: $(BIN_DIR)/TestUtf8
	$(BIN_DIR)/TestUtf8

testUtf8FileWrite: $(BIN_DIR)/TestUtf8FileWrite
	$(BIN_DIR)/TestUtf8FileWrite

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

clean:
	rm -rf $(BIN_DIR)
	rm -f *.o

.PHONY: all clean test