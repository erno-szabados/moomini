SRC_DIR = src
TEST_DIR = test
BIN_DIR = bin
GM2FLAGS = -fiso

GM2 = gm2-14

SRC_FILES = $(SRC_DIR)/UTF8.mod
DEF_FILES = $(SRC_DIR)/UTF8.def

all: test

$(BIN_DIR)/UTF8.o: $(SRC_DIR)/UTF8.mod $(SRC_DIR)/UTF8.def | $(BIN_DIR)
	$(GM2) $(GM2FLAGS) -I$(SRC_DIR) -c $(SRC_DIR)/UTF8.mod -o $(BIN_DIR)/UTF8.o

$(BIN_DIR)/TestUTF8: $(BIN_DIR)/UTF8.o $(TEST_FILES) | $(BIN_DIR)
	$(GM2) $(GM2FLAGS) -I$(SRC_DIR) $(TEST_DIR)/TestUTF8.mod $(BIN_DIR)/UTF8.o -o $(BIN_DIR)/TestUTF8

$(BIN_DIR)/TestUTF8FileWrite: $(BIN_DIR)/UTF8.o $(TEST_FILES) | $(BIN_DIR)
	$(GM2) $(GM2FLAGS) -I$(SRC_DIR) $(TEST_DIR)/TestUTF8FileWrite.mod $(BIN_DIR)/UTF8.o -o $(BIN_DIR)/TestUTF8FileWrite

test: testUTF8 testUTF8FileWrite

testUTF8: $(BIN_DIR)/TestUTF8
	$(BIN_DIR)/TestUTF8

testUTF8FileWrite: $(BIN_DIR)/TestUTF8FileWrite
	$(BIN_DIR)/TestUTF8FileWrite

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

clean:
	rm -rf $(BIN_DIR)
	rm *.o

.PHONY: all clean test